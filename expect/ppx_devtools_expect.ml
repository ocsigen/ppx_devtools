(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
 * Copyright (C) 2019 Hugo Heuzard
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

(** A small, self-contained reimplementation of the [ppx_expect] rewriter,
    paired with {!Ppx_devtools_expect_runtime}.

    It expands:
    - [let%expect_test "description" = body] (optionally [_] for no
      description, optionally carrying a [\[@tags "a", "b"\]] attribute) into a
      call to [Ppx_devtools_expect_runtime.register_test].
    - [%expect {| ... |}] / [%expect] assertion points into
      [Ppx_devtools_expect_runtime.check ~index]. Consecutive assertion points are
      coalesced into a single group of variants checked against the output
      captured since the previous group.
    - [%expect {| ... |} \[@when COND\]] conditional variants. [COND] is a
      predicate in the {!Ppx_devtools_predicate.Predicate} grammar; it is reified
      into a runtime test against {!Ppx_devtools_expect_runtime.Axes}. Further axes
      can be added with {!register_axes}. A plain
      [%expect] (no [\[@when\]]) is the default variant of its group.
    - [%expect.unreachable] into an assertion point that fails when reached.
    - [%expect.output] into [Ppx_devtools_expect_runtime.read_output ()], returning
      the output captured since the previous assertion point. *)

open StdLabels
open Ppxlib
module Predicate = Ppx_devtools_predicate.Predicate

let runtime = "Ppx_devtools_expect_runtime"

(* One variant of an assertion point, gathered from a single [%expect ...]. *)
type variant =
  { is_default : bool
  ; condition : expression option (* reified [@when] predicate, [bool]-valued *)
  ; expected : string
  ; line : int
  ; expect_start : int
  ; expect_end : int
  ; payload_start : int
  ; payload_end : int
  ; in_sequence : bool (* a statement of a sequence, not a lone expression *)
  ; is_unreachable : bool (* [%expect.unreachable] *)
  }

let invalid ~loc fmt = Location.raise_errorf ~loc fmt

(* Resolvers of the identifiers of [@when] predicates, most recent first. The
   predefined axes come last. *)
let resolvers =
  ref
    [ (fun name ->
        match name with
        | "backend"
        | "js"
        | "wasm"
        | "native"
        | "os_type"
        | "win32"
        | "unix"
        | "cygwin"
        | "ocaml_version"
        | "oxcaml" -> Some ("Ppx_devtools_expect_runtime.Axes." ^ name)
        | _ -> None)
    ]

let register_axes f = resolvers := f :: !resolvers

let resolve name = List.find_map !resolvers ~f:(fun f -> f name)

(* Pull the optional [@when COND] off the literal (or off the [Pstr_eval] item)
   and reify it into a [bool] expression evaluated against the runtime axes. *)
let when_condition ~expr_attrs ~eval_attrs =
  let find attrs =
    List.find_opt attrs ~f:(fun (a : attribute) -> String.equal a.attr_name.txt "when")
  in
  match
    match find expr_attrs with
    | Some _ as a -> a
    | None -> find eval_attrs
  with
  | None -> None
  | Some attr ->
      Attribute.mark_as_handled_manually attr;
      let loc = attr.attr_loc in
      let cond =
        match attr.attr_payload with
        | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] -> e
        | _ -> invalid ~loc "[@when] expects a single predicate expression"
      in
      let t =
        try Predicate.parse cond
        with Predicate.Invalid loc -> invalid ~loc "invalid [@when] predicate"
      in
      let e =
        try Predicate.reify ~resolve ~loc:cond.pexp_loc t
        with Predicate.Invalid loc ->
          invalid
            ~loc
            "[@when] predicate uses an unknown axis or is not meaningful at runtime"
      in
      Some e

(* Parse the payload of an [%expect ...] node into a {!variant}. *)
let variant_of_payload ~ext_loc (payload : payload) : variant =
  let line = ext_loc.loc_start.pos_lnum in
  let expect_start = ext_loc.loc_start.pos_cnum in
  let expect_end = ext_loc.loc_end.pos_cnum in
  match payload with
  | PStr [] ->
      (* Bare [%expect]: no literal yet. Promotion will insert one just before
         the closing [\]] of the extension. *)
      let p = ext_loc.loc_end.pos_cnum - 1 in
      { is_default = true
      ; condition = None
      ; expected = ""
      ; line
      ; expect_start
      ; expect_end
      ; payload_start = p
      ; payload_end = p
      ; in_sequence = true
      ; is_unreachable = false
      }
  | PStr [ { pstr_desc = Pstr_eval (lit, eval_attrs); _ } ] -> (
      match lit.pexp_desc with
      | Pexp_constant (Pconst_string (s, str_loc, delim)) ->
          let condition = when_condition ~expr_attrs:lit.pexp_attributes ~eval_attrs in
          (* [lit.pexp_loc] spans the parentheses and the attribute of
             [({| ... |} [@when COND])], so promoting over it would drop the
             guard. [str_loc] covers the contents of the literal only: extend
             it over the delimiters. *)
          let delim_len =
            match delim with
            | None -> 1
            | Some d -> String.length d + 2
          in
          { is_default = Option.is_none condition
          ; condition
          ; expected = s
          ; line
          ; expect_start
          ; expect_end
          ; payload_start = str_loc.loc_start.pos_cnum - delim_len
          ; payload_end = str_loc.loc_end.pos_cnum + delim_len
          ; in_sequence = true
          ; is_unreachable = false
          }
      | _ -> invalid ~loc:lit.pexp_loc "[%%expect] expects a string literal payload")
  | _ -> invalid ~loc:ext_loc "[%%expect] expects a string literal payload"

type point =
  | Expect of variant
  | Output
  | Other

let classify (e : expression) : point =
  match e.pexp_desc with
  | Pexp_extension ({ txt = "expect"; _ }, payload) ->
      Expect (variant_of_payload ~ext_loc:e.pexp_loc payload)
  | Pexp_extension ({ txt = "expect.unreachable"; loc }, payload) -> (
      match payload with
      | PStr [] ->
          let v = variant_of_payload ~ext_loc:e.pexp_loc payload in
          Expect { v with is_unreachable = true }
      | _ -> invalid ~loc "[%%expect.unreachable] does not take a payload")
  | Pexp_extension ({ txt = "expect.output"; loc }, payload) -> (
      match payload with
      | PStr [] -> Output
      | _ -> invalid ~loc "[%%expect.output] does not take a payload")
  | _ -> Other

let rec flatten_seq (e : expression) =
  match e.pexp_desc with
  | Pexp_sequence (a, b) -> a :: flatten_seq b
  | _ -> [ e ]

let build_seq ~loc = function
  | [] -> [%expr ()]
  | l ->
      let rec go = function
        | [] -> [%expr ()]
        | [ x ] -> x
        | x :: tl ->
            { (Ast_builder.Default.esequence ~loc [ x; go tl ]) with pexp_loc = loc }
      in
      go l

(* Rewrite a test body, allocating a group per run of adjacent assertion points
   and replacing each [%expect ...]/[%expect.output] with the matching runtime
   call. Returns the rewritten body and the groups in index order. *)
let transform_body (body : expression) : expression * variant list array =
  let groups = ref [] in
  let count = ref 0 in
  let alloc_group variants =
    let idx = !count in
    incr count;
    groups := variants :: !groups;
    idx
  in
  let check ~loc idx =
    [%expr
      [%e Ast_builder.Default.evar ~loc (runtime ^ ".check")]
        ~index:[%e Ast_builder.Default.eint ~loc idx]]
  in
  let mapper =
    object (self)
      inherit Ast_traverse.map as super

      method! expression e =
        match e.pexp_desc with
        | Pexp_sequence _ -> self#sequence e
        | _ -> (
            match classify e with
            | Expect v ->
                let loc = e.pexp_loc in
                check ~loc (alloc_group [ { v with in_sequence = false } ])
            | Output ->
                let loc = e.pexp_loc in
                [%expr [%e Ast_builder.Default.evar ~loc (runtime ^ ".read_output")] ()]
            | Other -> super#expression e)

      method private sequence e =
        let loc = e.pexp_loc in
        let stmts = flatten_seq e in
        let rec go = function
          | [] -> []
          | stmt :: rest -> (
              match classify stmt with
              | Expect v ->
                  let run, rest =
                    if v.is_unreachable then [ v ], rest else gather_run [ v ] rest
                  in
                  check ~loc:stmt.pexp_loc (alloc_group run) :: go rest
              | Output | Other -> self#expression stmt :: go rest)
        and gather_run acc = function
          | stmt :: rest -> (
              match classify stmt with
              | Expect v when not v.is_unreachable -> gather_run (v :: acc) rest
              | Expect _ | Output | Other -> List.rev acc, stmt :: rest)
          | [] -> List.rev acc, []
        in
        build_seq ~loc (go stmts)
    end
  in
  let body = mapper#expression body in
  body, Array.of_list (List.rev !groups)

(* -- Code generation for [register_test] ----------------------------------- *)

let estring ~loc s = Ast_builder.Default.estring ~loc s

let evariant ~loc (v : variant) : expression =
  let condition =
    match v.condition with
    | Some c -> [%expr fun () -> [%e c]]
    | None -> [%expr fun () -> false]
  in
  [%expr
    { Ppx_devtools_expect_runtime.condition = [%e condition]
    ; is_default = [%e Ast_builder.Default.ebool ~loc v.is_default]
    ; expected = [%e estring ~loc v.expected]
    ; line = [%e Ast_builder.Default.eint ~loc v.line]
    ; expect_start = [%e Ast_builder.Default.eint ~loc v.expect_start]
    ; expect_end = [%e Ast_builder.Default.eint ~loc v.expect_end]
    ; payload_start = [%e Ast_builder.Default.eint ~loc v.payload_start]
    ; payload_end = [%e Ast_builder.Default.eint ~loc v.payload_end]
    ; in_sequence = [%e Ast_builder.Default.ebool ~loc v.in_sequence]
    ; is_unreachable = [%e Ast_builder.Default.ebool ~loc v.is_unreachable]
    }]

let eexpectations ~loc (groups : variant list array) : expression =
  Ast_builder.Default.pexp_array
    ~loc
    (Array.to_list
       (Array.map groups ~f:(fun variants ->
            Ast_builder.Default.pexp_array ~loc (List.map variants ~f:(evariant ~loc)))))

(* [@tags "a", "b"] (or [@tags "a"]) on the test description. *)
let tags_of_attributes (attrs : attribute list) : string list =
  match
    List.find_opt attrs ~f:(fun (a : attribute) -> String.equal a.attr_name.txt "tags")
  with
  | None -> []
  | Some attr -> (
      Attribute.mark_as_handled_manually attr;
      let loc = attr.attr_loc in
      let one (e : expression) =
        match e.pexp_desc with
        | Pexp_constant (Pconst_string (s, _, _)) -> s
        | _ -> invalid ~loc "[@tags] expects string literals"
      in
      match attr.attr_payload with
      | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] -> (
          (* A tuple [@tags "a", "b"] is read via [Ast_pattern.pexp_tuple]:
             OxCaml's ppxlib changes the raw [Pexp_tuple] payload, but the
             pattern exposes a portable [expression list]. *)
          match
            Ast_pattern.parse_res Ast_pattern.(pexp_tuple __) e.pexp_loc e (fun l -> l)
          with
          | Ok l -> List.map l ~f:one
          | Error _ -> [ one e ])
      | _ -> invalid ~loc "[@tags] expects string literals")

(* [@when COND] on the test description gates whether the test runs at all,
   using the same predicate grammar as [%expect ... [@when]] (a runtime check,
   so it can mention backend/engine which [@@if] cannot). *)
let when_of_attributes (attrs : attribute list) : expression option =
  match
    List.find_opt attrs ~f:(fun (a : attribute) -> String.equal a.attr_name.txt "when")
  with
  | None -> None
  | Some attr ->
      Attribute.mark_as_handled_manually attr;
      let loc = attr.attr_loc in
      let cond =
        match attr.attr_payload with
        | PStr [ { pstr_desc = Pstr_eval (e, _); _ } ] -> e
        | _ -> invalid ~loc "[@when] expects a single predicate expression"
      in
      let t =
        try Predicate.parse cond
        with Predicate.Invalid loc -> invalid ~loc "invalid [@when] predicate"
      in
      Some
        (try Predicate.reify ~resolve ~loc:cond.pexp_loc t
         with Predicate.Invalid loc ->
           invalid
             ~loc
             "[@when] predicate uses an unknown axis or is not meaningful at runtime")

(* dune passes the library name being preprocessed as the [library-name]
   ppxlib cookie; record it so each test knows which library it belongs to. *)
let library_name = ref ""

let () =
  Driver.Cookies.add_simple_handler
    "library-name"
    Ast_pattern.(estring __)
    ~f:(function
      | Some name -> library_name := name
      | None -> ())

(* [@@expect.uncaught_exn {| ... |}] on the test: the exception it is expected
   to raise, as in ppx_expect. Its span starts at [body_end] so that deleting
   it also deletes the whitespace in front of it. *)
let uncaught_exn_of_attributes ~loc ~body_end (attrs : attribute list) : expression =
  match
    List.find_opt attrs ~f:(fun (a : attribute) ->
        String.equal a.attr_name.txt "expect.uncaught_exn")
  with
  | None -> [%expr None]
  | Some attr -> (
      Attribute.mark_as_handled_manually attr;
      match attr.attr_payload with
      | PStr
          [ { pstr_desc =
                Pstr_eval
                  ({ pexp_desc = Pexp_constant (Pconst_string (s, str_loc, delim)); _ }, _)
            ; _
            }
          ] ->
          let delim_len =
            match delim with
            | None -> 1
            | Some d -> String.length d + 2
          in
          let eint = Ast_builder.Default.eint ~loc in
          [%expr
            Some
              { Ppx_devtools_expect_runtime.expected_exn = [%e estring ~loc s]
              ; exn_payload_start = [%e eint (str_loc.loc_start.pos_cnum - delim_len)]
              ; exn_payload_end = [%e eint (str_loc.loc_end.pos_cnum + delim_len)]
              ; attribute_start = [%e eint body_end]
              ; attribute_end = [%e eint attr.attr_loc.loc_end.pos_cnum]
              }]
      | _ -> invalid ~loc:attr.attr_loc "[@@expect.uncaught_exn] expects a string literal"
      )

let expand_test ~loc ~item_attributes (vb : value_binding) : structure_item =
  let description, tags =
    match vb.pvb_pat.ppat_desc with
    | Ppat_constant (Pconst_string (s, _, _)) ->
        s, tags_of_attributes vb.pvb_pat.ppat_attributes
    | Ppat_any -> "", tags_of_attributes vb.pvb_pat.ppat_attributes
    | _ -> invalid ~loc:vb.pvb_pat.ppat_loc "let%%expect_test expects a string or _ name"
  in
  let pos = vb.pvb_loc.loc_start in
  let filename = pos.pos_fname in
  let line_number = pos.pos_lnum in
  (* Character offsets of the test on its starting line, like ppx_inline_test,
     so the [-verbose] location reads identically. *)
  let start_pos = pos.pos_cnum - pos.pos_bol in
  let end_pos = vb.pvb_loc.loc_end.pos_cnum - pos.pos_bol in
  let body_end = vb.pvb_expr.pexp_loc.loc_end.pos_cnum in
  let last_statement_start =
    (List.hd (List.rev (flatten_seq vb.pvb_expr))).pexp_loc.loc_start.pos_cnum
  in
  (* A [match] or [try] at the end of the body would capture a statement
     appended after it into its last case: promotion parenthesizes it first. *)
  let open_tail =
    let rec find (e : expression) =
      match e.pexp_desc with
      | Pexp_match _ | Pexp_try _ -> Some e.pexp_loc
      | Pexp_sequence (_, e)
      | Pexp_let (_, _, e)
      | Pexp_letmodule (_, _, e)
      | Pexp_letexception (_, e)
      | Pexp_open (_, e)
      | Pexp_letop { body = e; _ }
      | Pexp_ifthenelse (_, _, Some e)
      | Pexp_ifthenelse (_, e, None) -> find e
      | _ -> None
    in
    match find vb.pvb_expr with
    | None -> [%expr None]
    | Some l ->
        [%expr
          Some
            ( [%e Ast_builder.Default.eint ~loc l.loc_start.pos_cnum]
            , [%e Ast_builder.Default.eint ~loc l.loc_end.pos_cnum] )]
  in
  let uncaught_exn =
    uncaught_exn_of_attributes ~loc ~body_end (vb.pvb_attributes @ item_attributes)
  in
  let body, groups = transform_body vb.pvb_expr in
  let expectations = eexpectations ~loc groups in
  let tags = Ast_builder.Default.elist ~loc (List.map tags ~f:(estring ~loc)) in
  let condition =
    match when_of_attributes vb.pvb_pat.ppat_attributes with
    | Some c -> [%expr fun () -> [%e c]]
    | None -> [%expr fun () -> true]
  in
  [%stri
    let () =
      Ppx_devtools_expect_runtime.register_test
        ~lib:[%e estring ~loc !library_name]
        ~filename:[%e estring ~loc filename]
        ~description:[%e estring ~loc description]
        ~line_number:[%e Ast_builder.Default.eint ~loc line_number]
        ~start_pos:[%e Ast_builder.Default.eint ~loc start_pos]
        ~end_pos:[%e Ast_builder.Default.eint ~loc end_pos]
        ~tags:[%e tags]
        ~condition:[%e condition]
        ~expectations:[%e expectations]
        ~body_end:[%e Ast_builder.Default.eint ~loc body_end]
        ~last_statement_start:[%e Ast_builder.Default.eint ~loc last_statement_start]
        ~open_tail:[%e open_tail]
        ~uncaught_exn:[%e uncaught_exn]
        (fun () -> [%e body])]

let impl_mapper =
  object
    inherit Ast_traverse.map as super

    method! structure_item item =
      match item.pstr_desc with
      | Pstr_extension (({ txt = "expect_test"; _ }, payload), item_attributes) -> (
          match payload with
          | PStr [ { pstr_desc = Pstr_value (Nonrecursive, [ vb ]); _ } ] ->
              expand_test ~loc:item.pstr_loc ~item_attributes vb
          | _ -> invalid ~loc:item.pstr_loc "let%%expect_test expects a single binding")
      | _ -> super#structure_item item
  end

let () =
  Driver.register_transformation "ppx_devtools-expect" ~impl:(fun str ->
      impl_mapper#structure str)
