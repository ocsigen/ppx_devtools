(* Js_of_ocaml compiler
 * http://www.ocsigen.org/js_of_ocaml/
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
 *)

(* Produces the [<source>.corrected] files that dune diffs against the sources
   for promotion. Only the literal of the variant that actually mismatched is
   rewritten, or a new [%expect] inserted; every other byte of the source
   (including the non-matching [%expect.when] variants) is copied verbatim. *)

type kind =
  (* Replace the literal in [payload_start, payload_end). A bare [%expect]
     has an empty span, and gets a literal inserted. *)
  | Literal
  (* Replace the literal of an [[@@expect.uncaught_exn]] attribute in
     [payload_start, payload_end), or insert a new attribute there when the
     span is empty. *)
  | Uncaught_exn
  (* Replace [payload_start, payload_end) (a whole [%expect] node) with a
     new [%expect] holding [actual]. *)
  | Expect_node
  (* Replace [payload_start, payload_end) with the given text. *)
  | Replace of string
  (* Insert a new [%expect] at [payload_start] (= [payload_end]), indented
     like the line holding [indent_pos]. *)
  | New_expect of
      { indent_pos : int
      ; parens : parens
      }

(* Parentheses needed for the [; [%expect ...]] appended by [New_expect] to be
   a statement following the expression before it. *)
and parens =
  | No_parens
  (* Parenthesize [\[start, stop)] (a [match] or [try] that would otherwise
     extend over the new statement), unless it already is. *)
  | Before of
      { start : int
      ; stop : int
      }
  (* Parenthesize from [start] through the new [%expect] (a lone expression,
     not a statement of a sequence). *)
  | Around of { start : int }

type correction =
  { filename : string (* relative to the build context root (see [write]) *)
  ; payload_start : int (* byte offset of the start of the string literal token *)
  ; payload_end : int (* byte offset just past the literal token *)
  ; actual : string
  ; kind : kind
  }

let contains s sub =
  let n = String.length s and m = String.length sub in
  let rec loop i =
    if i + m > n
    then false
    else if String.equal (String.sub s i m) sub
    then true
    else loop (i + 1)
  in
  loop 0

let column_of source pos =
  let rec line_start i =
    if i <= 0 || Char.equal source.[i - 1] '\n' then i else line_start (i - 1)
  in
  pos - line_start pos

(* [source] has an already parenthesized expression at [pos]. *)
let parenthesized source pos =
  let has s =
    pos + String.length s <= String.length source
    && String.equal (String.sub source pos (String.length s)) s
  in
  has "(" || has "begin " || has "begin\n"

let indentation_of source pos =
  let start = pos - column_of source pos in
  let len = String.length source in
  let rec loop i = if i < len && Char.equal source.[i] ' ' then loop (i + 1) else i in
  loop start - start

(* Pick the shortest [{tag| ... |tag}] delimiter that does not clash with the
   body. [tag = ""] gives the usual [{| ... |}]. *)
let rec choose_tag body n =
  let tag = String.make n '_' in
  if contains body ("|" ^ tag ^ "}") then choose_tag body (n + 1) else tag

let render_literal ~column ~actual =
  let lines = Text.canonical_lines actual in
  let body = String.concat "\n" lines in
  let tag = choose_tag body 0 in
  let opening = "{" ^ tag ^ "|" and closing = "|" ^ tag ^ "}" in
  match lines with
  | [] -> opening ^ " " ^ closing
  | [ single ] -> Printf.sprintf "%s %s %s" opening single closing
  | _ ->
      let indent = String.make column ' ' in
      let body =
        String.concat
          ""
          (List.map
             (fun l -> if String.equal l "" then "\n" else indent ^ l ^ "\n")
             lines)
      in
      Printf.sprintf "%s\n%s%s%s" opening body indent closing

let read_file path =
  try
    let ic = open_in_bin path in
    let s = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Some s
  with Sys_error _ -> None

let write_file path data =
  (try if Sys.file_exists path then Sys.remove path with Sys_error _ -> ());
  let oc = open_out_bin path in
  output_string oc data;
  close_out oc

(* [filename]s are relative to the build context root; [root] is the path of
   that root relative to the runner's cwd (dune's [-source-tree-root]). Both the
   source read and the [.corrected] write go through [root] so they land next to
   the in-build copy of the source that dune diffs for promotion. Returns the
   number of sources that could not be read: their mismatches would otherwise
   be lost. *)
let write ~root corrections =
  let unreadable = ref 0 in
  let tbl = Hashtbl.create 16 in
  List.iter
    (fun c ->
      let l = try Hashtbl.find tbl c.filename with Not_found -> [] in
      Hashtbl.replace tbl c.filename (c :: l))
    corrections;
  Hashtbl.iter
    (fun filename cs ->
      let path = Filename.concat root filename in
      match read_file path with
      | None ->
          incr unreadable;
          Printf.eprintf
            "ppx_devtools-expect: cannot read %s to write its corrected version\n%!"
            filename
      | Some source ->
          let edits =
            List.concat_map
              (fun c ->
                match c.kind with
                | Literal ->
                    let literal =
                      render_literal
                        ~column:(column_of source c.payload_start)
                        ~actual:c.actual
                    in
                    (* A bare [%expect] has no literal to replace (the span is
                       empty); the literal is inserted, so add the leading
                       space that a written-out [%expect {| ... |}] would
                       have. *)
                    let literal =
                      if c.payload_start = c.payload_end then " " ^ literal else literal
                    in
                    [ c.payload_start, c.payload_end, literal ]
                | Uncaught_exn ->
                    (* Laid out like ppx_expect does. *)
                    let literal = render_literal ~column:2 ~actual:c.actual in
                    let literal =
                      if c.payload_start = c.payload_end
                      then "\n[@@expect.uncaught_exn " ^ literal ^ "]"
                      else literal
                    in
                    [ c.payload_start, c.payload_end, literal ]
                | Expect_node ->
                    let opening = "[%expect " in
                    let literal =
                      render_literal
                        ~column:(column_of source c.payload_start + String.length opening)
                        ~actual:c.actual
                    in
                    [ c.payload_start, c.payload_end, opening ^ literal ^ "]" ]
                | Replace s -> [ c.payload_start, c.payload_end, s ]
                | New_expect { indent_pos; parens } -> (
                    let indent = indentation_of source indent_pos in
                    let opening = "[%expect " in
                    let literal =
                      render_literal
                        ~column:(indent + String.length opening)
                        ~actual:c.actual
                    in
                    let expect =
                      Printf.sprintf ";\n%s%s%s]" (String.make indent ' ') opening literal
                    in
                    let insert pos s = pos, pos, s in
                    (* Insertions at the same offset stay in list order. *)
                    match parens with
                    | No_parens -> [ insert c.payload_start expect ]
                    | Before { start; stop } ->
                        if parenthesized source start
                        then [ insert c.payload_start expect ]
                        else
                          [ insert start "("
                          ; insert stop ")"
                          ; insert c.payload_start expect
                          ]
                    | Around { start } ->
                        [ insert start "("; insert c.payload_start (expect ^ ")") ]))
              cs
          in
          let edits =
            (* An insertion at the start of a deletion goes first. *)
            List.stable_sort
              (fun (s, e, _) (s', e', _) -> compare (s, e : int * int) (s', e'))
              edits
          in
          let buf = Buffer.create (String.length source + 256) in
          let pos = ref 0 in
          List.iter
            (fun (s, e, r) ->
              if s >= !pos
              then (
                Buffer.add_substring buf source !pos (s - !pos);
                Buffer.add_string buf r;
                pos := e))
            edits;
          Buffer.add_substring buf source !pos (String.length source - !pos);
          write_file (path ^ ".corrected") (Buffer.contents buf))
    tbl;
  !unreadable
