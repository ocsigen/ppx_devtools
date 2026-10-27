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

(** Minimal version of ppx_optcomp
    It only support the following attribute
    {[
      [@if ocaml_version < (4,12,0)]
      [@if os_type <> "Win32"]
    ]}
    The supported predicates are [ocaml_version], [ast_version], [oxcaml],
    [introspect], [arch_sixtyfour] and
    [os_type] (a string, e.g. "Unix" / "Win32" / "Cygwin"),
    combined with the usual comparison and boolean operators (see
    {!Ppx_devtools_predicate.Predicate}). They can be placed
    on module (Pstr_module, Psig_module),
    module types (Pstr_modtype, Psig_modtype),
    toplevel bindings (Pstr_value, Pstr_primitive, Psig_value),
    exceptions (Pstr_exception, Psig_exception),
    type declarations, variant and extension constructors,
    toplevel extensions, e.g. [let%expect_test "..." = ... [@@if ...]]
    (Pstr_extension),
    pattern in case (pc_lhs) and the alternatives of an or-pattern,
    and as a floating attribute [@@@if ...], gating the rest of the structure
    or signature. An [@if] anywhere else is an error.

    For toplevel extensions to be gated we must run before context-free
    rewriters such as ppx_devtools-expect expand them; this is achieved by
    registering the implementation transformation as a [Before]
    instrumentation (see registration at the end of the file).
*)

open StdLabels
open Ppxlib.Parsetree
module Predicate = Ppx_devtools_predicate.Predicate

(* Locations of the [@if] attributes that have been evaluated. Any other
   [@if] left in the tree is in a position that is not supported. *)
let evaluated = Hashtbl.create 16

let keep _loc (attrs : attributes) =
  let ifs =
    List.filter attrs ~f:(function
      | { attr_name = { txt = "if"; _ }; _ } -> true
      | _ -> false)
  in
  match ifs with
  | [] -> true
  | _ -> (
      let keep_one ({ attr_payload; attr_loc; _ } as attr) =
        Ppxlib.Attribute.mark_as_handled_manually attr;
        Hashtbl.replace evaluated attr_loc ();
        let e =
          match attr_payload with
          | PStr [ { pstr_desc = Pstr_eval (e, []); _ } ] -> e
          | _ -> raise (Predicate.Invalid attr_loc)
        in
        Predicate.eval_compile_time (Predicate.parse e)
      in
      try List.for_all ~f:keep_one ifs
      with Predicate.Invalid loc ->
        Location.raise_errorf ~loc "Invalid attribute format")

let filter_map ~f l =
  let l =
    List.fold_left
      ~f:(fun acc x ->
        match f x with
        | Some x -> x :: acc
        | None -> acc)
      ~init:[]
      l
  in
  List.rev l

let rec filter_pattern = function
  | { ppat_desc = Ppat_or (p1, p2); ppat_attributes; ppat_loc; _ } as p -> (
      if not (keep ppat_loc ppat_attributes)
      then None
      else
        match filter_pattern p1, filter_pattern p2 with
        | None, None -> None
        | Some p1, None -> Some p1
        | None, Some p2 -> Some p2
        | Some p1, Some p2 -> Some { p with ppat_desc = Ppat_or (p1, p2) })
  | { ppat_attributes; ppat_loc; _ } as p ->
      if keep ppat_loc ppat_attributes then Some p else None

let drop_attr =
  let attr =
    { attr_name = Location.mknoloc "ppx_devtools.optcomp.dropped"
    ; attr_loc = Location.none
    ; attr_payload = PStr []
    }
  in
  Ppxlib.Attribute.mark_as_handled_manually attr;
  attr

let drop_str loc = { pstr_desc = Pstr_attribute drop_attr; pstr_loc = loc }

let drop_sig loc = { psig_desc = Psig_attribute drop_attr; psig_loc = loc }

let is_if_attr = function
  | { attr_name = { txt = "if"; _ }; _ } -> true
  | _ -> false

let strip_if attrs = List.filter attrs ~f:(fun a -> not (is_if_attr a))

(* Attributes that gate a toplevel extension item such as
   [let%expect_test "..." = ... [@@if ...]]. The parser attaches the [@@if]
   to the binding inside the extension payload, so we look there (and on the
   extension item itself) for the gating attributes. *)
let extension_if_attrs ((_, payload) : extension) ext_attrs =
  let inner =
    match payload with
    | PStr [ { pstr_desc = Pstr_value (_, vbs); _ } ] ->
        List.concat (List.map vbs ~f:(fun vb -> vb.pvb_attributes))
    | PStr [ { pstr_desc = Pstr_eval (_, attrs); _ } ] -> attrs
    | _ -> []
  in
  List.filter (ext_attrs @ inner) ~f:is_if_attr

(* Remove the [@if] attributes once they have been evaluated, so that the
   downstream rewriters (e.g. ppx_devtools-expect) do not choke on a leftover
   attribute in an unexpected position. *)
let strip_extension_if ((name, payload) : extension) ext_attrs =
  let payload =
    match payload with
    | PStr [ ({ pstr_desc = Pstr_value (r, vbs); _ } as item) ] ->
        let vbs =
          List.map vbs ~f:(fun vb ->
              { vb with pvb_attributes = strip_if vb.pvb_attributes })
        in
        PStr [ { item with pstr_desc = Pstr_value (r, vbs) } ]
    | other -> other
  in
  (name, payload), strip_if ext_attrs

let keep_exception { ptyexn_attributes; ptyexn_constructor; ptyexn_loc } =
  keep ptyexn_loc ptyexn_attributes
  && keep ptyexn_constructor.pext_loc ptyexn_constructor.pext_attributes

let filter_type_declarations l =
  filter_map l ~f:(fun d -> if keep d.ptype_loc d.ptype_attributes then Some d else None)

exception Drop_item

(* A floating attribute [@@@if cond] gates the rest of the enclosing structure
   or signature: when [cond] is false every following item is dropped, when it
   is true the marker itself is removed. This is convenient to gate a whole test
   module on an OCaml version without repeating [@@if] on each item. *)
exception Drop_rest

let floating_if loc attr = if keep loc [ attr ] then raise Drop_item else raise Drop_rest

let traverse =
  object (self)
    inherit Ppxlib.Ast_traverse.map as super

    method! structure_item item =
      (* Handle gated toplevel extensions (e.g. [let%expect_test]) before
         recursing, so the whole extension is dropped before context-free
         rewriters expand it. *)
      match item.pstr_desc with
      | Pstr_attribute attr when is_if_attr attr -> floating_if item.pstr_loc attr
      | Pstr_extension (ext, ext_attrs) -> (
          match extension_if_attrs ext ext_attrs with
          | [] -> self#structure_item_default item
          | attrs ->
              if keep item.pstr_loc attrs
              then
                let ext, ext_attrs = strip_extension_if ext ext_attrs in
                super#structure_item
                  { item with pstr_desc = Pstr_extension (ext, ext_attrs) }
              else drop_str item.pstr_loc)
      | _ -> self#structure_item_default item

    method private structure_item_default item =
      let item = super#structure_item item in
      match item.pstr_desc with
      | Pstr_module { pmb_attributes; pmb_loc; _ } ->
          if keep pmb_loc pmb_attributes then item else drop_str pmb_loc
      | Pstr_primitive { pval_attributes; pval_loc; _ } ->
          if keep pval_loc pval_attributes then item else drop_str pval_loc
      | Pstr_value (r, l) -> (
          let l =
            filter_map l ~f:(fun b ->
                if keep b.pvb_loc b.pvb_attributes then Some b else None)
          in
          match l with
          | [] -> drop_str Location.none
          | _ -> { item with pstr_desc = Pstr_value (r, l) })
      | Pstr_type (r, l) -> (
          match filter_type_declarations l with
          | [] -> drop_str item.pstr_loc
          | l -> { item with pstr_desc = Pstr_type (r, l) })
      | Pstr_typext { ptyext_constructors = []; _ } -> drop_str item.pstr_loc
      | Pstr_exception exn -> if keep_exception exn then item else drop_str item.pstr_loc
      | Pstr_modtype { pmtd_attributes; pmtd_loc; _ } ->
          if keep pmtd_loc pmtd_attributes then item else drop_str pmtd_loc
      | _ -> item

    (* Lists are mapped with [self#list], also where the shape of the AST
       differs: with OxCaml's ppxlib, the cases of a [function] are not
       traversed through [cases], and a signature is a record. So list
       elements raise [Drop_item] to be removed, and [Drop_rest] to remove
       the following elements as well. *)
    method! list : 'a. ('a -> 'a) -> 'a list -> 'a list =
      fun f l ->
        let rec loop acc = function
          | [] -> List.rev acc
          | x :: rest -> (
              match f x with
              | x -> loop (x :: acc) rest
              | exception Drop_item -> loop acc rest
              | exception Drop_rest -> List.rev acc)
        in
        loop [] l

    method! case case =
      match filter_pattern case.pc_lhs with
      | None -> raise Drop_item
      | Some pattern -> super#case { case with pc_lhs = pattern }

    method! type_kind kind =
      match super#type_kind kind with
      | Ptype_variant l ->
          Ptype_variant
            (filter_map l ~f:(fun c ->
                 if keep c.pcd_loc c.pcd_attributes then Some c else None))
      | kind -> kind

    method! type_extension ext =
      let ext = super#type_extension ext in
      { ext with
        ptyext_constructors =
          filter_map ext.ptyext_constructors ~f:(fun c ->
              if keep c.pext_loc c.pext_attributes then Some c else None)
      }

    method! signature_item item =
      match item.psig_desc with
      | Psig_attribute attr when is_if_attr attr -> floating_if item.psig_loc attr
      | _ -> self#signature_item_default item

    method private signature_item_default item =
      let item = super#signature_item item in
      match item.psig_desc with
      | Psig_module { pmd_attributes; pmd_loc; _ } ->
          if keep pmd_loc pmd_attributes then item else drop_sig pmd_loc
      | Psig_value { pval_attributes; pval_loc; _ } ->
          if keep pval_loc pval_attributes then item else drop_sig pval_loc
      | Psig_type (r, l) -> (
          match filter_type_declarations l with
          | [] -> drop_sig item.psig_loc
          | l -> { item with psig_desc = Psig_type (r, l) })
      | Psig_typext { ptyext_constructors = []; _ } -> drop_sig item.psig_loc
      | Psig_exception exn -> if keep_exception exn then item else drop_sig item.psig_loc
      | Psig_modtype { pmtd_attributes; pmtd_loc; _ } ->
          if keep pmtd_loc pmtd_attributes then item else drop_sig pmtd_loc
      | _ -> item
  end

(* Fail on the [@if] attributes that were not evaluated, rather than silently
   keeping the code they are attached to. *)
let check_evaluated =
  object
    inherit Ppxlib.Ast_traverse.iter as super

    method! attribute attr =
      if is_if_attr attr && not (Hashtbl.mem evaluated attr.attr_loc)
      then Location.raise_errorf ~loc:attr.attr_loc "[@if] is not supported here";
      super#attribute attr
  end

let structure items =
  let items = traverse#structure items in
  check_evaluated#structure items;
  items

let signature items =
  let items = traverse#signature items in
  check_evaluated#signature items;
  items

let () =
  Ppxlib.Driver.register_transformation
    ~instrument:
      (Ppxlib.Driver.Instrument.make structure ~position:Ppxlib.Driver.Instrument.Before)
    ~intf:signature
    "ppx_devtools-optcomp"
