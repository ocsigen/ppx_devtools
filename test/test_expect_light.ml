let%expect_test "hello" =
  print_string "hello world";
  [%expect {| hello world |}]

let%expect_test "two points" =
  print_string "first";
  [%expect {| first |}];
  print_string "second";
  [%expect {| second |}]

let%expect_test "output accessor" =
  print_string "grab me";
  let s = [%expect.output] in
  print_string (String.uppercase_ascii s);
  [%expect {| GRAB ME |}]

let%expect_test ("native skip" [@tags "native-only"]) =
  print_string "only on native";
  [%expect {| only on native |}]

let%expect_test "conditional backend" =
  print_string
    (match Sys.backend_type with
    | Native | Bytecode -> "is native"
    | Other _ -> "not native");
  [%expect ({| not native |} [@when js || wasm])];
  [%expect {| is native |}]

let%expect_test "conditional version" =
  Printf.printf "ocaml %s" (if true then "modern" else "ancient");
  [%expect ({| ocaml ancient |} [@when ocaml_version < (4, 0, 0)])];
  [%expect {| ocaml modern |}]

let%expect_test "multiline" =
  print_string "line1\nline2\n  indented\n";
  [%expect {|
    line1
    line2
      indented |}]

(* Gated out on every supported OCaml: must never run (its body would fail). *)
let%expect_test "gated away" =
  print_string "should never be checked";
  [%expect {| deliberately wrong |}]
[@@if ocaml_version < (4, 0, 0)]

(* Gated patterns, in the cases of a match and of a function. *)
let%expect_test "gated patterns" =
  let f x =
    match x with
    | (1 [@if ocaml_version < (4, 0, 0)]) -> "gated"
    | 1 | (2 [@if ocaml_version < (4, 0, 0)]) -> "one"
    | _ -> "other"
  in
  let g = function
    | (1 [@if ocaml_version < (4, 0, 0)]) -> "gated"
    | 1 | (2 [@if ocaml_version < (4, 0, 0)]) -> "one"
    | _ -> "other"
  in
  Printf.printf "%s %s %s %s" (f 1) (f 2) (g 1) (g 2);
  [%expect {| one other one other |}]

(* Gated variant constructors and type declarations. *)
module Flags : sig
  type t =
    | A
    | B [@if ocaml_version < (4, 0, 0)]
    | C

  type gone = int [@@if ocaml_version < (4, 0, 0)]

  type e = ..

  type e += E1 | E2 [@if ocaml_version < (4, 0, 0)]

  type e += E3 [@if ocaml_version < (4, 0, 0)]

  val all : t list
end = struct
  type t =
    | A
    | B [@if ocaml_version < (4, 0, 0)]
    | C

  type gone = int [@@if ocaml_version < (4, 0, 0)]

  type gone = string [@@if ocaml_version >= (4, 0, 0)]

  let _ : gone = "kept"

  let all = [ A; C ]

  type e = ..

  type e += E1 | E2 [@if ocaml_version < (4, 0, 0)]

  type e += E3 [@if ocaml_version < (4, 0, 0)]

  let _ = function
    | E1 -> ()
    | (E2 [@if ocaml_version < (4, 0, 0)]) -> ()
    | _ -> ()
end

let%expect_test "gated constructors" =
  List.iter
    (fun (x : Flags.t) ->
      print_string
        (match x with
        | A -> "A"
        | (B [@if ocaml_version < (4, 0, 0)]) -> "B"
        | C -> "C"))
    Flags.all;
  [%expect {| AC |}]

let%expect_test "extension axes" =
  print_string "extended";
  [%expect ({| boolean axis |} [@when disabled])];
  [%expect ({| string axis |} [@when flavour <> "override"])];
  [%expect ({| int axis |} [@when level > 3])];
  [%expect ({| version axis |} [@when version >= (1, 3)])];
  [%expect ({| extended |} [@when enabled && extra && flavour = "override" && level = 3])];
  [%expect {| default |}]

let%expect_test ("gated by an extension axis" [@when disabled]) =
  print_string "should never be checked";
  [%expect {| deliberately wrong |}]

(* An axis named like a [Stdlib] value. *)
let%expect_test "axis named like a Stdlib value" =
  print_string "ok";
  [%expect ({| ok |} [@when not compare])];
  [%expect {| default |}]

let%expect_test "overridden predefined axis" =
  print_string "overridden";
  [%expect ({| overridden |} [@when cygwin])];
  [%expect {| default |}]

(* Generated predicates must not use the operators of the test module. Keep
   this at the end of the file: it shadows them for what follows. *)
open struct
  [@@@warning "-32"]

  let ( > ) (_ : int) (_ : int) = true

  let ( && ) (_ : bool) (_ : bool) = false

  let not (_ : bool) = true
end

let%expect_test "operators shadowed by the test module" =
  print_string "not shadowed";
  [%expect ({| not shadowed |} [@when (not (level > 3)) && enabled])];
  [%expect {| shadowed |}]
