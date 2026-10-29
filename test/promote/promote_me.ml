(* Every expectation below is wrong on purpose. *)

let%expect_test "guarded variant, parenthesized" =
  print_string "actual";
  [%expect ({| stale |} [@when ocaml_version >= (4, 0, 0)])];
  [%expect {| default |}]

let%expect_test "guarded multiline variant, parenthesized" =
  print_string "line 1\nline 2";
  [%expect ({|
    stale 1
    stale 2
    |} [@when ocaml_version >= (4, 0, 0)])];
  [%expect {| default |}]

let%expect_test "guarded variant, regular string" =
  print_string "actual";
  [%expect ("stale" [@when ocaml_version >= (4, 0, 0)])];
  [%expect {| default |}]

let%expect_test "default variant" =
  print_string "actual";
  [%expect ({| other |} [@when ocaml_version < (4, 0, 0)])];
  [%expect {| stale |}]

let%expect_test "empty" =
  print_string "actual";
  [%expect]

let%expect_test "no default variant" =
  print_string "actual";
  [%expect ({| other |} [@when ocaml_version < (4, 0, 0)])]

let%expect_test "trailing output" =
  print_string "checked";
  [%expect {| checked |}];
  print_string "line 1\nline 2"

let%expect_test "trailing output, after a let" =
  let s = "trailing" in
  print_string s

let%expect_test "trailing output, after a match" =
  match Some 1 with
  | Some _ -> print_string "some"
  | None -> print_string "none"

[@@@ocamlformat "disable"]

(* ocamlformat would drop the parentheses under test. *)
let%expect_test "trailing output, after a parenthesized match" =
  (match Some 1 with
  | Some _ -> print_string "some"
  | None -> print_string "none")
[@@@ocamlformat "enable"]

let%expect_test "trailing output, after a try in a let" =
  let s = "try" in
  try print_string s with Exit -> ()

let%expect_test "no default variant, in if-then-else" =
  print_string "actual";
  if true then [%expect ({| other |} [@when ocaml_version < (4, 0, 0)])] else ()

let%expect_test "no default variant, in if-then" =
  print_string "actual";
  if true then [%expect ({| other |} [@when ocaml_version < (4, 0, 0)])]

let () = Ppx_devtools_expect_runtime.exit ()
