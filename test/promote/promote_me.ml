(* Every expectation below is wrong on purpose, except where noted. *)

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

let%expect_test "uncaught exception" =
  print_string "checked";
  [%expect {| checked |}];
  print_string "trailing";
  failwith "boom"

let%expect_test "uncaught exception, no trailing output" = raise Exit

let%expect_test "uncaught exception, stale" = raise Exit
[@@expect.uncaught_exn {| Not_found |}]

let%expect_test "uncaught exception, as expected" = raise Exit
[@@expect.uncaught_exn {| Stdlib.Exit |}]

let%expect_test "uncaught exception, no longer raised" =
  print_string "trailing";
  print_newline ()
[@@expect.uncaught_exn {| Stdlib.Exit |}]

let%expect_test "different outputs" =
  List.iter
    (fun s ->
      print_string s;
      [%expect {| a |}])
    [ "a"; "b"; "a" ]

let%expect_test "same outputs" =
  List.iter
    (fun s ->
      print_string s;
      [%expect {| a |}])
    [ "a"; "a" ]

let%expect_test "reached [%expect.unreachable]" =
  print_string "reached";
  [%expect.unreachable]

let%expect_test "unreached [%expect.unreachable]" =
  if false then [%expect.unreachable];
  print_string "reached";
  [%expect {| reached |}]

let%expect_test "unreached [%expect]" =
  if false
  then begin
    print_string "unreached";
    [%expect {| unreached |}]
  end;
  print_string "reached";
  [%expect {| reached |}]

let%expect_test "unreached [%expect], after an exception" =
  print_string "reached";
  [%expect {| reached |}];
  if true then raise Exit;
  [%expect {| unreached |}]
[@@expect.uncaught_exn {| Stdlib.Exit |}]

let%expect_test "unreached [@when] variant" =
  if false
  then begin
    print_string "unreached";
    [%expect ({| unreached |} [@when ocaml_version >= (4, 0, 0)])];
    [%expect {| default |}]
  end;
  print_string "reached";
  [%expect {| reached |}]

let%expect_test "unreached default variant" =
  if false
  then begin
    print_string "unreached";
    [%expect ({| other |} [@when ocaml_version < (4, 0, 0)])];
    [%expect {| unreached |}]
  end;
  print_string "reached";
  [%expect {| reached |}]

let%expect_test "reached [@when] [%expect.unreachable]" =
  print_string "reached";
  [%expect.unreachable] [@when ocaml_version >= (4, 0, 0)];
  [%expect {| default |}]

let () = Ppx_devtools_expect_runtime.exit ()
