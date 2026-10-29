(* ocamlformat would move the literal, and so change how the CR is indented. *)

[@@@ocamlformat "disable"]

let%expect_test "different outputs, promoted" =
  List.iter
    (fun s ->
      print_string s;
      [%expect {|
               (* CR ppx_devtools-expect: this [%expect] saw different outputs *)
               ========================== Output 1 / 2 ==========================
               a
               ========================== Output 2 / 2 ==========================
               b
               |}])
    [ "b"; "a"; "b" ]

(* The error is reported on the variant holding the CR. *)
let%expect_test "different outputs, promoted, in a group" =
  List.iter
    (fun s ->
      print_string s;
      [%expect ({| other |} [@when ocaml_version < (4, 0, 0)])];
      [%expect {|
               (* CR ppx_devtools-expect: this [%expect] saw different outputs *)
               ========================== Output 1 / 2 ==========================
               a
               ========================== Output 2 / 2 ==========================
               b
               |}])
    [ "b"; "a"; "b" ]

let () = Ppx_devtools_expect_runtime.exit ()
