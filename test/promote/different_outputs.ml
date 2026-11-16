(* ocamlformat would move the literal, and so change how the CR is indented. *)

[@@@ocamlformat "disable"]

let%expect_test "different outputs, promoted" =
  List.iter
    (fun s ->
      print_string s;
      [%expect {|
               (* CR expect_test: Test ran multiple times with different test outputs *)
               ============================= Output 1 / 2 ==============================
               a
               ============================= Output 2 / 2 ==============================
               b
               |}])
    [ "b"; "a"; "b" ]

let () = Ppx_devtools_expect_runtime.exit ()
