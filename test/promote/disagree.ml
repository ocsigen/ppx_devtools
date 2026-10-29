let%expect_test "disagreeing runs" =
  List.iter
    (fun s ->
      print_string s;
      [%expect {| a |}])
    [ "a"; "b"; "c" ]

let%expect_test "agreeing runs" =
  List.iter
    (fun s ->
      print_string s;
      [%expect {| a |}])
    [ "a"; "a" ]

let () = Ppx_devtools_expect_runtime.exit ()
