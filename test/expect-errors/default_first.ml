let%expect_test "default first" =
  print_string "output";
  [%expect {| default |}];
  [%expect ({| variant |} [@when js])]
