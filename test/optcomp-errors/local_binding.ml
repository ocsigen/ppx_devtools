let x =
  let y = 1 [@@if ocaml_version < (4, 0, 0)] in
  y
