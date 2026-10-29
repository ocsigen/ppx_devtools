let f = function
  | Some (1 [@if ocaml_version < (4, 0, 0)]) -> ()
  | _ -> ()
