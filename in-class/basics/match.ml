(* Extracts the third element from a list *)
let x = 
  match [7;8;9] with
    | [x;y;z;] -> z
    | _ -> -1
in
x
