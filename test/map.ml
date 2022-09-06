(* Applies a function over a list *)
let square x = x*x in (* alternative function definition *)
let rec map = fun (f,x) -> (match x with
  | [] -> []
  | a::more -> f a :: map (f, more)
) in
map (square, [1;2;3])
