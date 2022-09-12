type operator = Plus | Minus | Multiply ;;

type expr = Identifier of string
          | Number of int
          | BinOp of expr * operator * expr
          | Paren of expr ;;

let rec replace = fun (t,x,r) -> (match t with
  | Paren(ie) -> Paren(replace (ie,x,r))
  | BinOp(le,op,re) -> let (left,right) =
                    (replace (le,x,r), replace (re,x,r)) in
                    BinOp(left, op, right)
  | Number(_) -> t
  | Identifier(y) -> (match x = y with
                      | true -> r  (* Replacing! *)
                      | false -> t)
) in
replace (Identifier("x"), "x", BinOp(Number(1), Plus, Identifier("y")));;
