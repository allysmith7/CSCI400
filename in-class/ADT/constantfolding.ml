type operator = Plus | Minus | Multiply ;;

type expr = Identifier of string
          | Number of int
          | BinOp of expr * operator * expr
          | Paren of expr ;;

let rec constant_fold e = match e with
  | Paren(ie) -> constant_fold ie
  | BinOp(le,op,re) -> let (left,right) = (constant_fold le, constant_fold re) in
                        (match (left, right) with
                        | (Number(l), Number(r)) -> (match op with
                                                    | Plus -> Number(l + r)
                                                    | Minus -> Number(l - r)
                                                    | Multiply -> Number(l * r))
                        | _ -> BinOp(left,op, right))
  | _ -> e
in
constant_fold (BinOp(Number(3),Minus,Number(400)))