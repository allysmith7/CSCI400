type tree = Leaf of int | Node of tree * int * tree ;;

let rec reduce_tree = fun (t, f) -> match t with
  | Leaf(x) -> x
  | Node(lt, x, rt) -> f ( reduce_tree (lt, f), x, reduce_tree (rt, f))

in reduce_tree ( Node(Leaf(1), 2, Node(Leaf(3), 4, Leaf(5))), (fun (a, b, c) -> (a+b+c)))