type tree = Empty | Node of tree * int * tree ;;

let rec map = fun f -> 
  fun t -> (match t with 
        | Empty -> t
        | Node(lt,x,rt) -> Node(map f lt, f x, map f rt))
in map (fun n -> n*2) Node(Empty,3,Node(Empty,2,Empty))