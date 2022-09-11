let rec len = fun x -> (match x with
  | [] -> 0
  | _::more -> 1 + len more
) in

let rec firsthalf = fun (x,n) -> (match (x,n) with
  | (_,0) -> []
  | ([],_) -> x
  | (a::more, _) -> a::(firsthalf (more, (n-1)))
) in 
let rec lasthalf = fun (x,n) -> (match (x,n) with
  | (_,0) -> x
  | ([],_) -> x
  | (_::more, _) -> lasthalf (more, n-1)
) in 
let split = fun x -> (
  let n = (len x)/2 in (* number of elements to keep/remove*)
  (firsthalf(x, n), lasthalf(x,n))
) in

let rec merge = fun (a,b) -> (match (a,b) with
  | ([],y) -> y
  | (x,[]) -> x
  | (x::amore, y::bmore) -> (match x < y with
                              | true -> x:: merge(amore, b)
                              | false -> y:: merge(a, bmore)
                              )
) in
let rec sort = fun list -> (match list with
  | [] -> []
  | [_] -> list
  | _ -> let (left, right) = split(list) in
          let leftsort = sort(left) in
          let rightsort = sort(right) in
          merge(leftsort, rightsort)
) in
let _ = sort [3;2;4;5;1] in
sort [5;1]
