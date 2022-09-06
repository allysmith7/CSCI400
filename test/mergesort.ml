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
let split = fun (x, f) -> (
  let n = (len x)/2 in (* number of elements to keep/remove*)
  (f(firsthalf(x, n)), f(lasthalf(x,n)))
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
  | [a] -> list
  | [a;b] -> (match a < b with
              | true -> [a;b]
              | false -> [b;a]
              )
  | l -> merge(split(list, sort))
) in
sort [3;2;4;5;1]
