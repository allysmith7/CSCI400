let rec fib_helper = fun (last1, last2, max) -> (
  let next = last2 + last1 in
  match next > max with
    | true -> 0
    | false -> (
        (* OCaml checks for exhaustive match based 
          on type only, not range of values *)
        match (next mod 2) = 0 with
          | true -> next
          | false -> 0
        ) + fib_helper(next, last1, max)
) in
let fib = fun max -> 
    fib_helper (1, 1, max) in
fib (40000000)