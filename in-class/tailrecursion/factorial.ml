let fact = fun n -> (
  let rec fact_helper = fun (n, acc) -> (
    match n with
    | 0 -> acc
    | _ -> fact_helper(n-1, n*acc)
  )
 in fact_helper(n, 1)
)
in fact 12