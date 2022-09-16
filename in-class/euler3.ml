let rec factorize_helper = fun (factor, number) -> (
  match (number <= 2) with
    | true -> number
    | false -> (
        match factor >= number with
          | true -> factor
          | false -> (match number mod factor with
                      | 0 -> factorize_helper (factor, number / factor)
                      | _ -> factorize_helper (factor+1, number)
                    )
    )
) in
let factorize = fun number ->
        factorize_helper (2, number) in
factorize (600851475143)
