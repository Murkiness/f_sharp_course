// 16.1
let notDivisible (n, m) = m % n = 0

// 16.2
let  prime n = 
    let sqrt = (float >> sqrt >> int) n
    [ 2 .. sqrt ]
    |> List.forall (fun x -> n % x <> 0)