// 20.3.1
let vat n = fun y -> float(y) / 100.0 * float(n) + float(y)

// 20.3.2
let unvat n = fun y -> float(y) / (1.0 + float(n) / 100.0) 

// 20.3.3
let min f =
    let rec check =
        function
        | x when f x = 0 -> x
        | x -> check (x + 1)

    check 1