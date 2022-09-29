// 49.5.1
let even_seq = Seq.initInfinite (fun i -> (i + 1) * 2)

// 49.5.2
let rec fact n =
    match n with
        | 0 -> 1
        | n -> n * fact (n - 1)

let fac_seq = Seq.initInfinite fact

// 49.5.3
let seq_seq = Seq.initInfinite (fun x -> if x = 0 then 0 elif x % 2 = 1 then -x / 2 - 1 else x / 2)