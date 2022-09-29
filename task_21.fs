// 50.2.1
let rec fact n =
    match n with
        | 0 -> 1
        | n -> n * fact (n - 1)

let fac_seq = seq {
    let mutable i = 0
    while true do
        yield fact i
        i <- i + 1
}


// 50.2.2
let seq_seq = seq {
    let mutable i = 0
    let mutable res = 0
    while true do
        if i = 0 then res <- i
        elif i % 2 = 1 then res <- -i / 2 - 1
        else res <- i / 2
        yield res
        i <- i + 1
}