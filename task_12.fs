// 34.1
let rec upto n = [1 .. n]

// 34.2
let rec dnto = function
 | n when n <= 0 -> []
 | n -> n :: dnto(n - 1)

// 34.3
let evenn n = 
    let rec get_list = function
        | (0, i) -> []
        | (n, i) -> i * 2 :: get_list ((n - 1), (i + 1))
    get_list (n, 0)