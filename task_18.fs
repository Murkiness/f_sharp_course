// 47.4.1
let f n = 
    let mutable fact = 1
    let mutable x = n
    while x > 1 do
        fact <- fact * x
        x <- x - 1

    fact

// 47.4.2
let fibo n =
    let mutable i = 0
    let mutable fib_sum = 0
    let mutable fib_1 = 1
    let mutable fib_2 = 1
    while i < n - 2 do
        fib_sum <- fib_1 + fib_2
        fib_1 <- fib_2
        fib_2 <- fib_sum
        i <- i + 1
    
    fib_sum