// 40.1
let rec sum = function
    | (p, []) -> 0
    | (p, head :: tail) when (p head) -> head + sum (p, tail)
    | (p, head :: tail) when not (p head) -> sum (p, tail)

// 40.2.1
let rec count = function
    | ([], n) -> 0
    | (head :: tail, n) when head = n -> 1 + count (tail, n)
    | (head :: tail, n) when head < n -> count (tail, n)
    | _ -> 0

// 40.2.2
let rec insert = function
    | ([], n) -> [n]
    | (head :: tail, n) when n > head -> head :: insert (tail, n)
    | (head :: tail, n) when n <= head -> n :: (head :: tail)

// 40.2.3
let rec intersect = function
    | (head :: tail, head_2 :: tail_2) when head < head_2 -> intersect (tail, head_2 :: tail_2)
    | (head :: tail, head_2 :: tail_2) when head > head_2 -> intersect (head :: tail, tail_2)
    | (head :: tail, head_2 :: tail_2) when head = head_2 -> head :: intersect (tail, tail_2)
    | _ -> []

// 40.2.4
let rec plus = function
    | (xs1, []) -> xs1
    | ([], xs2) -> xs2
    | (xs1, head :: tail) -> plus (insert (xs1, head), tail)

// 40.3.2
let rec delete = function
    | (n, []) -> []
    | (n, head :: tail) when n = head -> tail
    | (n, head :: tail) when n <> head -> head :: delete (n, tail)

// 40.2.5
let rec minus = function
    | (xs1, []) -> xs1
    | ([], xs2) -> []
    | (xs1, head :: tail) -> minus (delete (head, xs1), tail)

// 40.3.1
let rec get_smallest = function
    | ([], current) -> Some current
    | (head :: tail, current) when head < current -> get_smallest (tail, head)
    | (head :: tail, current) when head >= current -> get_smallest (tail, current)

let rec smallest = function
    | head :: tail -> get_smallest (tail, head)

// 40.3.3
let rec sort = function
    | [] -> []
    | xs -> (smallest xs).Value::sort (delete ((smallest xs).Value, xs))

// 40.4
let rec revrev = fun xs ->
    let rec rev xss = match xss with
    |   [] -> []
    |   head::tail -> (rev tail)@[head]

    match xs with
    |   [] -> []
    |   head::tail -> (revrev tail)@[rev head]
