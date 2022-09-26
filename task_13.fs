// 39.1
let rec rmodd : int list -> int list = function
    | h :: (h2 :: tail) -> h2 :: rmodd(tail)
    | [] -> []
    | [h] -> []

// 39.2
let rec del_even = function
    | [] -> []
    | h :: tail when h % 2 = 0 ->  del_even(tail)
    | h :: tail -> h ::  del_even(tail)

// 39.3
let rec multiplicity x xs = match xs with
    | [] -> 0
    | h ::tail -> (if h = x then 1 else 0) + multiplicity x tail

// 39.4
let rec do_split = function
    | ([], (l1, l2)) -> (List.rev l1, List.rev l2)
    | (h :: (h2 :: tail), (l1, l2)) -> do_split(tail, (h :: l1, h2 :: l2))
    | ([h], (l1, l2)) -> do_split([], (h :: l1, l2))

let rec split = fun li -> do_split(li, ([], []))

// 39.5
let rec zip (xs1, xs2) = match (xs1, xs2) with
 | ([], []) -> []
 | (h_1 :: tail_1, h_2 :: tail_2) -> (h_1, h_2) :: zip (tail_1, tail_2)