// 43.3

let try_find key m = 
    let rec find = function
        | [] -> None
        | (k, value) :: tail -> if k = key then Some value else find tail

    find (Map.toList m)