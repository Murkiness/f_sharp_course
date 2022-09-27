// 41.4.1
let list_filter f xs =
    let foldFunc x acc = if f x then x::acc else acc
    List.foldBack foldFunc xs []

// 41.4.2
let sum (p, xs) =
    let foldFunc acc x = if p x then acc + x else acc
    List.fold foldFunc 0 xs

// 41.4.3
let revrev = fun xs ->
    let foldFunc x tail = (List.rev tail)::x
    List.fold foldFunc [] xs