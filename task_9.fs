// 23.4.1
let rec compute = function
 | (a, b, c) when c > 11 -> compute(a, b + (c / 12), c % 12)
 | (a, b, c) when b > 19 -> compute(a + (b / 20), b % 20, c)
 | (a, b, c) when c < 0 && b > 0 -> compute(a, b - 1, c + 12)
 | (a, b, c) when b < 0 && a > 0 -> compute(a - 1, b + 20, c)
 | (a, b, c) when c < 0 && a > 0 -> compute(a - 1, b + 19 ,c + 12)
 | (a, b, c) -> (a, b, c)

let (.+.) (a, b, c) (x, y, z) = compute(a + x, b + y, c + z)
let (.-.) (a, b, c) (x, y, z) = compute(a - x, b - y, c - z)

// 23.4.2
let (.+) (a, b) (x, y) = (a + x, b + y)
let (.-) (a, b) (x, y) = (a - x, b - y)
let (.*) (a, b) (x, y) = (a * x - b * y, b * x + a * y)
let (./) (a, b) (x, y) = (a, b) .* (x / (x * x + y * y), -y / (x * x + y * y))