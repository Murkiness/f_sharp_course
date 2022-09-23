type F = 
  | AM
  | PM

type TimeOfDay = { hours : int; minutes : int; f: F }
    
let convert_time = function
 | (x, PM) -> (x.hours + 12) * 60 + x.minutes
 | (x, AM) -> x.hours * 60 + x.minutes

let (.>.) x y = (convert_time (x, x.f)) > (convert_time (y, y.f))