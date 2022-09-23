type TimeOfDay = { hours: int; minutes: int; f: string }

let convert_time (t: TimeOfDay)  =
    if t.f = "PM" then
        (12 + t.hours) * 60 + t.minutes
    else
        t.hours * 60 + t.minutes

let (.>.) x y = (convert_time x) > (convert_time y)