
module TestCode

let even a = 
    let rec even x = (if x = 0 then true else odd (x-1))
    and odd x = (if x = 0 then false else even (x-1))
    even a

if even 11 then failwith "fail!" 
if not (even 10) then failwith "fail fail!" 
        