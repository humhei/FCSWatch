
module TestCode

let f () = 
    let mutable x = 1
    x <- x + 1
    x <- x + 1
    x
if f() <> 3 then failwith "fail fail!" 
        