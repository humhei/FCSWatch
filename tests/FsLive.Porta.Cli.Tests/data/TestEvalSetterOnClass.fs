
module TestCode

type C(x: int) = 
    let mutable y = x
    member __.Y with get() = y and set v = y <- v

let c = C(3)
if c.Y <> 3 then failwith "fail!" 
c.Y <- 4
if c.Y <> 4 then failwith "fail! fail!" 
        