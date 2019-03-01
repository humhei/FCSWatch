
module TestCode

let mutable x = 0
if x <> 0 then failwith "failure A!" else 1
let y(c:int) = 
    (x <- x + 1
     x)
let z1 = y(3)
if x <> 1 then failwith "failure B!" else 1
let z2 = y(4)
if x <> 2 then failwith "failure C!" else 1
if z1 <> 1 || z2 <> 2 then failwith "failure D!" else 1
        