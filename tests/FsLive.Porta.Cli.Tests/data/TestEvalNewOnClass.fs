
module TestCode

type C(x: int) = 
    member __.X = x

let y = C(3)
let z = if y.X <> 3 then failwith "fail!" else 1
        