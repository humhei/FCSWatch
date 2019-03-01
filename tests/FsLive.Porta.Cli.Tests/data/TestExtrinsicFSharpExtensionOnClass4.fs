
module TestCode

type System.String with 
    member x.LengthProp = x.Length

let y = "abcd".LengthProp
let z = if y <> 4 then failwith "fail!" else 1
        