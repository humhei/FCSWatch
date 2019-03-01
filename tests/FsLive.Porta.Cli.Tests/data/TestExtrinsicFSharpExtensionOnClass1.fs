
module TestCode

type System.String with 
    member x.GetLength() = x.Length

let y = "a".GetLength() 
let z = if y <> 1 then failwith "fail!" else 1
        