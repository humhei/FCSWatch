
module TestCode

type System.String with 
    member x.GetLength2(y:int) = x.Length + y

let y = "ab".GetLength2(5) 
let z = if y <> 7 then failwith "fail!" else 1
        