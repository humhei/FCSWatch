
module TestCode

type System.String with 
    static member GetLength3(x:string) = x.Length

let y = System.String.GetLength3("abc") 
let z = if y <> 3 then failwith "fail!" else 1
        