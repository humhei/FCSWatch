
module TestCode


type UserType = A of int | B
let f () = 
    let l = [ 1; 2; 3 ]
    let s = Set.ofList [ 1; 2; 3 ]
    let m = Map.ofList [ (1,1) ]
    if l.Length <> 3 then failwith "unexpected"
    if s.Count <> 3 then failwith "unexpected"
    if m.Count <> 1 then failwith "unexpected"
    let a = [| UserType.A 1 ]
    if a.Length <> 1 then failwith "unexpected"

f()
