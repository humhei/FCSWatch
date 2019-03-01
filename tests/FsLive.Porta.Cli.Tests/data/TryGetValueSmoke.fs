
module TestCode

let m = dict  [ (1,"2") ]
let f() = 
    match m.TryGetValue 1 with
    | true, v -> if v <> "2" then failwith "fail!"
    | _ -> failwith "fail2!"

f()
       