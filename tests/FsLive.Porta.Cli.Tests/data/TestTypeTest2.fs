
module TestCode

let x = match box 2 with :? string as a -> failwith "fail!" | _ -> 1
if x <> 1 then failwith "fail fail!" 
        