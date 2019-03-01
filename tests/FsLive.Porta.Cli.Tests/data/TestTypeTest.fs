
module TestCode

let x = match box 1 with :? int as a -> a | _ -> failwith "fail!"
if x <> 1 then failwith "fail fail!" 
        