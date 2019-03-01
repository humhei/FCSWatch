
module TestCode

module Exceptions = 
    let x2 = try invalidArg "a" "wtf" with :? System.ArgumentException -> () 
    let x4 = try failwith "hello" with e -> () 
    let x5 = try 1 with e -> failwith "fail!" 
    if x5 <> 1 then failwith "fail! fail!" 
        