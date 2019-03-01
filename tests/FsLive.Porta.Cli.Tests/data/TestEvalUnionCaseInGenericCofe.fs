
module TestCode

let f<'T>(x:'T) = Some x

let y = f 3
printfn "y = %A, y.GetType() = %A" y (y.GetType())
        