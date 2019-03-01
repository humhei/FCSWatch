
module TestCode

module Options = 
    let x2 = None : int option 
    let x3 = Some 3 : int option 
    let x5 = x2.IsNone
    let x6 = x3.IsNone
    let x7 = x2.IsSome
    let x8 = x3.IsSome
        