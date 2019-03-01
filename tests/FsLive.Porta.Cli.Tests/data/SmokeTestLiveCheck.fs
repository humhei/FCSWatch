
module TestCode

module SmokeTestLiveCheck = 
    type LiveCheckAttribute() = 
        inherit System.Attribute()
    
    let mutable v = 0
    
    let x1 = 
        v <- v + 1
        4 

    [<LiveCheck>]
    let x2 = 
        v <- v + 1
        4 

    [<LiveCheck>]
    let x3 = 
        // For live checking, bindings are executed on-demand
        // 'v' is only incremented once - because `x1` is not yet evaluated!
        if v <> 1 then failwithf "no way John, v = %d" v

        let y1 = x2 + 3
        
        // 'v' has not been incremented again - because `x2` is evaluated once!
        if v <> 1 then failwithf "no way Jane, v = %d" v
        if y1 <> 7 then failwithf "no way Juan, y1 = %d" y1

        let y2 = x1 + 1

        // 'v' has been incremented - because `x1` is now evaluated!
        if v <> 2 then failwithf "no way Julie, v = %d" v
        if y2 <> 5 then failwithf "no way Jose, x2 = %d, v = %d" x2 v

        5 

    let x4 : int = failwith "no way"
        