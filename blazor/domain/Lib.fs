module domain = 
    let incr (n: int) =
        n+1

[<EntryPoint>]
let main args=
    let n = 1
    let incremented = domain.incr n
    printfn "%i incremented is %i" n incremented
    0