open Freckle

[<EntryPoint>]
let main argv = 
    let tt  () :  Reactive<unit, int, int,int> =        
        Reactive.event
        |> Reactive.filter ((=) ())
        |> Reactive.map (const' 2)        
        |> Reactive.foldp (+) 0

    let evts = [((), 1);((), 2);((), 3)]
    let res = tt () (Events evts) 0
    printfn "%A" argv
    0 // return an integer exit code
