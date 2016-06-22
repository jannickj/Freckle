module Freckle.Freck.Debug
    open Freckle

    let trace fr = Freck.map (fun (t,v) -> printfn "%A: %A" t v; v) (Freck.timeStamp fr)
