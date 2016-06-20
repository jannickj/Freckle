module Tests.Freckle.Freck

open FSharp.Freckle
open Xunit

[<Fact>]
let ``map is lazy`` () =
    let mutable isLazy = true
    let strict = lazy (isLazy <- false)
    let fr = Freck.ofList [(Time 0L, strict); (Time 1L, strict)]
             |> Freck.map (fun s -> s.Force())

    Assert.True(isLazy)
