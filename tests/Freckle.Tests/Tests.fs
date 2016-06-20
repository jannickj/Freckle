module Tests.Freckle.Freck
open FSharp.Freckle
open FSharp.Helpers
open Xunit
open FsUnit.Xunit
open System.Threading

[<Fact>]
let ``map is lazy`` () =
    let mutable isLazy = true
    let strict = lazy (isLazy <- false)
    let fr = Freck.ofList [(Time 0L, strict); (Time 1L, strict)]
             |> Freck.map (fun s -> s.Force())
    
    isLazy |> should equal true
    Assert.True(isLazy)


[<Fact>]
let ``filter removes elements not satisfying condition`` () =
    
    let l = [(Time 0L, false); (Time 1L, true)]
            |> Freck.ofList
            |> Freck.filter id
            |> Freck.toList

    l |> should equal [(Time 1L, true)]


[<Fact>]
let ``cutNow discards all elements older than now`` () =
    let l = [(Time 0L, ()); (Time 1L, ()); (Time 1L, ()); (Time 2L, ())]
            |> Freck.ofList
            |> Freck.setNow (Time 1L)
            |> Freck.cutToNow
            |> Freck.toList            
            
    l |> should equal [ (Time 2L, ())
                      ; (Time 1L, ())
                      ; (Time 1L, ())
                      ]


[<Fact>]
let ``mapFold both folds and maps`` () =    
    let l = List.map (fun t -> (Time (int64 t), t)) [ 1 .. 4 ]
            |> Freck.ofList
            |> Freck.setNow (Time 2L)
            |> Freck.mapFoldNow (fun s a -> (s + a, a - 1)) 0
            |> Freck.toList
  
    l |> should equal [(Time 4L, (9, 3)); (Time 3L, (5, 2)); (Time 2L, (2, 1))]

[<Fact>]
let ``weave select the correct state when merging`` () =
    let lA = [(Time 0L, (1, "1a"));(Time 2L, (2, "2a"))] |> Freck.ofList
    let lB = [(Time 1L, "1b");(Time 2L, "2b");(Time 3L, "3b")] |> Freck.ofList

    let lAB = Freck.weave (fun optA b -> ((Option.mapDefault 0 fst optA), b)) lB lA
              |> Freck.toList

    lAB |> should equal [ (Time 3L, (2, "3b"))
                        ; (Time 2L, (2, "2b"))
                        ; (Time 2L, (2, "2a"))
                        ; (Time 1L, (1, "1b"))
                        ; (Time 0L, (1, "1a"))
                        ]

[<Fact>]
let ```transitionNow correctly transitions between async states`` () =
    let l = List.map (fun i -> (Time (int64 i), i)) [ 1 .. 5 ]
            |> Freck.ofList
            |> Freck.setNow (Time 2L)
            |> Freck.transitionNow (fun s a -> async { return s + a } ) 0
    let l' = Async.RunSynchronously l
             |> Freck.toList

    l' |> should equal [ (Time 5L, 14)
                       ; (Time 4L, 9)
                       ; (Time 3L, 5)
                       ; (Time 2L, 2)
                       ]