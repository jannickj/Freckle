module Tests.Freckle.Freck
open Freckle
open FSharp.Helpers
open Freckle.Internal
open Xunit
open FsUnit.Xunit
open System.Threading

let time v = (Time (0UL,v))

let now = Now << time

[<Fact>]
let ``map is lazy`` () =
    let mutable isLazy = true
    let strict = lazy (isLazy <- false)
    let fr = Freck.ofList [(time 0L, strict); (time 1L, strict)]
             |> Freck.map (fun s -> s.Force())
    
    isLazy |> should equal true
    Assert.True(isLazy)


[<Fact>]
let ``filter removes elements not satisfying condition`` () =
    
    let l = [(time 0L, false); (time 1L, true)]
            |> Freck.ofList
            |> Freck.filter id
            |> Freck.toList

    l |> should equal [(time 1L, true)]


[<Fact>]
let ``cutNow discards all elements older than now`` () =
    let l = [(time 0L, ()); (time 1L, ()); (time 1L, ()); (time 2L, ())]
            |> Freck.ofList
            |> Freck.discardBefore (time 1L)
            |> Freck.toList            
            
    l |> should equal [ (time 2L, ())
                      ; (time 1L, ())
                      ; (time 1L, ())
                      ]


[<Fact>]
let ``mapFold both folds and maps`` () =    
    let l = List.map (fun t -> (time (int64 t), t)) [ 1 .. 4 ]
            |> Freck.ofList
            |> Freck.mapFold (now 2L) (fun s a -> (s + a, a - 1)) 0
            |> Freck.toList
  
    l |> should equal [(time 4L, (9, 3)); (time 3L, (5, 2)); (time 2L, (2, 1))]

[<Fact>]
let ``weave select the correct state when merging`` () =
    let lA = [(time 0L, (1, "1a"));(time 2L, (2, "2a"))] |> Freck.ofList
    let lB = [(time 1L, "1b");(time 2L, "2b");(time 3L, "3b")] |> Freck.ofList

    let lAB = Freck.weave (fun optA b -> ((Option.mapDefault 0 fst optA), b)) lB lA
              |> Freck.toList

    lAB |> should equal [ (time 3L, (2, "3b"))
                        ; (time 2L, (2, "2b"))
                        ; (time 2L, (2, "2a"))
                        ; (time 1L, (1, "1b"))
                        ; (time 0L, (1, "1a"))
                        ]

[<Fact>]
let ```transitionNow correctly transitions between async states`` () =
    let l = List.map (fun i -> (time (int64 i), i)) [ 1 .. 5 ]
            |> Freck.ofList
            |> Freck.transition (now 2L) (fun s a -> async { return s + a } ) 0
    let l' = Async.RunSynchronously l
             |> Freck.toList

    l' |> should equal [ (time 5L, 14)
                       ; (time 4L, 9)
                       ; (time 3L, 5)
                       ; (time 2L, 2)
                       ]