module Tests.Freckle.Feed
open Freckle
open FSharp.Helpers
open Freckle.Feed.Internal
open Xunit
open FsUnit.Xunit
open System.Threading

let time v = Time.time v

let nowWithPast v = { Current = Time.time System.Int64.MaxValue ; Past = Time.time v }

[<Fact>]
let ``map is lazy`` () =
    let mutable isLazy = true
    let strict = lazy (isLazy <- false)
    let fr = Feed.ofList [(time 0L, strict); (time 1L, strict)]
             |> Feed.map (fun s -> s.Force())
    
    isLazy |> should equal true
    Assert.True(isLazy)


[<Fact>]
let ``filter removes elements not satisfying condition`` () =
    
    let l = [(time 0L, false); (time 1L, true)]
            |> Feed.ofList
            |> Feed.filter id
            |> Feed.toList

    l |> should equal [(time 1L, true)]


[<Fact>]
let ``cutNow discards all elements older than now`` () =
    let l = [(time 0L, ()); (time 1L, ()); (time 1L, ()); (time 2L, ())]
            |> Feed.ofList
            |> Feed.discardBefore (time 1L)
            |> Feed.toList            
            
    l |> should equal [ (time 2L, ())
                      ; (time 1L, ())
                      ; (time 1L, ())
                      ]


[<Fact>]
let ``mapFold both folds and maps`` () =    
    let l = List.map (fun t -> (time (int64 t), t)) [ 1 .. 3 ]
            |> Feed.ofList
            |> Feed.mapFold (nowWithPast 2L) (fun s a -> (s + a, a - 1)) 0
            |> Feed.toList
  
    l |> should equal [ (time 3L, (5, 2))
                      ; (time 2L, (2, 1))]

[<Fact>]
let ``weave select the correct state when merging`` () =
    let lA = [(time 0L, (1, "1a"));(time 2L, (2, "2a"))] |> Feed.ofList
    let lB = [(time 1L, "1b");(time 2L, "2b");(time 3L, "3b")] |> Feed.ofList

    let lAB = Feed.weave (fun optA b -> ((Option.mapDefault 0 fst optA), b)) lB lA
              |> Feed.toList

    lAB |> should equal [ (time 3L, (2, "3b"))
                        ; (time 2L, (2, "2b"))
                        ; (time 2L, (2, "2a"))
                        ; (time 1L, (1, "1b"))
                        ; (time 0L, (1, "1a"))
                        ]

[<Fact>]
let ```transitionNow correctly transitions between async states`` () =
    async {
        let! mb = Mailbox.create (Clock.alwaysAt 0L)
        let! evtSource = Mailbox.receive mb
        let! l = List.map (fun i -> (time (int64 i), i)) [ 1 .. 5 ]
                 |> Feed.ofList
                 |> Feed.transitionNow (fun s a -> act { return s + a } ) 0
                 |> Act.run mb evtSource ({ Current = Time.time 4L; Past = Time.time 2L })
        
        l |> should equal [ (time 5L, 14)
                          ; (time 4L, 9)
                          ; (time 3L, 5)
                          ; (time 2L, 2)
                          ]
        return ()   
    }
    