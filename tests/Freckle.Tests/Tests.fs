module Tests
open FSharp.Helpers
open Freckle
open Xunit
open FsUnit.Xunit
open System.Threading
open System
open FsCheck

let time v = Time.time v

let now vfrom vto = { Finish = Time.time vto ; Beginning = Time.time vfrom }

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
            |> Feed.discardOlderExcl (time 1L)
            |> Feed.toList            
            
    l |> should equal [ (time 2L, ())
                        ; (time 1L, ())
                        ; (time 1L, ())
                        ]


[<Fact>]
let ``mapFold both folds and maps`` () =    
    let l = List.map (fun t -> (time (int64 t), t)) [ 1 .. 3 ]
            |> Feed.ofList
            |> Feed.mapScanPast (fun s a -> (s + a, a - 1)) 0
            |> Sample.realise (now 1L 3L)
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

//[<Fact>]
//let ``transitionNow correctly transitions between async states`` () =
//    async {
//        let! mb = Mailbox.create (Clock.alwaysAt 0L)
//        let! evtSource = Mailbox.receive mb
//        let! _, l = List.map (fun i -> (time (int64 i), i)) [ 1 .. 5 ]
//                    |> Feed.ofList
//                    |> Feed.transitionNow (fun s a -> act { return s + a } ) 0
//                    |> Act.run mb evtSource ({ Current = Time.time 4L; Past = Time.time 1L })
//        let l' = Feed.toList l
//        l' |> should equal [ (time 4L, 9)
//                           ; (time 3L, 5)
//                           ; (time 2L, 2)
//                           ]
//        return ()   
//    } |> Async.StartAsTask
//
//[<Fact>]
//let ``pulse gives correct pulses`` () =
//    async {
//        let! mb = Mailbox.create (Clock.alwaysAt 0L)
//        let! evtSource = Mailbox.receive mb
//        let! reg, l = Act.pulse 5u
//                    |> Act.run mb evtSource ({ Current = Time.time TimeSpan.TicksPerSecond; Past = Time.time 0L })
//        let l' = Feed.toList l
//        let dist = (TimeSpan.TicksPerSecond / 5L)
//        l' |> should equal [ (time (dist * 5L), time (dist * 5L))
//                           ; (time (dist * 4L), time (dist * 4L))
//                           ; (time (dist * 3L), time (dist * 3L))
//                           ; (time (dist * 2L), time (dist * 2L))
//                           ; (time (dist * 1L), time (dist * 1L))
//                           ]
//        reg.NextPoll |> should equal (Some dist)
//        return ()   
//    } |> Async.StartAsTask

let pretty fr = fr |> Feed.toList |> List.map (fun (t, a) -> t.Ticks, a)

let toFeedData feed = 
    feed.Event
    |> Feed.Internal.sortSameTime
    |> LazyList.toList
    |> List.map (fun (t, ab) -> (t.Ticks, ab))

open Feed.Operator


[<Fact>]
let ``feed join`` () = 
    let lA =  [(time 0L, "1a"); (time 3L, "2aX");(time 3L, "2aY")] |> Feed.ofList    
    let lB = [(time 1L, "1b");(time 3L, "2bX");(time 3L, "2bY");(time 5L, "3b")] |> Feed.ofList
    let lAB = [(time 3L, lB); (time 3L, lA)] |> Feed.ofList

    let joinX = Feed.join lAB
    let expected = [(Time.time 5L, "3b"); (Time.time 3L, "2aY"); (Time.time 3L, "2aX"); (Time.time 3L, "2bY"); (Time.time 3L, "2bX")] |> Feed.ofList
    
    should equal  (toFeedData joinX) (toFeedData expected)



[<Fact>]
let ``feed monad`` () = 
    let f x = [(time 1L, "f1" + x);(time 3L, "f2" + x)] |> Feed.ofList
    let g y = [(time 2L, "g1" + y);(time 4L, "g2" + y)] |> Feed.ofList
    let m = [(time 55L, "a"); (time 13L, "b")] |> Feed.ofList
 
    let lA =  [(time 0L, "1a"); (time 3L, "2aX");(time 3L, "2aY")] |> Feed.ofList
    
    let lB = [(time 1L, "1b");(time 3L, "2bX");(time 3L, "2bY");(time 5L, "3b")] |> Feed.ofList
        
    let lC = [(time 0L, "1c");(time 2L, "2c");(time 6L, "3c")] |> Feed.ofList


    let expectedABC = [ (6L, ("2aY" + "3b"  + "3c"))
                        (6L, ("2aX" + "3b"  + "3c"))
                        (5L, ("2aY" + "3b"  + "2c"))
                        (5L, ("2aX" + "3b"  + "2c"))
                        (3L, ("2aY" + "2bY" + "2c"))
                        (3L, ("2aY" + "2bX" + "2c"))
                        (3L, ("2aX" + "2bX" + "2c"))  
                        (2L, ("1a"  + "1b"  + "2c"))
                        (1L, ("1a"  + "1b"  + "1c"))
                      ]


    let mX = 
        feed {
            let! x = lA
            let! y = lB
            let! z = lC
            if x = "2aX" && y = "2bY"
            then return! Feed.empty
            else return (x + y + z) 
        }
    
    let mY = 
        feed {
            let! z = lC
            let! x = lA
            let! y = lB
            if x = "2aX" && y = "2bY"
            then return! Feed.empty
            else return (x + y + z) 
        }
    
    should equal  (toFeedData mX) (toFeedData mY)
    should equal  (toFeedData mX) expectedABC 


type Bind = Bind of (int -> Feed<int>)
type Value = Value of int
type Category = Category of Feed<int>

open FsCheck.Xunit

let futureGen =
    gen {
        let! a = Gen.choose(1, 100)
        let! t = Gen.choose(1, 100)
        return (Time.time (int64 t), a)
    }

let feedGen =
    gen {
        let! futures = futureGen
                        |> Gen.listOf
        return Feed.ofList futures
    }

let feedFunctionGen = 
    gen {
        let f1 a = Feed.pure' a
        let f2 a = Feed.empty
        let f3 a = [(time 1L, 1);(time 2L, a)] |> Feed.ofList
        let! f = feedGen
        let f4 a = f
        return! Gen.elements([f1; f2; f3; f4])
    }


type Generators =
    static member Category() = Arb.fromGen (feedGen |> Gen.map Category)
    static member Value() = Arb.fromGen (Gen.choose(1, 100) |> Gen.map Value)
    static member Bind() = Arb.fromGen (feedFunctionGen |> Gen.map Bind)


[<Arbitrary(typeof<Generators>)>]
module ``Feed monad laws`` =
    open Feed.Operator

    [<Property>]
    let ``left identity`` (Bind f) (Value a) =
        let feed1 = Feed.pure' a >>= f |> toFeedData
        let feed2 = f a                |> toFeedData
        
        should equal feed1 feed2


    [<Property>]
    let ``right identity`` (Category m) =
        let feed1 = m >>= Feed.pure' |> toFeedData
        let feed2 = m                |> toFeedData

        should equal feed1 feed2


    [<Property>]
    let ``associativity`` (Bind f) (Bind g) (Category m) =
        let feed1 = (m >>= f) >>= g            |> toFeedData
        let feed2 = m >>= (fun x -> f x >>= g) |> toFeedData

        should equal  feed1 feed2

