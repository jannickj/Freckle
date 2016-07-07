module Tests.Freckle.Feed
open Freckle
open FSharp.Helpers
open Freckle.Feed.Internal
open Xunit
open FsUnit.Xunit
open System.Threading
open System
open FsCheck

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
let ``transitionNow correctly transitions between async states`` () =
    async {
        let! mb = Mailbox.create (Clock.alwaysAt 0L)
        let! evtSource = Mailbox.receive mb
        let! _, l = List.map (fun i -> (time (int64 i), i)) [ 1 .. 5 ]
                    |> Feed.ofList
                    |> Feed.transitionNow (fun s a -> act { return s + a } ) 0
                    |> Act.run mb evtSource ({ Current = Time.time 4L; Past = Time.time 2L })
        let l' = Feed.toList l
        l' |> should equal [ (time 5L, 14)
                           ; (time 4L, 9)
                           ; (time 3L, 5)
                           ; (time 2L, 2)
                           ]
        return ()   
    } |> Async.StartAsTask

[<Fact>]
let ``pulse gives correct pulses`` () =
    async {
        let! mb = Mailbox.create (Clock.alwaysAt 0L)
        let! evtSource = Mailbox.receive mb
        let! reg, l = Act.pulse 5u
                    |> Act.run mb evtSource ({ Current = Time.time TimeSpan.TicksPerSecond; Past = Time.time 0L })
        let l' = Feed.toList l
        let dist = (TimeSpan.TicksPerSecond / 5L)
        l' |> should equal [ (time (dist * 5L), time (dist * 5L))
                           ; (time (dist * 4L), time (dist * 4L))
                           ; (time (dist * 3L), time (dist * 3L))
                           ; (time (dist * 2L), time (dist * 2L))
                           ; (time (dist * 1L), time (dist * 1L))
                           ]
        reg.NextPoll |> should equal (Some dist)
        return ()   
    } |> Async.StartAsTask

let pretty fr = fr |> Feed.toList |> List.map (fun (t, a) -> t.Ticks, a)

[<Fact>]
let ``feed monad`` () = 
    let lA = [(time 1L, "1a"); (time 4L, "2a");(time 5L, "3a")] |> Feed.ofList
    let lB = [(time 2L, "1b");(time 4L, "2b");(time 6L, "3b")] |> Feed.ofList

//    let lAB = Feed.bind (fun a -> Feed.bind (fun b -> Feed.pure' (a, b)) lB) lA |> pretty
    let lBA = Feed.bind (fun b -> Feed.bind (fun a -> Feed.pure' (a, b)) lA) lB |> pretty

//    let lAB = feed {
//                let! a = lA
//                let! b = lB
//                return (a, b)
//              } |> pretty

    let lBA = feed {
                let! b = lB
                let! a = lA
                return (a, b)
              } |> pretty

    let expected = [ (6L, ("3a", "3b"))
                     (5L, ("3a", "2b"))
                     (4L, ("2a", "2b"))
                     (4L, ("2a", "1b"))
                     (2L, ("1a", "1b"))
                   ]

//    expected |> should equal lAB
    expected |> should equal lBA
    
[<Fact>]
let ``Feed monad`` () =    
    let lA = [(time 0L, "1a");(time 2L, "2a");(time 4L, "3a")] |> Feed.ofList
    let lB = [(time 1L, "1b");(time 3L, "2b");(time 5L, "3b")] |> Feed.ofList

    let lAB = 
        feed {
            let! a = lA
            let! b = lB
            return (a, b)
        } |> Feed.toList
          |> List.map (fun (t, ab) -> (t.Ticks, ab))
          

    should equal lAB []



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
                       |> Gen.listOfLength 2
        return Feed.ofList futures
    }

let feedFunctionGen = 
    gen {
        let f1 a = Feed.pure' a
        let f2 a = Feed.empty
        let f3 a = [(time 0L, 1);(time 2L, a)] |> Feed.ofList
        return! Gen.elements([f1; f2; f3])
    }


type Generators =
    static member Category() = Arb.fromGen (feedGen |> Gen.map Category)
    static member Value() = Arb.fromGen (Gen.choose(1, 100) |> Gen.map Value)
    static member Bind() = Arb.fromGen (feedFunctionGen |> Gen.map Bind)

let toFeedData feed = Feed.toList feed |> List.map (fun (t, ab) -> (t.Ticks, ab))

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


    [<Property(Replay="(1251986508,296175100)")>]
    let ``associativity`` (Bind f) (Bind g) (Category m) (Value x) (Value y) =
        let feed1 = (m >>= f) >>= g            |> toFeedData
        let feed2 = m >>= (fun x -> f x >>= g) |> toFeedData

        should equal feed1 feed2 
