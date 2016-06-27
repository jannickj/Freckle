[<AutoOpen>]
module Freckle.Act
open Freckle.Feed.Internal
open LazyList
open FSharp.Helpers

[<AutoOpen>]
module Types =
    
    type Context =
        { Mailbox : Mailbox
          Now : Now
          EventSource : EventSource
        }

    type Requirements =
        { NextPoll : Ticks
        }

    type Act<'e> = Act of (Context -> (Async<Option<Requirements> * 'e>))

module Internal =
    let inline inner (Act a) = a

    let context = Act (fun c -> async.Return (None, c))

    let combineReq r1 r2 =
        match r1, r2 with
        | Some r1, Some r2 -> Some { NextPoll = if r1.NextPoll < r2.NextPoll then r1.NextPoll else r2.NextPoll }
        | Some _, None -> r1
        | None, Some _ -> r2
        | None, None -> None

    let map f act c =
        async {
            let! (r, a) = act c
            return (r, f a)
        }

    let join act c =
        async {
            let! (r, mact) = act c
            let! (r', a) = mact c
            return (combineReq r r', a)
        }

[<AutoOpen>]
module Core =
    module Act =

        let inline pure' (a : 'a) : Act<'a> = 
            Act (fun _ -> async.Return (None,  a))

        let inline map (f : 'a -> 'b) (act : Act<'a>) : Act<'b> = 
            Act (Internal.map f (Internal.inner act))

        let inline join (act : Act<Act<'a>>) : Act<'a> = 
            let act' = Internal.inner (map Internal.inner act)
            Act (Internal.join act')
    
        let inline bind (f : 'a -> Act<'b>) (m : Act<'a> ) : Act<'b> =
            join ((map f) m)

        let now = Act (fun c -> async { return None, c.Now })

[<AutoOpen>]
module ComputationalExpression =
    type Builder() =
        member inline this.Return(x : 'T) = Act.pure' x
        member inline this.ReturnFrom(x) = x
        member inline this.Bind(ma, f) = Act.bind f ma
        member inline this.Zero () = Act.pure' ()

    let act = Builder()


[<AutoOpen>]
module Planning =
    module Feed =
        open Feed.Internal
        let planNow (fullFeed : Feed<Act<'a>>) : Act<Feed<'a>> =
            act {
                let! now = Act.now
                let fr = Feed.discardBefore now.Past fullFeed
                let folder (t,ma) (newV) =
                    act {
                        let! newV' = newV
                        let! a = ma
                        return LazyList.cons (t,a) newV'
                    }
                let! folded =  (Seq.foldBack folder (toEvent fr)) (Act.pure' LazyList.empty)
                return (flip setEvent fr) folded
            }
        
        let transitionNow (f : 's -> 'a -> Act<'s>) (state : 's) (allFeed : Feed<'a>)  : Act<Feed<'s>> =
            act {
                let! now = Act.now
                let fr = Feed.discardBefore now.Past allFeed
                let rec inner l =
                    act {
                        match l with
                        | Cons((t,h), rest) ->
                            let! (s, l') = inner rest
                            let! s' = f s h
                            return (s', (consDelayed (t, s') (fun () -> l')))
                        | Nil -> return (state, LazyList.empty)
                    }
                let! (_, l) = inner (toEvent fr)
                return setEvent l fr
            }

[<AutoOpen>]
module Signal =
    module Act =
        open System

        let pulse (ticksPerSecond : uint32) : Act<Feed<Time>> =
            act {
                let! now = Act.now
                let { Current = time } = now
                let rec inner dist time ()  = LazyList.consDelayed (time, time) (inner dist (Time.time (time.Ticks - dist)))
                let ticks = Time.ticks time
                let tps = TimeSpan.TicksPerSecond
                let pulseDistance = tps / (int64 ticksPerSecond)
                let calc = ticks - (ticks % pulseDistance)
                return Feed.feed (LazyList.delayed (inner pulseDistance (Time.time calc)))
            }

        let react<'e> : Act<Feed<'e>> = 
            act {
                let! c = Internal.context
                return EventSource.read c.EventSource
            }

[<AutoOpen>]
module Execution =
    module Feed =
        open System
        open System.Threading

        let execute' (mailbox : Mailbox)  (act : 's -> Act<Feed<'s>>) (state : 's)  : Async<unit> = 
            let folder (regOpt,past,s) =
                async {
                    match regOpt with
                    | Some reg -> do! Mailbox.awaitMailTimeout reg.NextPoll mailbox
                    | None -> do! Mailbox.awaitMail mailbox
                    
                    let! evtSource = Mailbox.receive mailbox
                    let! currentTicks = Mailbox.currentTick mailbox
                    let currentTime = EventSource.setTimeId currentTicks evtSource

                    let now = { Current = currentTime; Past = past }
                    let context = 
                        { Mailbox = mailbox
                          EventSource = evtSource
                          Now = now
                        }

                    let (Act frS) = act s
                    let! (regOpt', feed) = frS context
                    let s' = Option.default' s <| Feed.tryHead feed


                    return Async.Signal.Continue (regOpt', currentTime,  s')
                }

            async {
                let! currentTicks = Mailbox.currentTick mailbox
                do! Async.recursion folder (Some { NextPoll = 0L }, Time.time currentTicks, state)
                    |> Async.Ignore
            }