﻿[<AutoOpen>]
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
        { NextPoll : Ticks option
        }
        with override x.ToString() = sprintf "%A" x

    type Act<'e> = Act of (Context -> (Async<Requirements * 'e>))

[<AutoOpen>]
module Support =
    module Requirements =
        let none = { NextPoll = None }
        let nextPoll t = { none with NextPoll = Some t }

module Internal =
    let inline inner (Act a) = a

    let context = Act (fun c -> async.Return (Requirements.none, c))
    
    let combinePoll p1 p2 = 
        match p1, p2 with
        | Some p1', Some p2' -> if p1' < p2' then p1 else p2
        | Some _, None -> p1
        | None, Some _ -> p2
        | None, None -> None

    let combineReq r1 r2 = { NextPoll = combinePoll r1.NextPoll r2.NextPoll }

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
            Act (fun _ -> async.Return (Requirements.none,  a))

        let inline map (f : 'a -> 'b) (act : Act<'a>) : Act<'b> = 
            Act (Internal.map f (Internal.inner act))

        let inline join (act : Act<Act<'a>>) : Act<'a> = 
            let act' = Internal.inner (map Internal.inner act)
            Act (Internal.join act')
    
        let inline bind (f : 'a -> Act<'b>) (m : Act<'a> ) : Act<'b> =
            join ((map f) m)

        let now = Act (fun c -> async { return Requirements.none, c.Now })
        
        let combine (Act ma) (Act mb) = 
            Act (fun c ->
                    async { 
                        let! ra, () = ma c
                        let! rb, b = mb c
                        return (Internal.combineReq ra rb, b) 
                    })


        let doNothing = pure' ()

        let require r = Act(fun _ -> async { return r, () })

        let ofAsync ma = 
            Act (fun _ -> 
                    async {
                        let! a = ma
                        return Requirements.none, a
                    })

        let startChild (Act ma) =
            Act (fun c ->
                async {
                        let! a = (ma c) |> Async.StartChild
                        return Requirements.none, ofAsync a
                    })

[<AutoOpen>]
module ComputationalExpression =
    type Builder() =
        member inline this.Return(x : 'T) = Act.pure' x
        member inline this.ReturnFrom(x) = x
        member inline this.Bind(ma, f) = Act.bind f ma
        member inline this.Zero () = Act.pure' ()
        member this.Combine (a,b) = Act.combine a b
    let act = Builder()


[<AutoOpen>]
module Planning =
    module Feed =
        open Feed.Internal
        let planNow (fullFeed : Feed<Act<'a>>) : Act<Feed<'a>> =
            act {
                let! now = Act.now
                let fr = Feed.betweenNow now fullFeed
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
                let fr =  Feed.betweenNow now allFeed
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
                let rec inner dist time ()  = 
                    if time.Ticks < dist 
                    then LazyList.empty 
                    else LazyList.consDelayed (time, time) (inner dist (Time.time (time.Ticks - dist)))
                let ticks = Time.ticks time
                let tps = TimeSpan.TicksPerSecond
                let pulseDistance = tps / (int64 ticksPerSecond)
                let ticksSincePulse = (ticks % pulseDistance)
                let lastPulse = ticks - ticksSincePulse
                do! Act.require (Requirements.nextPoll (pulseDistance - ticksSincePulse))
                return Feed.feed (LazyList.delayed (inner pulseDistance (Time.time lastPulse)))
            }

        let react<'e> : Act<Feed<'e>> = 
            act {
                let! c = Internal.context
                return EventSource.read c.EventSource
            }

[<AutoOpen>]
module Execution =
    module Act =
        open System
        open System.Threading

        let run (mailbox : Mailbox) (evtSource : EventSource) (now : Now) (act : Act<Feed<'s>>) : Async< Requirements * Feed<'s>> =
            async {
                let context = 
                    { Mailbox = mailbox
                      EventSource = evtSource
                      Now = now
                    }

                let (Act frS) = act
                let! (reg, feed) = frS context

                return (reg,  feed)
                }

        let runRecursive (mailbox : Mailbox)  (act : 's -> Act<Feed<'s>>) (state : 's)  : Async<unit> = 
            let await pushedLast reg =
                async {
                    if pushedLast then
                        return false
                    else
                        match reg.NextPoll with
                        | Some poll -> 
                            do! Mailbox.awaitMailTimeout poll mailbox
                            return false
                        | _ -> 
                            do! Mailbox.awaitMail mailbox
                            return true    
                }
            
            let folder (reg,past,s, pushedLast) =
                async {
                    let! wasPush = await pushedLast reg
                    let! evtSource = Mailbox.receive mailbox
                    let! currentTicks = Mailbox.currentTick mailbox
                    let currentTime = EventSource.setTimeId currentTicks evtSource

                    let now = { Current = currentTime; Past = past }
                    let! (reg', feed) = run mailbox evtSource now (act s)
                    let s' = Option.default' s <| Feed.tryHead feed

                    return Async.Signal.Continue (reg', currentTime,  s', wasPush)
                }

            async {
                let! currentTicks = Mailbox.currentTick mailbox
                do! Async.recursion folder ({ NextPoll = Some 0L }, Time.time currentTicks, state, false)
                    |> Async.Ignore
            }