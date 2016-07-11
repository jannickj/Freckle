[<AutoOpen>]
module Freckle.Act
open Freckle.Feed.Internal
open LazyList
open FSharp.Helpers

[<AutoOpen>]
module Types =
    
    type Act<'a> = Sample<Async<'a>>

module Internal =

    let inline map f act p =
        async {
            let! a = act p
            return f a
        }

    let inline join act p =
        async {
            let! mact = act p
            return! mact p
        }

[<AutoOpen>]
module Core =
    module Act =
        open Internal
        
        let inline pure' (a : 'a) : Act<'a> = Sample.pure' (async.Return a)

        let inline map (f : 'a -> 'b) (act : Act<'a>) : Act<'b> = map f act

        let inline join (act : Act<Act<'a>>) : Act<'a> = join act
    
        let inline bind (f : 'a -> Act<'b>) (m : Act<'a> ) : Act<'b> =
            join ((map f) m)
                    
        let combine ma mb = 
            fun c ->
                async { 
                    let! () = ma c
                    let! b = mb c
                    return b 
                }


        let doNothing = pure' ()

        let ofAsync ma : Act<_> = Sample.pure' ma

        let startChild ma =
            fun c ->
                async {
                        let! a = (ma c) |> Async.StartChild
                        return Requirements.none, ofAsync a
                }

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