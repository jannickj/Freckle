[<AutoOpen>]
module Freckle.Sample
open LazyList
open FSharp.Helpers

[<AutoOpen>]
module Types =
    open System

    type TimeId = uint32
    type Ticks = int64

    type Time = 
        { Ticks : Ticks
          //Id    : TimeId
        }
        with static member time t = { Ticks = t } //; Id = 0u }
             static member origin = Time.time 0L
             static member ticks t = t.Ticks
             static member incId t tOld =
                match tOld with
                | tOld' when t = tOld'.Ticks -> { Ticks = t } //; Id = tOld'.Id + 1u }
                | _ -> Time.time t             
             static member toDateTime t = DateTime(Time.ticks t)
             static member max = { Ticks = Int64.MaxValue } //; Id = UInt32.MaxValue }
             static member realise t1 t2 = if t2.Ticks = 0L then t1 else t2
             override x.ToString() = sprintf "%A" x

    type Period = 
        { Finish    : Time
          Beginning : Time
        }

    type Sample<'a> = Period -> 'a


[<AutoOpen>]
module Core =
    module Sample =
        
        let inline pure' (a : 'a) : Sample<'a> = const' a

        let inline map (f : 'a -> 'b) (sample : Sample<'a>) : Sample<'b> = sample >> f

        let inline join (sample : Sample<Sample<'a>>) : Sample<'a> = fun p -> sample p p
    
        let inline bind (f : 'a -> Sample<'b>) (m : Sample<'a> ) : Sample<'b> =
            join ((map f) m)
        
        let period : Sample<Period> = id
        
        let beginning (p : Period) = p.Beginning
        
        let finish (p : Period) = p.Finish
        

[<AutoOpen>]
module ComputationalExpression =
    type Builder() =
        member inline this.Return(x : 'T) = Sample.pure' x
        member inline this.ReturnFrom(x) = x
        member inline this.Bind(ma, f) = Sample.bind f ma
        member inline this.Zero () = Sample.pure' ()
    
    let sample = Builder()

[<AutoOpen>]
module Sampling =

    let batch cout clock (Sample :  : List<'a> = undefined

//
//
//
//
//[<AutoOpen>]
//module Execution =
//    module Act =
//        open System
//        open System.Threading
//
//        let run (mailbox : Mailbox) (evtSource : EventSource) (now : Now) (act : Act<Feed<'s>>) : Async< Requirements * Feed<'s>> =
//            async {
//                let context = 
//                    { Mailbox = mailbox
//                      EventSource = evtSource
//                      Now = now
//                    }
//
//                let (Act frS) = act
//                let! (reg, feed) = frS context
//
//                return (reg,  feed)
//                }
//
//        let runRecursive (mailbox : Mailbox)  (act : 's -> Act<Feed<'s>>) (state : 's)  : Async<unit> = 
//            let await pushedLast reg =
//                async {
//                    if pushedLast then
//                        return false
//                    else
//                        match reg.NextPoll with
//                        | Some poll -> 
//                            do! Mailbox.awaitMailTimeout poll mailbox
//                            return false
//                        | _ -> 
//                            do! Mailbox.awaitMail mailbox
//                            return true    
//                }
//            
//            let folder (reg,past,s, pushedLast) =
//                async {
//                    let! wasPush = await pushedLast reg
//                    let! evtSource = Mailbox.receive mailbox
//                    let! currentTicks = Mailbox.currentTick mailbox
//                    let currentTime = EventSource.setTimeId currentTicks evtSource
//
//                    let now = { Current = currentTime; Past = past }
//                    let! (reg', feed) = run mailbox evtSource now (act s)
//                    let s' = Option.default' s <| Feed.tryHead feed
//
//                    return Async.Signal.Continue (reg', currentTime,  s', wasPush)
//                }
//
//            async {
//                let! currentTicks = Mailbox.currentTick mailbox
//                do! Async.recursion folder ({ NextPoll = Some 0L }, Time.time currentTicks, state, false)
//                    |> Async.Ignore
            }