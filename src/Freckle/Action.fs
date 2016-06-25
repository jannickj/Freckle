namespace Freckle
open Freckle.Internal.Freck.Internal
open LazyList
open FSharp.Helpers

[<AutoOpen>]
module Types =
    
    type Context =
        { Mailbox : Mailbox
          Now : Now
          EventSource : Map<SortedType, Freck<obj>>
        }

    type Requirements =
        { NextPoll : int
        }

    type Reaction<'e> = Reaction of (Context -> (Async<Option<Requirements> * 'e>))

[<AutoOpen>]
module Core =
    module Reaction =
        module Internal =
            let inline inner (Reaction a) = a

            let context = Reaction (fun c -> async.Return (None, c))

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

        let inline pure' (a : 'a) : Reaction<'a> = 
            Reaction (fun _ -> async.Return (None,  a))

        let inline map (f : 'a -> 'b) (act : Reaction<'a>) : Reaction<'b> = 
            Reaction (Internal.map f (Internal.inner act))

        let inline join (act : Reaction<Reaction<'a>>) : Reaction<'a> = 
            let act' = Internal.inner (map Internal.inner act)
            Reaction (Internal.join act')
    
        let inline bind (f : 'a -> Reaction<'b>) (m : Reaction<'a> ) : Reaction<'b> =
            join ((map f) m)

        let now = Reaction (fun c -> async { return None, c.Now })

[<AutoOpen>]
module ComputationalExpression =
    type Builder() =
        member inline this.Return(x : 'T) = Reaction.pure' x
        member inline this.ReturnFrom(x) = x
        member inline this.Bind(ma, f) = Reaction.bind f ma
        member inline this.Zero () = Reaction.pure' ()

    let reaction = Builder()


[<AutoOpen>]
module Planning =
    module Freck =
        let planNow (fullFreck : Freck<Reaction<'a>>) : Reaction<Freck<'a>> =
            reaction {
                let! now = Reaction.now
                let fr = Freck.discardBefore now.Past fullFreck
                let folder (t,ma) (newV) =
                    reaction {
                        let! newV' = newV
                        let! a = ma
                        return LazyList.cons (t,a) newV'
                    }
                let! folded =  (Seq.foldBack folder (toEvent fr)) (Reaction.pure' LazyList.empty)
                return (flip setEvent fr) folded
            }
        
        let transitionNow (f : 's -> 'a -> Reaction<'s>) (state : 's) (allFreck : Freck<'a>)  : Reaction<Freck<'s>> =
            reaction {
                let! now = Reaction.now
                let fr = Freck.discardBefore now.Past allFreck
                let rec inner l =
                    reaction {
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
    module Reaction =
        open System

        let pulse (ticksPerSecond : uint32) : Reaction<Freck<Time>> =
            reaction {
                let! now = Reaction.now
                let { Current = time } = now
                let rec inner dist time ()  = LazyList.consDelayed (time, time) (inner dist (Time.time (time.Ticks - dist)))
                let ticks = Time.ticks time
                let tps = TimeSpan.TicksPerSecond
                let pulseDistance = tps / (int64 ticksPerSecond)
                let calc = ticks - (ticks % pulseDistance)
                return Freck.freck (LazyList.delayed (inner pulseDistance (Time.time calc)))
            }

        let listenTo<'e> : Reaction<Freck<'e>> = 
            reaction {
                let! c = Reaction.Internal.context
                let t = typeof<'e>
                let st = SortedType(t)
                match Map.tryFind st c.EventSource with
                | Some fr -> return Freck.map unbox fr
                | None -> return Freck.empty
            }

[<AutoOpen>]
module Execution =
    module Freck =
        open System
        open System.Threading

        let execute (fs : Now -> Freck<'e> -> 's -> Async<Freck<'s>>) (state : 's) (events : Async<'e>) : Async<unit> =            
            let fetchTime =
                async {
                    return Time.time DateTime.UtcNow.Ticks
                }

            let manyEvents currentTime (sema : AutoResetEvent) evts =
                async  {
                    while true do
                        let! evt = events
                        let! time = currentTime
                        evts := Freck.push time evt !evts
                        sema.Set() |> ignore
                }

            let folder currentTime (sema : AutoResetEvent) events (past,s) =
                async {
                    sema.WaitOne(1) |> ignore
                    let evts = !events                               

                    let! current = currentTime
                    
                    let current' = 
                        match Freck.tryHead (evts |> Freck.time) with
                        | Some t when t >= current -> Time.incId current t
                        | _ -> current

                    let now = { Current = current'; Past = past.Current }
                    
                    let! frS = fs now evts s
                    let s' = Option.default' s <| Freck.tryHead frS


                    return Async.Signal.Continue (now,  s')
                }

            async {
                let sema = new AutoResetEvent(false)
                let evts = ref Freck.empty
                let! currentTime = fetchTime
                let! _ = Async.StartChild (manyEvents fetchTime sema evts)
                do! Async.recursion (folder fetchTime sema evts) ({ Current = currentTime; Past = Time.origin }, state)
                    |> Async.Ignore
            }