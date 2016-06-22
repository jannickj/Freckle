namespace Freckle

open FSharp.Helpers
open LazyList

[<AutoOpen>]
module Types =
    open System
    
    type TimeId = uint64
    type Time = Time of TimeId * int64
        with static member origin = Time (0UL, 0L)
                static member time (Time (_,t)) = t
                static member toDateTime (Time (_,t)) = DateTime(t)
    type CurrentTime = CurrentTime of Time
        with static member beginning = CurrentTime (Time.origin)
    type Freck<'e> = 
        { Event : LazyList<Time * 'e> 
          Now   : CurrentTime
        }    

module Internal =
    module Freck = 
        open LazyList

        let inline updateEvent f (fr : Freck<'a>) : Freck<'b> =
            { Event = f fr.Event; Now = fr.Now }

        let inline setEvent l (fr : Freck<'a>) : Freck<'b> =
            { Event = l; Now = fr.Now }

        let inline combineMeta l frA frB =
            let now = if frA.Now > frB.Now then frA.Now else frB.Now
            { Event = l; Now = now }

        let inline setNow t fr : Freck<_> = { fr with Now = CurrentTime t }

        let inline toEvent (fr : Freck<_>) = fr.Event

        let inline toNow (fr : Freck<_>) = fr.Now

        let rec merge' l1 l2 : LazyList<Time *'e> =
            match (l1, l2) with
            | Nil, vs -> vs
            | us, Nil -> us
            | LazyList.Cons((ta,a), ps), LazyList.Cons((tb,_), _) when ta >= tb ->           
                let mergePs () = merge' ps l2
                LazyList.consDelayed (ta,a) mergePs
            | _, LazyList.Cons((tb,b), qs) ->           
                let mergeQs () = merge' l1 qs
                LazyList.consDelayed (tb,b) mergeQs            

        let tryHead fr =
            match (toEvent fr) with
            | LazyList.Cons ((_,h), _) -> Some h
            | _ -> None

        let push t e fr = setEvent (LazyList.consDelayed (t,e) (fun () -> toEvent fr)) fr

open Internal.Freck

[<AutoOpen>]
module Core =  
    module Freck =     
        let inline freck event now = { Event = event; Now = now }

        let inline empty<'e> : Freck<'e> = freck LazyList.empty CurrentTime.beginning    

        let inline pure' (a : 'a) : Freck<'a> = 
            freck (LazyList.ofList [(Time.origin, a)]) CurrentTime.beginning

        let inline map (f : 'a -> 'b) (fr : Freck<'a>) : Freck<'b> = 
            updateEvent (LazyList.map (fun (t,a) -> (t, f a))) fr

        let inline join (fr : Freck<Freck<'a>>) : Freck<'a> = 
            let f = LazyList.map (fun (t, cfr) -> LazyList.map (tuple t) (LazyList.map snd (toEvent cfr)))
                    >> LazyList.concat
            updateEvent f fr            
    
        let inline bind (f : 'a -> Freck<'b>) : Freck<'a> -> Freck<'b> =
            join << (map f) 
             
    

[<AutoOpen>]
module Transformation =
    module Freck = 
        let inline toList fr = LazyList.toList (toEvent fr)

        let inline ofList l = setEvent (LazyList.ofList (List.sortByDescending fst l)) Freck.empty

[<AutoOpen>]
module Merging = 
    module Freck =    
        
        let weave (f : Option<'a> -> 'b -> 'a) (frB : Freck<'b>) (frA : Freck<'a>) : Freck<'a> = 
            let rec inner lb la =
                match lb, la with
                | (Cons ((tb,_), _), Cons ((ta,va), rest)) when ta > tb -> LazyList.consDelayed (ta,va) (fun () -> inner lb rest)
                | (Cons ((tb,vb), rest), Cons ((_,va), _)) -> LazyList.consDelayed (tb,f (Some va) vb) (fun () -> inner rest la)
                | (Cons ((tb,vb), rest), Nil) -> LazyList.consDelayed (tb,f None vb) (fun () -> inner rest la)
                | Nil, _ -> la
            combineMeta (inner (toEvent frB) (toEvent frA)) frA frB

        let combine (frA : Freck<'a>) (frB : Freck<'a>) : Freck<'a> = 
            combineMeta (merge' (toEvent frB) (toEvent frB)) frA frB


[<AutoOpen>]
module Timing =
    open Internal
    module Freck =

        let now fr = Freck.map (fun a -> (fr.Now, a)) fr 
    
        let timeStamp (fr : Freck<'a>) : Freck<Time * 'a> = 
            updateEvent (LazyList.map (fun (t,a) -> (t,(t,a)))) fr

        let timeStampAsTicks fr =
            Freck.map (fun (t,a) -> (Time.time t,a)) (timeStamp fr)

        let dateTimed fr =
            Freck.map (fun (t,a) -> (Time.toDateTime t,a)) (timeStamp fr)


[<AutoOpen>]
module Filtering =
    module Freck =
        let filter (f : 'a -> bool) (fr : Freck<'a>) : Freck<'a> =
            let rec inner l =
                match l with
                | LazyList.Cons((t,a), rest) when f a -> LazyList.consDelayed (t,a) (fun () -> inner rest)
                | LazyList.Cons(_, rest) -> inner rest
                | Nil -> LazyList.empty
            updateEvent inner fr

        let choose (f : 'a -> 'b option): Freck<'a> -> Freck<'b> = 
            Freck.map f >> filter (Option.isSome) >> Freck.map Option.get

        let partition (f : 'a -> bool) (fr : Freck<'a>) : (Freck<'a> * Freck<'a>) = (filter f fr, filter (not << f) fr)   
    
        let takeWhile (f : 'a -> bool) (fr : Freck<'a>) : Freck<'a> = 
            let rec inner l () =
                match l with
                | LazyList.Cons((t,h), rest) when f h ->
                    LazyList.consDelayed (t,h) (inner rest)
                | _ -> LazyList.empty

            updateEvent (flip inner ()) fr

        let cutToNow fr =
            let (CurrentTime ct) = toNow fr
            Freck.timeStamp fr
            |> takeWhile (fun (t,_) -> t >= ct)
            |> Freck.map snd

[<AutoOpen>] 
module Folding =
    module Freck =
        
        let mapFoldNow (f : 's -> 'a -> ('s * 'b)) (state : 's) (fr : Freck<'a>) : (Freck<'s * 'b>) =
            let fr = Freck.cutToNow fr
            let (l', _) = Seq.mapFoldBack (fun (t,a) s -> let (s', b) = f s a in ((t, (s', b))), s') (toEvent fr) state 
            setEvent (LazyList.ofSeq l') fr
        
        let foldNow (f : 's -> 'a -> 's) (s : 's) (fr : Freck<'a>) : Freck<'s> =
            Freck.map fst <| mapFoldNow (fun s a -> let s' = f s a in (s', ())) s fr

[<AutoOpen>]
module Planning =
    module Freck =

        let planNow (fullFreck : Freck<Async<'a>>) : Async<Freck<'a>> =            
            let fr = Freck.cutToNow fullFreck
            let folder (t,ma) (newV) =
                async {
                    let! newV' = newV
                    let! a = ma
                    return LazyList.cons (t,a) newV'
                }
            let folded =  (Seq.foldBack folder (toEvent fr)) (async.Return LazyList.empty)
            Async.map (flip setEvent fr) folded
        
        let transitionNow (f : 's -> 'a -> Async<'s>) (state : 's) (allFreck : Freck<'a>)  : Async<Freck<'s>> =
            let fr = Freck.cutToNow allFreck
            let rec inner l =
                async {
                    match l with
                    | Cons((t,h), rest) ->
                        let! (s, l') = inner rest
                        let! s' = f s h
                        return (s', (consDelayed (t, s') (fun () -> l')))
                    | Nil -> return (state, LazyList.empty)
                }
            async {
                let! (_, l) = inner (toEvent fr)
                return setEvent l fr
        }

[<AutoOpen>]
module Pull =
    module Freck =

[<AutoOpen>]
module Execution =
    module Freck =
        open System
        open System.Threading

        let execute (fs : Freck<'e> -> 's -> Async<Freck<'s>>) (state : 's) (events : Async<'e>) : Async<unit> =
            let wait (arv : AutoResetEvent) = arv.WaitOne() |> ignore
            let release (arv : AutoResetEvent) = arv.Set() |> ignore

            let timeCalc (timeUpdate : AutoResetEvent) (timePrep : AutoResetEvent) timeid =
                async {
                    while true do
                        wait timeUpdate
                        timeid := !timeid + 1UL
                        release timePrep
                }

            let fetchTime (timeFetch : AutoResetEvent) (timeUpdate : AutoResetEvent) (timePrep : AutoResetEvent) timeid =
                async {
                    wait timeFetch
                    release timeUpdate
                    wait timePrep
                    let timeId = !timeid
                    release timeFetch
                    return Time (timeId, DateTime.UtcNow.Ticks)
                }

            let manyEvents currentTime (sema : AutoResetEvent) evts =
                async  {
                    while true do
                        let! evt = events
                        let! time = currentTime
                        evts := push time evt !evts
                        sema.Set() |> ignore
                }

            let folder currentTime (sema : AutoResetEvent) events (now,s) =
                async {
                    sema.WaitOne() |> ignore
                    let allevents = !events
                    let evts = allevents |> setNow now
                    let! frS = fs evts s
                    let optS = tryHead frS

                    let! newNow = currentTime

                    return Async.Signal.Continue (newNow, Option.default' s optS)
                }

            async {
                let timeFetch = new AutoResetEvent(true)
                let timeUpdate = new AutoResetEvent(false)
                let timePrep = new AutoResetEvent(false)
                let timeId = ref 0UL
                let! _ = timeCalc timeUpdate timePrep timeId |> Async.StartChild
                let currentTime = fetchTime timeFetch timeUpdate timePrep timeId
                let sema = new AutoResetEvent(false)
                let evts = ref Freck.empty
                let! now = currentTime
                let! _ = Async.StartChild (manyEvents currentTime sema evts)
                do! Async.recursion (folder currentTime sema evts) (now, state)
                    |> Async.Ignore
            }

[<AutoOpen>]
module ComputationalExpression =
    type FreckBuilder() =
        member inline this.Return(x : 'T) = Freck.pure' x
        member inline this.ReturnFrom(x) = x
        member inline this.Bind(ma, f) = Freck.bind f ma
        member inline this.Zero () = Freck.pure' ()

    let freckle = FreckBuilder()
        