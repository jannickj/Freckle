﻿module FSharp.Freckle

open System
    
type Time = Time of int64
    with static member origin = Time 0L
type CurrentTime = CurrentTime of Time
    with static member beginning = CurrentTime (Time.origin)
type Freck<'e> = Freck of (LazyList<Time * 'e> * CurrentTime)

module Freck =  
    open LazyList
    open FSharp.Helpers
    open System.Threading

    let currentTime (Freck (_, CurrentTime ct)) = ct

    let toList (Freck (l, _)) = LazyList.toList l

    let empty<'e> : Freck<'e> = (Freck (LazyList.empty, CurrentTime.beginning))

    let rec private merge' l1 l2 : LazyList<Time *'e> =
        match (l1, l2) with
        | Nil, vs -> vs
        | us, Nil -> us
        | LazyList.Cons((ta,a), ps), LazyList.Cons((tb,_), _) when ta >= tb ->           
            let mergePs () = merge' ps l2
            LazyList.consDelayed (ta,a) mergePs
        | _, LazyList.Cons((tb,b), qs) ->           
            let mergeQs () = merge' l1 qs
            LazyList.consDelayed (tb,b) mergeQs

    let pure' (a : 'a) : Freck<'a> = 
        Freck <| (LazyList.ofList [(Time.origin, a)], CurrentTime.beginning)

    let map (f : 'a -> 'b) (Freck (fr,ct) : Freck<'a>) : Freck<'b> = 
        (fr |> LazyList.map (fun (t,a) -> (t, f a)), ct) |> Freck

    let filter (f : 'a -> bool) (Freck (l, ct) : Freck<'a>) : Freck<'a> =
        let rec inner l =
            match l with
            | LazyList.Cons((t,a), rest) when f a -> LazyList.consDelayed (t,a) (fun () -> inner rest)
            | LazyList.Cons(_, rest) -> inner rest
            | Nil -> LazyList.empty
        Freck (inner l, ct)

    let choose (f : 'a -> 'b option): Freck<'a> -> Freck<'b> = 
        map f >> filter (Option.isSome) >> map Option.get
        
    let timed (Freck (l,ct) : Freck<'a>) : Freck<Time * 'a> = 
        Freck <| (LazyList.map (fun (t,a) -> (t,(t,a))) l,ct)

    let ticks fr =
        map (fun (Time t,a) -> (t,a)) (timed fr)

    let dateTimed fr =
        map (fun (t,a) -> (DateTime(t),a)) (ticks fr)
    
    let join (Freck (fr,ct) : Freck<Freck<'a>>) : Freck<'a> = 
        LazyList.map (fun (t, Freck (cfr,_)) -> LazyList.map (tuple t) (LazyList.map snd cfr)) fr
        |> LazyList.concat
        |> (fun l -> (l, ct))
        |> Freck
            
    let partition (f : 'a -> bool) (fr : Freck<'a>) : (Freck<'a> * Freck<'a>) = (filter f fr, filter (not << f) fr)
    
    let bind (f : 'a -> Freck<'b>) : Freck<'a> -> Freck<'b> =
        join << (map f) 
                
    let listenTo<'a> (fr : Freck<obj>) : Freck<'a> = choose safeUnbox fr

    let weave (f : Option<'a> -> 'b -> 'a) (Freck (listb, ctb) : Freck<'b>) (Freck (lista, cta) : Freck<'a>) : Freck<'a> = 
        let rec inner lb la =
            match lb, la with
            | (Cons ((tb,_), _), Cons ((ta,va), rest)) when ta > tb -> LazyList.consDelayed (ta,va) (fun () -> inner lb rest)
            | (Cons ((tb,vb), rest), Cons ((_,va), _)) -> LazyList.consDelayed (tb,f (Some va) vb) (fun () -> inner rest la)
            | (Cons ((tb,vb), rest), Nil) -> LazyList.consDelayed (tb,f None vb) (fun () -> inner rest la)
            | Nil, _ -> la
        let ct = if cta > ctb then cta else ctb
        Freck (inner listb lista, ct)

    let combine (Freck (lA,cta) : Freck<'a>) (Freck (lB,ctb) : Freck<'a>) : Freck<'a> = 
        let ct' = if cta > ctb then cta else ctb
        Freck (merge' lA lB, ct')
    
    let takeWhile (f : 'a -> bool) (Freck (list,ct) : Freck<'a>) : Freck<'a> = 
        let rec inner l () =
            match l with
            | LazyList.Cons((t,h), rest) when f h ->
                LazyList.consDelayed (t,h) (inner rest)
            | _ -> LazyList.empty

        Freck (inner list (), ct)

    let cutToNow fr =
        let ct = currentTime fr
        timed fr
        |> takeWhile (fun (t,_) -> t >= ct)
        |> map snd


    let mapFoldNow (f : 's -> 'a -> ('s * 'b)) (state : 's) (fr : Freck<'a>) : (Freck<'s * 'b>) =
        let (Freck (l, ct)) = cutToNow fr
        let (l', _) = Seq.mapFoldBack (fun (t,a) s -> let (s', b) = f s a in ((t, (s', b))), s') l state 
        Freck <| (LazyList.ofSeq l', ct)

    let planNow (fr : Freck<Async<'a>>) : Async<Freck<'a>> =            
        let (Freck (l, ct)) = cutToNow fr
        let folder (t,ma) (newV) =
            async {
                let! newV' = newV
                let! a = ma
                return LazyList.cons (t,a) newV'
            }
        Async.map (Freck << (fun l -> (l, ct))) <| (Seq.foldBack folder l) (async.Return LazyList.empty)
        
    let transitionNow (f : 's -> 'a -> Async<'s>) (state : 's) (fr : Freck<'a>)  : Async<Freck<'s>> =
        let (Freck (list,ct)) = cutToNow fr
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
            let! (_, l) = inner list
            return Freck (l, ct)
        }

    let ofList l = Freck (LazyList.ofList (List.sortByDescending fst l), CurrentTime (Time 0L))

    let foldNow (f : 's -> 'a -> 's) (s : 's) (fr : Freck<'a>) : Freck<'s> =
        map fst <| mapFoldNow (fun s a -> let s' = f s a in (s', ())) s fr

    let tryHead (Freck (l, _)) =
        match l with
        | LazyList.Cons ((_,h), _) -> Some h
        | _ -> None

    let push t e (Freck (l, ct)) = Freck (LazyList.consDelayed (t,e) (fun () -> l), ct)
    
    let rec pushMany l fr =
        match List.sortBy fst l with
        | (t, e)::rest -> pushMany rest (push t e fr)
        | [] -> fr

    let setNow t (Freck (l, _)) = Freck (l, CurrentTime t)
    
    let execute (fs : Freck<'e> -> 's -> Async<Freck<'s>>) (state : 's) (events : Async<'e>) : Async<unit> =
        let currentTime =
            async {
                return Time (DateTime.UtcNow.Ticks)
            }

        let manyEvents (sema : AutoResetEvent) evts =
            async  {
                while true do
                    let! evt = events
                    let! time = currentTime
                    evts := push time evt !evts
                    sema.Set() |> ignore
            }
        let folder (sema : AutoResetEvent) events (now,s) =
            async {
                do! Async.Sleep 1
                sema.WaitOne() |> ignore
                let allevents = !events
                let evts =  setNow now allevents

                let! frS = fs evts s
                let optS = tryHead frS

                let! newNow = currentTime

                return Async.Continue (newNow, Option.default' s optS)
            }
        async {
            let sema = new AutoResetEvent(false)
            let evts = ref empty
            let! now = currentTime
            let! _ = Async.StartChild (manyEvents sema evts)
            do! Async.recursion (folder sema evts) (now, state)
                |> Async.Ignore
        }

open Freck
type FreckBuilder() =
    member inline this.Return(x : 'T) = pure' x
    member inline this.ReturnFrom(x) = x
    member inline this.Bind(ma, f) = bind f ma
    member inline this.Zero () = pure' ()

let freckle = FreckBuilder()
        