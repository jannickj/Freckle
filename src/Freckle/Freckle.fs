namespace FSharp

module Freckle =

    open System
    
    type Time = Time of int64
        with static member origin = Time 0L
    type CurrentTime = CurrentTime of Time
        with static member beginning = CurrentTime (Time.origin)
    type Freck<'e> = Freck of (LazyList<Time * 'e> * CurrentTime)

    module Freck =  
        open LazyList
        open FSharp.Helpers
        let currentTime (Freck (_, CurrentTime ct)) = ct

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

        let merge (f : 'a -> 'b -> 'a) (fb : Freck<'b>) (fa : Freck<'a>) : Freck<'a> = undefined

        let combine (Freck (lA,cta) : Freck<'a>) (Freck (lB,ctb) : Freck<'a>) : Freck<'a> = 
            let ct' = if cta > ctb then cta else ctb
            Freck (merge' lA lB, ct')
    
        let until (f : 'a -> bool) (Freck (l,ct) : Freck<'a>) : Freck<'a> = 
            let rec inner l () =
                match l with
                | LazyList.Cons((t,h), rest) when f h -> LazyList.consDelayed (t,h) (inner rest)
                | _ -> LazyList.empty

            Freck (inner l (), ct)

        let private cutToNow fr =
            let ct = currentTime fr
            timed fr
            |> until (fun (t,_) -> t < ct)
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
        
        let transitionNow (f : 's -> 'a -> Async<'s>) (state : 's) (fr : Freck<'a>)  : Async<Freck<'s>> = undefined

        let foldNow (f : 's -> 'a -> 's) (s : 's) (fr : Freck<'a>) : Freck<'s> =
            map fst <| mapFoldNow (fun s a -> let s' = f s a in (s', ())) s fr

//        let update  (fr : Freck<'a>) : Freck<'s -> Async<'s>> =
//            map (flip f) fr
                

        let tryHead (Freck (l, _)) =
            match l with
            | LazyList.Cons ((_,h), _) -> Some h
            | _ -> None

        let push t e (Freck (l, ct)) = Freck (LazyList.consDelayed (t,e) (fun () -> l), ct)
        
        let private setNow t (Freck (l, _)) = Freck (l, CurrentTime t)

        let execute (fs : Freck<'e> -> 's -> Async<Freck<'s>>) (state : 's) (timer : Async<Time>) (events : Async<'e>) : Async<unit> =
            let manyEvents evts =
                async  {                    
                    let! evt = events
                    let! time = timer
                    return Async.Continue <| push time evt evts 
                }
            
            let folder (evts, s) =
                async {
                    use source = new System.Threading.CancellationTokenSource()

                    let! now = timer

                    let! childrenEvts = Async.StartChild (Async.recursionWithCancel source.Token manyEvents evts)
                                    
                    let! frS = fs evts s
                    let optS = tryHead frS

                    source.Cancel()
                    let! evts' = childrenEvts
                    
                    let evts'' = setNow now evts'
                    
                    return Async.Continue (evts'', Option.default' s optS)
                }
            
            Async.recursion folder (empty, state)
            |> Async.Ignore

    open Freck
    type FreckBuilder() =
        member inline this.Return(x : 'T) = pure' x
        member inline this.ReturnFrom(x) = x
        member inline this.Bind(ma, f) = bind f ma
        member inline this.Zero () = pure' ()

    let freckle = FreckBuilder()
        