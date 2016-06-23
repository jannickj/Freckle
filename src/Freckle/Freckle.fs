namespace Freckle

open FSharp.Helpers
open LazyList

[<AutoOpen>]
module Types =
    open System

    type TimeId = uint64
    type Time = 
            { Ticks : int64
              Id    : uint32
            }
        with static member time t = { Ticks = t; Id = 0u }
             static member origin = Time.time 0L
             static member ticks t = t.Ticks
             static member incId t tOld = { t with Id = tOld.Id + 1u }
             
             static member toDateTime t = DateTime(Time.ticks t)

    type Now = 
        { Current : Time
          Past : Time
        }
        with static member beginning = { Current = Time.origin; Past = Time.origin }

    type Freck<'e> =
        { Event : LazyList<Time * 'e>
        }    

module Internal =
    module Freck = 
        open LazyList

        let inline updateEvent f (fr : Freck<'a>) : Freck<'b> =
            { Event = f fr.Event  }

        let inline setEvent l (_ : Freck<'a>) : Freck<'b> =
            { Event = l }

        let inline combineMeta l _ _ =
            { Event = l }
            
        let inline toEvent (fr : Freck<_>) = fr.Event
        
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
                            
open Internal.Freck

[<AutoOpen>]
module Core =  
    module Freck =     
        let inline freck event = { Event = event }

        let inline empty<'e> : Freck<'e> = freck LazyList.empty 

        let inline pure' (a : 'a) : Freck<'a> = 
            freck (LazyList.ofList [(Time.origin, a)])

        let inline map (f : 'a -> 'b) (fr : Freck<'a>) : Freck<'b> = 
            updateEvent (LazyList.map (fun (t,a) -> (t, f a))) fr

        let inline join (fr : Freck<Freck<'a>>) : Freck<'a> = 
            let f = LazyList.map (fun (t, cfr) -> LazyList.map (tuple t) (LazyList.map snd (toEvent cfr)))
                    >> LazyList.concat
            updateEvent f fr            
    
        let inline bind (f : 'a -> Freck<'b>) : Freck<'a> -> Freck<'b> =
            join << (map f) 
             
        let inline stick (a : 'a) (fr : Freck<'b>) : Freck<'a * 'b> = map (tuple a) fr 

        let inline push t e fr = 
            let rec inner l () =
                match l with
                | LazyList.Cons((t', e'), rest) when Time.ticks t' > Time.ticks t -> 
                    LazyList.consDelayed (t', e') (inner rest)
                | LazyList.Cons((t', _), _) when Time.ticks t' = Time.ticks t ->
                    LazyList.cons (Time.incId t t', e) l
                | other ->
                    LazyList.cons (t, e) other

            updateEvent (flip inner ()) fr
            
        let tryHead fr =
            match (toEvent fr) with
            | LazyList.Cons ((_,h), _) -> Some h
            | _ -> None    

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
        
        let time (fr : Freck<'a>) : Freck<Time> = 
            updateEvent (LazyList.map (fun (t,_) -> (t,t))) fr

        let timeStamp (fr : Freck<'a>) : Freck<Time * 'a> = 
            updateEvent (LazyList.map (fun (t,a) -> (t,(t,a)))) fr

        let timeStampAsTicks fr =
            Freck.map (fun (t,a) -> (Time.ticks t,a)) (timeStamp fr)

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


        let skipWhile (f : 'a -> bool) (fr : Freck<'a>) : Freck<'a> = 
            let rec inner l () =
                match l with
                | LazyList.Cons((_,h), rest) when f h ->
                    LazyList.delayed (inner rest)
                | _ -> l
            updateEvent (flip inner ()) fr


        let discardBefore time fr =
            Freck.timeStamp fr
            |> takeWhile (fun (t,_) -> t >= time)
            |> Freck.map snd

[<AutoOpen>]
module Grouping =
    module Freck =

        let span (f : 'a -> bool) (fr : Freck<'a>) : (Freck<'a> * Freck<'a>) = 
            (Freck.takeWhile f fr, Freck.skipWhile f fr)
                        

[<AutoOpen>] 
module Folding =
    module Freck =
        
        let mapFold (now : Now) (f : 's -> 'a -> ('s * 'b)) (state : 's) (fr : Freck<'a>) : (Freck<'s * 'b>) =
            let fr = Freck.discardBefore now.Past fr
            let (l', _) = Seq.mapFoldBack (fun (t,a) s -> let (s', b) = f s a in ((t, (s', b))), s') (toEvent fr) state 
            setEvent (LazyList.ofSeq l') fr
        
        let fold now (f : 's -> 'a -> 's) (s : 's) (fr : Freck<'a>) : Freck<'s> =
            Freck.map fst <| mapFold now (fun s a -> let s' = f s a in (s', ())) s fr

[<AutoOpen>]
module Planning =
    module Freck =

        let plan (now : Now) (fullFreck : Freck<Async<'a>>) : Async<Freck<'a>> =            
            let fr = Freck.discardBefore now.Past fullFreck
            let folder (t,ma) (newV) =
                async {
                    let! newV' = newV
                    let! a = ma
                    return LazyList.cons (t,a) newV'
                }
            let folded =  (Seq.foldBack folder (toEvent fr)) (async.Return LazyList.empty)
            Async.map (flip setEvent fr) folded
        
        let transition (now : Now) (f : 's -> 'a -> Async<'s>) (state : 's) (allFreck : Freck<'a>)  : Async<Freck<'s>> =
            let fr = Freck.discardBefore now.Past allFreck
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
module Signal =
    module Freck =
        open System

        let pulse ({ Current = time }) (hertz : uint32) : Freck<Time> =
            let rec inner dist time ()  = LazyList.consDelayed (time, time) (inner dist (Time.time (time.Ticks - dist)))
            let ticks = Time.ticks time
            let tps = TimeSpan.TicksPerSecond
            let pulseDistance = tps / (int64 hertz)
            let calc = ticks - (ticks % pulseDistance)
            Freck.freck (LazyList.delayed (inner pulseDistance (Time.time calc)))


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

[<AutoOpen>]
module ComputationalExpression =
    type FreckBuilder() =
        member inline this.Return(x : 'T) = Freck.pure' x
        member inline this.ReturnFrom(x) = x
        member inline this.Bind(ma, f) = Freck.bind f ma
        member inline this.Zero () = Freck.pure' ()

    let freckle = FreckBuilder()
        