[<AutoOpen>]
module Freckle.Feed

open FSharp.Helpers
open LazyList

[<AutoOpen>]
module Types =
    open System

    type TimeId = uint32
    type Ticks = int64

    type Time = 
            { Ticks : Ticks
              Id    : TimeId
            }
        with static member time t = { Ticks = t; Id = 0u }
             static member origin = Time.time 0L
             static member ticks t = t.Ticks
             static member incId t tOld = 
                if t = tOld.Ticks 
                then { Ticks = t; Id = tOld.Id + 1u }
                else Time.time t
             
             static member toDateTime t = DateTime(Time.ticks t)
             override x.ToString() = sprintf "%A" x 

    type Now = 
        { Current : Time
          Past : Time
        }
        with static member beginning = { Current = Time.origin; Past = Time.origin }

    type Feed<'e> =
        { Event : LazyList<Time * 'e>
        }

[<AutoOpen>]
module Internal =
    module Feed = 
        module Internal =
            open LazyList
            
            let inline updateEvent f (fr : Feed<'a>) : Feed<'b> =
                { Event = f fr.Event  }
            
            let inline setEvent l (_ : Feed<'a>) : Feed<'b> =
                { Event = l }

            let inline combineMeta l _ _ =
                { Event = l }
            
            let inline toEvent (fr : Feed<_>) = fr.Event
        
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
            
            let rec join (ls : LazyList<Time * LazyList<Time * 'a>>) : LazyList<Time * 'a> =
                match ls with
                | Cons((ts, (Cons((t, _), _) as la)), rest) -> 
                    let tDiff = (ts.Ticks - t.Ticks)
                    let la' = map (fun (t,a) -> ({ t with Ticks = t.Ticks + tDiff}, a)) la
                    let ls' = delayed (fun () -> join rest)
                    delayed (fun () -> merge' la' ls')
                | Cons((_, Nil), rest) -> delayed (fun () -> join rest)
                | Nil -> LazyList.empty

            let inline unsafePush t e feed =
                updateEvent (LazyList.cons (t, e)) feed

open Feed.Internal

[<AutoOpen>]
module Core =  
    module Feed =     
        let inline feed event = { Event = event }

        let inline empty<'e> : Feed<'e> = feed LazyList.empty 

        let inline singleton time evt = feed (LazyList.cons (time, evt) LazyList.empty)

        let inline pure' (a : 'a) : Feed<'a> = 
            feed (LazyList.ofList [(Time.origin, a)])

        let inline map (f : 'a -> 'b) (fr : Feed<'a>) : Feed<'b> = 
            updateEvent (LazyList.map (fun (t,a) -> (t, f a))) fr

        let inline join (fr : Feed<Feed<'a>>) : Feed<'a> =
            setEvent (Feed.Internal.join <| toEvent (map toEvent fr)) fr            
    
        let inline bind (f : 'a -> Feed<'b>) : Feed<'a> -> Feed<'b> =
            join << (map f) 
             
        let inline stick (a : 'a) (fr : Feed<'b>) : Feed<'a * 'b> = map (tuple a) fr 

        let inline push (t : Ticks) e fr = 
            let rec inner l () =
                match l with
                | LazyList.Cons((t', e'), rest) when Time.ticks t' > t -> 
                    LazyList.consDelayed (t', e') (inner rest)
                | LazyList.Cons((t', _), _) when Time.ticks t' = t ->
                    LazyList.cons (Time.incId t t', e) l
                | other ->
                    LazyList.cons (Time.time t, e) other

            updateEvent (flip inner ()) fr
            
        let tryHead fr =
            match (toEvent fr) with
            | LazyList.Cons ((_,h), _) -> Some h
            | _ -> None    

[<AutoOpen>]
module Transformation =
    module Feed = 
        let inline toList fr = LazyList.toList (toEvent fr)

        let inline ofList l = setEvent (LazyList.ofList (List.sortByDescending fst l)) Feed.empty


[<AutoOpen>]
module Merging = 
    module Feed =    
        
        let weave (f : Option<'a> -> 'b -> 'a) (frB : Feed<'b>) (frA : Feed<'a>) : Feed<'a> = 
            let rec inner lb la =
                match lb, la with
                | (Cons ((tb,_), _), Cons ((ta,va), rest)) when ta > tb -> LazyList.consDelayed (ta,va) (fun () -> inner lb rest)
                | (Cons ((tb,vb), rest), Cons ((_,va), _)) -> LazyList.consDelayed (tb,f (Some va) vb) (fun () -> inner rest la)
                | (Cons ((tb,vb), rest), Nil) -> LazyList.consDelayed (tb,f None vb) (fun () -> inner rest la)
                | Nil, _ -> la
            combineMeta (inner (toEvent frB) (toEvent frA)) frA frB

        let combine (frA : Feed<'a>) (frB : Feed<'a>) : Feed<'a> = 
            combineMeta (merge' (toEvent frB) (toEvent frB)) frA frB


[<AutoOpen>]
module Timing =
    open Internal
    module Feed =
        
        let time (fr : Feed<'a>) : Feed<Time> = 
            updateEvent (LazyList.map (fun (t,_) -> (t,t))) fr

        let timeStamp (fr : Feed<'a>) : Feed<Time * 'a> = 
            updateEvent (LazyList.map (fun (t,a) -> (t,(t,a)))) fr

        let timeStampAsTicks fr =
            Feed.map (fun (t,a) -> (Time.ticks t,a)) (timeStamp fr)

        let dateTimed fr =
            Feed.map (fun (t,a) -> (Time.toDateTime t,a)) (timeStamp fr)

[<AutoOpen>]
module Filtering =
    module Feed =
        
        let filter (f : 'a -> bool) (fr : Feed<'a>) : Feed<'a> =
            let rec inner l =
                match l with
                | LazyList.Cons((t,a), rest) when f a -> LazyList.consDelayed (t,a) (fun () -> inner rest)
                | LazyList.Cons(_, rest) -> inner rest
                | Nil -> LazyList.empty
            updateEvent inner fr

        let choose (f : 'a -> 'b option): Feed<'a> -> Feed<'b> = 
            Feed.map f >> filter (Option.isSome) >> Feed.map Option.get

        let partition (f : 'a -> bool) (fr : Feed<'a>) : (Feed<'a> * Feed<'a>) = (filter f fr, filter (not << f) fr)   
    
        let takeWhile (f : 'a -> bool) (fr : Feed<'a>) : Feed<'a> = 
            let rec inner l () =
                match l with
                | LazyList.Cons((t,h), rest) when f h ->
                    LazyList.consDelayed (t,h) (inner rest)
                | _ -> LazyList.empty

            updateEvent (flip inner ()) fr


        let skipWhile (f : 'a -> bool) (fr : Feed<'a>) : Feed<'a> = 
            let rec inner l () =
                match l with
                | LazyList.Cons((_,h), rest) when f h ->
                    LazyList.delayed (inner rest)
                | _ -> l
            updateEvent (flip inner ()) fr


        let discardBefore time fr =
            Feed.timeStamp fr
            |> takeWhile (fun (t,_) -> t >= time)
            |> Feed.map snd

[<AutoOpen>]
module Grouping =
    module Feed =

        let span (f : 'a -> bool) (fr : Feed<'a>) : (Feed<'a> * Feed<'a>) = 
            (Feed.takeWhile f fr, Feed.skipWhile f fr)
                        

[<AutoOpen>] 
module Folding =
    module Feed =
        
        let mapFold (now : Now) (f : 's -> 'a -> ('s * 'b)) (state : 's) (fr : Feed<'a>) : (Feed<'s * 'b>) =
            let fr' = Feed.discardBefore now.Past fr
            let (l', _) = Seq.mapFoldBack (fun (t,a) s -> let (s', b) = f s a in ((t, (s', b))), s') (toEvent fr') state 
            setEvent (LazyList.ofSeq l') fr'
        
        let fold now (f : 's -> 'a -> 's) (s : 's) (fr : Feed<'a>) : Feed<'s> =
            Feed.map fst <| mapFold now (fun s a -> let s' = f s a in (s', ())) s fr



[<AutoOpen>]
module ComputationalExpression =
    type FeedBuilder() =
        member inline this.Return(x : 'T) = Feed.pure' x
        member inline this.ReturnFrom(x) = x
        member inline this.Bind(ma, f) = Feed.bind f ma
        member inline this.Zero () = Feed.pure' ()

    let feed = FeedBuilder()
    
    
module Debug =
    let trace fr = Feed.map (fun (t,v) -> printfn "%A: %A" t v; v) (Feed.timeStamp fr)
