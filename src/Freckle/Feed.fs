namespace Freckle
[<AutoOpen>]
module Feed =

    open FSharp.Helpers
    open LazyList

    [<AutoOpen>]
    module Types =
        open System
        
        type Feed<'e> =
            { Event : LazyList<Time * 'e>
            }
            with override x.ToString() = sprintf "%A" x
                
    module Period =
        let isAfter (p1 : Period) (p2 : Period) =
            p1.Beginning > p2.Beginning

    [<AutoOpen>]
    module Internal =
        module Feed = 
            module Internal =
                open LazyList
            
                let inline updateEvent f (fr : Feed<'a>) : Feed<'b> =
                    { Event = f fr.Event  }
            
                let inline ofEvent l : Feed<'b> =
                    { Event = l }
                                
                let inline toEvent (fr : Feed<_>) = fr.Event

                let single time a = { Event = LazyList.cons (time,a) LazyList.empty }
        
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
           
                let rec skipWhile' f l () =
                    match l with
                    | LazyList.Cons(a, rest) when f a ->
                        LazyList.delayed (skipWhile' f rest)
                    | _ -> l

                let rec takeWhile' f l ()=
                    match l with
                    | LazyList.Cons(a, rest) when f a ->
                        LazyList.consDelayed a (takeWhile' f rest)
                    | _ -> LazyList.empty

                let inline discardBeforeExcl time =
                    LazyList.delayed << takeWhile' (fun (t,_) -> t >= time)

                let inline discardBeforeIncl time =
                    LazyList.delayed << takeWhile' (fun (t,_) -> t > time)

                let inline discardAfterIncl time =
                   LazyList.delayed << skipWhile' (fun (t,_) -> t >= time)

                let inline discardAfterExcl time =
                   LazyList.delayed << skipWhile' (fun (t,_) -> t > time)
            
                let mapFirst f l =
                     match l with
                     | Cons(a, r) -> cons (f a) r
                     | Nil -> empty

                let mapSecond f l =
                     match l with
                     | Cons(a, (Cons(b, r))) -> cons a (cons (f b) r)
                     | _ -> l 
                
                let cutOff t lA = map (fun (tA, lB) -> (tA,discardAfterIncl t lB)) lA 
            
                let groupTime' l =
                    match l with
                    | Cons((t,a), tail) -> 
                        let res = takeWhile' (fun (t2, _) -> t = t2) tail |> delayed |> map snd |> toList
                        consDelayed (t, a :: res) 
            
                let rec groupBy f (l : LazyList<'a>) =
                    match l with
                    | Nil -> empty
                    | Cons(x, xs) ->
                        let (ys, zs) = takeWhile' (f x) xs, skipWhile' (f x) xs
                        consDelayed (consDelayed x ys) (fun () -> groupBy f (delayed zs))

                let sameTime l = 
                    groupBy (fun (t1,_) (t2,_) -> t1 = t2) l

                let sortSameTime l =
                    sameTime l
                    |> map (fun l -> Seq.sortDescending l |> LazyList.ofSeq)
                    |> LazyList.concat
                
            
                let isBetter (t1a,t2a) (t1b,t2b) =
                    let minA = min t1a t2a
                    let maxA = max t1a t2a
                    let minB = min t1b t2b
                    let maxB = max t1b t2b
                    if minA > minB then true
                    elif minA = minB then maxA > maxB
                    else false
                    

                let inline capMax maxAge l =
                    (discardAfterIncl maxAge l)
                    |> map (fun  (t, la) -> (t, discardAfterIncl maxAge la))

                let rec findNext minAge ((t1Best,t2Best,bests) as best) (l : LazyList<Time * LazyList<Time * 'a>>) =
                    match l with
                    | Cons((t1, la), rest) when t1 >= minAge -> 
                        match la with
                        | Cons((t2,_),_) when t2 < minAge -> findNext minAge best rest
                        | Cons((t2,a), rest2) when (t1 = t1Best && t2 = t2Best) || (t2 = t1Best && t1 = t2Best) -> 
                            findNext minAge (t1, t2, a :: bests) (cons (t1, rest2) rest)
                        | Cons((t2,a), rest2) when isBetter (t1, t2) (t1Best, t2Best) -> 
                            findNext (max (min t1 t2) minAge) (t1, t2, [a]) (cons (t1, rest2) rest)
                        | Cons((t2,_), rest2) ->
                            findNext (max (min t1 t2) minAge) best (cons (t1, rest2) rest)
                        | Nil ->
                            findNext minAge best rest
                    | _ -> best

                let rec joinInner l () =
                    match l with
                    | Nil -> empty
                    | Cons((_,Nil),rest) -> joinInner rest ()
                    | Cons((t1,(Cons((t2,a),_))),_) -> 
                        let (t1', t2', la) = findNext (min t1 t2) (t1,t2,List.empty) l
                        let age = max t1' t2'
                        let l'' = capMax age l
                        append (List.map (fun a -> (age, a)) la |> ofList) ((delayed << joinInner) l'')
            
                let join' l = (delayed << joinInner) l

                let inline unsafePush t e feed =
                    updateEvent (LazyList.cons (t, e)) feed

                let rec take n l =
                    match l with
                    | Cons(a, rest) when n > 0 -> consDelayed a (fun () -> take (n - 1) rest)
                    | _ -> empty
                
                let rec skip n l =
                    match l with
                    | Cons(_, rest) when n > 0 -> delayed (fun () -> skip (n - 1) rest)
                    | _ -> l

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
                ofEvent (join' <| toEvent (map toEvent fr))            
    
            let inline bind (f : 'a -> Feed<'b>) : Feed<'a> -> Feed<'b> =
                join << (map f)

            let inline bind_ (f : 'a -> Feed<unit>) : Feed<'a> -> Feed<'a> =
                bind (fun a -> map (const' a) (f a))
             
            let inline stick (a : 'a) (fr : Feed<'b>) : Feed<'a * 'b> = map (tuple a) fr 
            
            let tryHead fr =
                match (toEvent fr) with
                | LazyList.Cons ((_,h), _) -> Some h
                | _ -> None

            let inline (|Head|Empty|) feed =
                match toEvent feed with
                | Cons(a, rest) -> Head(snd a, ofEvent rest)
                | Nil -> Empty
                  
            let take n fr = 
                updateEvent (fun l -> take n l) fr
            
            let skip n fr = 
                updateEvent (fun l -> skip n l) fr

            let testLength n fr = take n fr |> toEvent |> length

    module Operator =
    
        let (>>=) m f = Feed.bind f m

    [<AutoOpen>]
    module Transformation =
        module Feed = 
            let inline toList fr = LazyList.toList (toEvent fr)

            let inline ofList l = ofEvent (LazyList.ofList (List.sortByDescending fst l))


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
                ofEvent (inner (toEvent frB) (toEvent frA))

            let combine (frA : Feed<'a>) (frB : Feed<'a>) : Feed<'a> = 
                ofEvent (merge' (toEvent frA) (toEvent frB))

    [<AutoOpen>]
    module Signal =
        module Feed =
            open System

            let pulse (pulsePerSecond : uint32) : Sample<Feed<Time>> =
                sample {
                    let! finish = Sample.finish
                    let! beginning = Sample.beginning
                    let rec inner dist time ()  = 
                        if time <= beginning
                        then LazyList.empty 
                        else LazyList.consDelayed (time, time) (inner dist (Time.time (time.Ticks - dist)))
                    let ticks = Time.ticks finish
                    let tps = TimeSpan.TicksPerSecond
                    let pulseDistance = tps / (int64 pulsePerSecond)
                    let ticksSincePulse = (ticks % pulseDistance)
                    let lastPulse = ticks - ticksSincePulse
                
                    return Feed.feed (LazyList.delayed (inner pulseDistance (Time.time lastPulse)))
                }

            let pulseAtmost pulsePerSecond =
                pulse pulsePerSecond
                |> Sample.map (Feed.take 1)

    [<AutoOpen>]
    module Timing =
        open Internal
        module Feed =

            let delay t fr = 
                updateEvent (fun l -> map (fun (t',a) -> (Time.delay t' t, a)) l) fr

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
                updateEvent (LazyList.delayed << takeWhile' (f << snd)) fr


            let skipWhile (f : 'a -> bool) (fr : Feed<'a>) : Feed<'a> = 
                updateEvent (LazyList.delayed << skipWhile' (f << snd)) fr
            
            let discardOlderExcl (time : Time)  fr : Feed<'a>=
                updateEvent (discardBeforeExcl time) fr

            let discardOlderIncl (time : Time)  fr : Feed<'a>=
                updateEvent (discardBeforeIncl time) fr

            let discardYoungerIncl (time : Time)  fr : Feed<'a>=
                updateEvent (discardAfterIncl time) fr

            let discardYoungerExcl (time : Time)  fr : Feed<'a>=
                updateEvent (discardAfterExcl time) fr

            let discardFuture fr =
                sample {
                    let! finish = Sample.finish
                    return fr |> discardYoungerExcl finish
                }

            let discardPast fr =
                sample {
                    let! beginning = Sample.beginning
                    return fr |> discardOlderIncl beginning
                }
                
            ///Discards all events before the sample (inclusive) and all events after the sample (Exclusive)
            let discardOutsideSample fr =
                sample {
                    let! fr' = discardFuture fr
                    return! discardPast fr'
                }

    [<AutoOpen>]
    module Grouping =
        module Feed =
        
            let span (f : 'a -> bool) (fr : Feed<'a>) : (Feed<'a> * Feed<'a>) = 
                (Feed.takeWhile f fr, Feed.skipWhile f fr)

            let groupBy (f : 'a -> 'a -> bool) (fr : Feed<'a>) : Feed<Feed<'a>> =
                let mapl l = (fst <| LazyList.head l, ofEvent l)
                updateEvent (fun l -> LazyList.map mapl <| groupBy (fun a b -> f (snd a) (snd b)) l) fr
                        

    [<AutoOpen>] 
    module Folding =
        module Feed =
            
            let scan f s = updateEvent (LazyList.scan (fun (_,s) (t,a) -> (t, f s a)) (Time.origin,s))

            let mapScan (f : 's -> 'a -> ('s * 'b)) (s : 's) (fr : Feed<'a>) : Feed<'s * 'b>= 
                updateEvent (LazyList.ofSeq << fst << (Seq.mapFold (fun s (t,a)  -> let (b, s') = f s a in ((t, (b, s'))), b) s) ) fr

            let mapScanPast (f : 's -> 'a -> ('s * 'b)) (state : 's) (fr : Feed<'a>) : (Sample<Feed<'s * 'b>>) =
                sample {
                    let! fr' = Feed.discardOutsideSample fr
                    let (l', _) = Seq.mapFoldBack (fun (t,a) s -> let (s', b) = f s a in ((t, (s', b))), s') (toEvent fr') state 
                    return ofEvent (LazyList.ofSeq l')
                }

            let mapFoldPast (f : 's -> 'a -> ('s * 'b)) (state : 's) (fr : Feed<'a>) : (Sample<'s * Feed<'b>>) =
                sample {
                    let! fr' = Feed.discardOutsideSample fr
                    let (l', state') = Seq.mapFoldBack (fun (t,a) s -> let (s', b) = f s a in ((t, b), s')) (toEvent fr') state 
                    return (state', ofEvent (LazyList.ofSeq l'))
                }

            let scanPast (f : 's -> 'a -> 's) (state : 's) (fr : Feed<'a>) : Sample<Feed<'s>> =
                sample {
                    let! fr' = Feed.discardOutsideSample fr
                    let (l, _) = Seq.mapFoldBack (fun (t,a) s -> let s' = f s a in ((t, s'), s')) (toEvent fr') state 
                    return ofEvent (LazyList.ofSeq l)
                }

            let foldPast (f : 's -> 'a -> 's) (state : 's) (fr : Feed<'a>) : Sample<'s> =
                sample {
                    let! fr' = Feed.discardOutsideSample fr
                    return Seq.foldBack (fun (_, a) s -> f s a) (toEvent fr') state
                }
            
    [<AutoOpen>]
    module Planning =
        module Feed =
            open Feed.Internal
            
            let plan (fullFeed : Feed<Async<'a>>) : Sample<Async<Feed<'a>>> =
                sample {
                    let! fr = Feed.discardOutsideSample fullFeed
                    let folder (t,ma) (newV) =
                        async {
                            let! newV' = newV
                            let! a = ma
                            return LazyList.cons (t,a) newV'
                        }
                    let folded =  (Seq.foldBack folder (toEvent fr)) (async.Return LazyList.empty)
                    return Async.map ofEvent folded
                }

            let plan_ fr = plan fr |> Sample.map Async.Ignore
        
            let transition (f : 's -> 'a -> Async<'s>) (state : 's) (allFeed : Feed<'a>)  : Sample<Async<'s>> =
                sample {
                    let! fr = Feed.discardOutsideSample allFeed
                    let inner ms a =
                        async {
                            let! s = ms
                            return! f s a
                        }
                    return! Feed.foldPast inner (async.Return state) fr
                }

    [<AutoOpen>]
    module ComputationalExpression =
        type FeedBuilder() =
            member inline this.Return(x : 'T) = Feed.pure' x
            member inline this.ReturnFrom(x) = x
            member inline this.Bind(ma, f) = Feed.bind f ma
            member inline this.Zero () = Feed.pure' ()

        let feed = FeedBuilder()
    
    
    module Debug =
        module Feed =
            let traceMap f fr = Feed.map (fun (t,v) -> printfn "%A: %A" (Time.toDateTime t) (f v); v) (Feed.timeStamp fr)
            let trace fr = traceMap id fr