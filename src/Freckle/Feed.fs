namespace Freckle

///The Feed represents an continoues (possibly extremely large) number of events, for this reason the type is Lazy.
///The Feed type is identical to the Event type in traditional FRP.
///However it has been renamed in order to be more intuitive, as it should not be thought of as a single event but rather a continoues stream of events.
type Feed<'e> = Feed of LazyList<Time * 'e>
    with override x.ToString() = sprintf "%A" x

///The feed module contains all functions necessary to work with feeds.
module Feed =
    open FSharp.Helpers
    open LazyList
    open Sample.ComputationalExpression
    
    ///This module contains internally used functions, these are suceptible to change even with minor updates. 
    ///Is not recommended for use in a production environment.
    module Internal =
        open LazyList
            
        let inline updateEvent f (Feed fr : Feed<'a>) : Feed<'b> = Feed (f fr)
            
        let inline ofEvent l : Feed<'b> = (Feed l)
                                
        let inline toEvent (Feed fr : Feed<_>) = fr

        let single time a = Feed (LazyList.cons (time,a) LazyList.empty )
        
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
            | Nil -> (fun _ -> empty)
            
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
            | Cons((t1,(Cons((t2,_),_))),_) -> 
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
    
        let inline feed event = Feed event

        let everyTicks (pulseDistance : Ticks) : Sample<Feed<Time>> =
            sample {
                let! finish = Period.finish
                let rec inner dist time ()  = 
                    if time < Time.origin
                    then LazyList.empty 
                    else LazyList.consDelayed (time, time) (inner dist (Time.time (time.Ticks - dist)))
                let finalticks = Time.ticks finish
                let ticksSincePulse = (finalticks % pulseDistance)
                let lastPulse = finalticks - ticksSincePulse
                
                return feed (LazyList.delayed (inner pulseDistance (Time.time lastPulse)))
            }

    open Internal

    ///Provides an empty feed with no events
    let inline empty<'e> : Feed<'e> = feed LazyList.empty 

    ///Provides a feed with a single event at the specified time
    let inline singleton time evt = feed (LazyList.cons (time, evt) LazyList.empty)

    ///Provides a feed with a single event at origin Time
    let inline pure' (a : 'a) : Feed<'a> = 
        feed (LazyList.ofList [(Time.origin, a)])

    ///Get a new feed where events are mapped from one value to another, retaining their current time
    let inline map (f : 'a -> 'b) (fr : Feed<'a>) : Feed<'b> = 
        updateEvent (LazyList.map (fun (t,a) -> (t, f a))) fr
        
    ///Joins multiple feeds together.
    ///A white paper will be released to explain how exactly this function works
    let inline join (fr : Feed<Feed<'a>>) : Feed<'a> =
        ofEvent (join' <| toEvent (map toEvent fr))            
    
    ///The bind operator for the monad type class instance
    let inline bind (f : 'a -> Feed<'b>) : Feed<'a> -> Feed<'b> =
        join << (map f)

    ///Bind but with the feed returned by the function ignored
    let inline bind_ (f : 'a -> Feed<unit>) : Feed<'a> -> Feed<'a> =
        bind (fun a -> map (const' a) (f a))
             
    ///Attach a constant value to all events
    let inline stick (a : 'a) (fr : Feed<'b>) : Feed<'a * 'b> = map (tuple a) fr 
            
    ///Attempt to get lastest event from the feed
    let inline tryHead fr =
        match (toEvent fr) with
        | LazyList.Cons ((_,h), _) -> Some h
        | _ -> None

    ///Active pattern matching such that Head and Empty can be used in pattern matching
    let inline (|Head|Empty|) feed =
        match toEvent feed with
        | Cons(a, rest) -> Head(snd a, ofEvent rest)
        | Nil -> Empty
                  
    ///Leading debounce, that prevents more than one event occuring for each sampling
    let inline debouncing fr = 
        updateEvent (take 1) fr

    ///Take upto the first n events in a feed and return them in a new feed
    [<System.Obsolete("use deboucing, this name makes no sense")>]
    let inline take n fr = 
        updateEvent (take n) fr

    
    ///Skip the first n events in feed and return the remaining events in a new feed
    let inline skip n fr = 
        updateEvent (fun l -> skip n l) fr

    ///Test the if a new have a certain count of events.
    ///This will return upto the specified count, depending on the actual count.
    let inline testLength n fr = take n fr |> toEvent |> length
    
    ///Transforms a feed to list.
    ///Be careful when using this function as list is strict, and given Feeds can be possible trillions of events long when fully evaluated, this can lead to inefficient code.
    let inline toList fr = LazyList.toList (toEvent fr)

    ///Transforms a list into a feed
    let inline ofList l = ofEvent (LazyList.ofList (List.sortByDescending fst l))
    
    let inline combine (frA : Feed<'a>) (frB : Feed<'a>) : Feed<'a> = 
        ofEvent (merge' (toEvent frA) (toEvent frB))

    ///A special combine function that takes the last event of Feed<'a> that is not older than an event from Feed<'b> and maps it with 'b. 
    ///If no event in Feed<'a> is found then initA is used instead.
    let inline weave (f : 'a -> 'b -> 'a) (initA : 'a) (frB : Feed<'b>) (frA : Feed<'a>) : Feed<'a> = 
        let rec inner lb la =
            match lb, la with
            | (Cons ((tb,_), _), Cons ((ta,va), rest)) when ta > tb -> LazyList.consDelayed (ta,va) (fun () -> inner lb rest)
            | (Cons ((tb,vb), rest), Cons ((_,va), _)) -> LazyList.consDelayed (tb,f va vb) (fun () -> inner rest la)
            | (Cons ((tb,vb), rest), Nil) -> LazyList.consDelayed (tb,f initA vb) (fun () -> inner rest la)
            | Nil, _ -> la
        ofEvent (inner (toEvent frB) (toEvent frA))

    ///Push an event with a specified time onto a feed. This function is safe and will maintain Temporal Monotonicity.
    let inline push t a fr = combine (singleton t a) fr
    
    ///Get a feed with events firing using the frequency of pulsePerSecond, starting from the finish time of a Sample, continuing until origin Time.
    ///This is useful for making pull-based events.
    let inline pulse (pulsePerSecond : int) : Sample<Feed<Time>> =
        let tps = System.TimeSpan.TicksPerSecond
        let pulseDistance = tps / (int64 pulsePerSecond)
        everyTicks pulseDistance

    ///Identical to pulse except events outside the sampling resolution are ignored
    ///This is useful if old events that came about due to lag aren't necessary for operations to continue.
    ///e.g. when drawing the screen in a graphical application it would be wasteful to do multiple times. Just because of internal lag that prevented drawing last two updates.
    let inline pulseUpto pulsePerSecond =
        pulse pulsePerSecond
        |> Sample.map (take 1)

    ///Get a feed with events firing with an interval of the specified time, starting from the finish time of a Sample, continuing until origin Time.
    ///This is useful for making pull-based events.
    let inline every time : Sample<Feed<Time>> =
        everyTicks (Time.ticks time)

    ///Identical to every except does not guarantee all events are fired, if for instance a lagspike occurs.
    let inline everyUpto time : Sample<Feed<Time>> =
        every time
        |> Sample.map (take 1)

    ///delays events to an later sampling, it is recommended to be used together with discardFuture, as these events will still be present in the feed but just ignored by the sampling.
    let inline delay t fr = 
        updateEvent (fun l -> LazyList.map (fun (t',a) -> (t' + t, a)) l) fr

    ///Overwrites the event value with the time of the event
    let inline time (fr : Feed<'a>) : Feed<Time> = 
        updateEvent (LazyList.map (fun (t,_) -> (t,t))) fr

    ///Timestamps all event with their arrival time, changing the timestamp will not have an impact of their arrival time, see delay for this.
    let inline timeStamp (fr : Feed<'a>) : Feed<Time * 'a> = 
        updateEvent (LazyList.map (fun (t,a) -> (t,(t,a)))) fr

    ///Same as timeStamp but provides the time in ticks
    let inline timeStampAsTicks fr =
        map (fun (t,a) -> (Time.ticks t,a)) (timeStamp fr)

        
    ///Same as timeStamp but provides the time in DateTime
    let inline dateTimed fr =
        map (fun (t,a) -> (Time.toDateTime t,a)) (timeStamp fr)

    ///Get a new feed where all event values satisfy the predicate
    let inline filter (predicate: 'a -> bool) (fr : Feed<'a>) : Feed<'a> =
        let rec inner l =
            match l with
            | LazyList.Cons((t,a), rest) when predicate a -> LazyList.consDelayed (t,a) (fun () -> inner rest)
            | LazyList.Cons(_, rest) -> inner rest
            | Nil -> LazyList.empty
        updateEvent inner fr
    
    ///Selects all events with the Value of Some
    let inline choose (f : 'a -> 'b option): Feed<'a> -> Feed<'b> = 
        map f >> filter (Option.isSome) >> map Option.get

    ///Partitions the feed into two feeds, the first satisfy the predicate and the second does not satisfy
    let inline partition (f : 'a -> bool) (fr : Feed<'a>) : (Feed<'a> * Feed<'a>) = (filter f fr, filter (not << f) fr)   
    
    ///Take events from the feed until the predicate is no longer satisfied
    let inline takeWhile (f : 'a -> bool) (fr : Feed<'a>) : Feed<'a> =            
        updateEvent (LazyList.delayed << takeWhile' (f << snd)) fr

    ///Discard events from the feed until the predicate is no longer satisfied
    let inline skipWhile (f : 'a -> bool) (fr : Feed<'a>) : Feed<'a> = 
        updateEvent (LazyList.delayed << skipWhile' (f << snd)) fr
            
    ///Discard events occuring before the specified time, excluding events occuring exactly at the time
    let inline discardOlderExcl (time : Time)  fr : Feed<'a>=
        updateEvent (discardBeforeExcl time) fr

    ///Discard events occuring before the specified time, including events occuring exactly at the time
    let inline discardOlderIncl (time : Time)  fr : Feed<'a>=
        updateEvent (discardBeforeIncl time) fr
        
    ///Discard events occuring after the specified time, including events occuring exactly at the time
    let inline discardYoungerIncl (time : Time)  fr : Feed<'a>=
        updateEvent (discardAfterIncl time) fr
        
    ///Discard events occuring after the specified time, excluding events occuring exactly at the time
    let inline discardYoungerExcl (time : Time)  fr : Feed<'a>=
        updateEvent (discardAfterExcl time) fr

    ///Discard all events occuring after the sample finish time, excluding events occuring exactly at the finish time
    let inline discardFuture fr =
        sample {
            let! finish = Period.finish
            return fr |> discardYoungerExcl finish
        }
        
    ///Discard all events occuring before the sample beginning time, including events occuring exactly at the beginning time
    let inline discardPast fr =
        sample {
            let! beginning = Period.beginning
            return fr |> discardOlderIncl beginning
        }
                
    ///Discards all events before the sample (inclusive) and all events after the sample (Exclusive)
    let inline discardOutsideSample fr =
        sample {
            let! fr' = discardFuture fr
            return! discardPast fr'
        }
        
    ///Span, applied to a predicate p and a feed, returns a tuple of feed where the first feed  satisfy p and second element is the remainder of the list: 
    let inline span (p : 'a -> bool) (fr : Feed<'a>) : (Feed<'a> * Feed<'a>) = 
        (takeWhile p fr, skipWhile p fr)
    
    ///Groups elements together where f is satisfied
    let inline groupBy (f : 'a -> 'a -> bool) (fr : Feed<'a>) : Feed<Feed<'a>> =
        let mapl l = (fst <| LazyList.head l, ofEvent l)
        updateEvent (fun l -> LazyList.map mapl <| groupBy (fun a b -> f (snd a) (snd b)) l) fr
                              
    ///Return a new feed consisting of the results of applying the given accumulating function to the newests events of the feed
    let inline scan f s = updateEvent (LazyList.scan (fun (_,s) (t,a) -> (t, f s a)) (Time.origin,s))

    //Combines mapping events and the results of applying the given accumulating function to the newests events of the feed
    let inline mapScan (f : 's -> 'a -> ('s * 'b)) (s : 's) (fr : Feed<'a>) : Feed<'s * 'b>= 
        updateEvent (LazyList.ofSeq << fst << (Seq.mapFold (fun s (t,a)  -> let (b, s') = f s a in ((t, (b, s'))), b) s) ) fr

    ///Discards all events outside the sample, then scanMaps from the past
    let inline mapScanPast (f : 's -> 'a -> ('s * 'b)) (state : 's) (fr : Feed<'a>) : (Sample<Feed<'s * 'b>>) =
        sample {
            let! fr' = discardOutsideSample fr
            let (l', _) = Seq.mapFoldBack (fun (t,a) s -> let (s', b) = f s a in ((t, (s', b))), s') (toEvent fr') state 
            return ofEvent (LazyList.ofSeq l')
        }

    ///Discards all events outside the sample, then folds and maps from the past
    let inline mapFoldPast (f : 's -> 'a -> ('s * 'b)) (state : 's) (fr : Feed<'a>) : (Sample<'s * Feed<'b>>) =
        sample {
            let! fr' = discardOutsideSample fr
            let (l', state') = Seq.mapFoldBack (fun (t,a) s -> let (s', b) = f s a in ((t, b), s')) (toEvent fr') state 
            return (state', ofEvent (LazyList.ofSeq l'))
        }
    
    ///Discards all events outside the sample, then scans the events from the past by applying the given accumulating function to the oldest events of the feed,
    ///Returning a feed of the results
    let inline scanPast (f : 's -> 'a -> 's) (state : 's) (fr : Feed<'a>) : Sample<Feed<'s>> =
        sample {
            let! fr' = discardOutsideSample fr
            let (l, _) = Seq.mapFoldBack (fun (t,a) s -> let s' = f s a in ((t, s'), s')) (toEvent fr') state 
            return ofEvent (LazyList.ofSeq l)
        }
        
    ///Discards all events outside the sample, then folds the events from the past by applying the given accumulating function to the oldest events of the feed,
    ///Returning the a the last state as the result
    let foldPast (f : 's -> 'a -> 's) (state : 's) (fr : Feed<'a>) : Sample<'s> =
        sample {
            let! fr' = discardOutsideSample fr
            return Seq.foldBack (fun (_, a) s -> f s a) (toEvent fr') state
        }

    ///Discards all events outside the sample, then combine all async computations into a single async computation with the result of a feed of the asyncs' results.
    let inline plan (fullFeed : Feed<Async<'a>>) : Sample<Async<Feed<'a>>> =
        sample {
            let! fr = discardOutsideSample fullFeed
            let folder (t,ma) (newV) =
                async {
                    let! newV' = newV
                    let! a = ma
                    return LazyList.cons (t,a) newV'
                }
            let folded =  (Seq.foldBack folder (toEvent fr)) (async.Return LazyList.empty)
            return Async.map ofEvent folded
        }

    ///Same as plan but discard the feed of results
    let inline plan_ fr = plan fr |> Sample.map Async.Ignore
    ///Splits a Feed into components of length upto k
    let chunkBy n feed = 
        let giveTime l = (fst <| Array.head l, l)
        ofEvent (LazyList.ofSeq (Seq.chunkBySize n (toEvent feed) |> Seq.map giveTime))

        
    ///Discard all events outside the sample, then from the past folds all the events of Feed<'a> using an async state transition into a single state
    let inline transition (f : 's -> 'a -> Async<'s>) (state : 's) (allFeed : Feed<'a>)  : Sample<Async<'s>> =
        sample {
            let! fr = discardOutsideSample allFeed
            let inner ms a =
                async {
                    let! s = ms
                    return! f s a
                }
            return! foldPast inner (async.Return state) fr
        }

    //Fire an event each time this feed is fired, but ignore the feed's output
    let then' m feed = bind_ (fun _ -> m |> map ignore) feed 

    ///Provide operator syntax for bind
    module Operator =
    
        //Identical to bind
        let inline (>>=) m f = bind f m

     ///Setup for computational expression(Do Notation in haskell) for Feed, to better understand how this works, consider two event streams firing after each other a computation is made each time,
     //one of the events is fired.
    module ComputationalExpression = 
        type Builder() =
            member inline this.Return(x : 'T) = pure' x
            member inline this.ReturnFrom(x) = x
            member inline this.Bind(ma, f) = bind f ma
            member inline this.Zero () = pure' ()
            member inline this.Yield x = pure' x
            member inline this.YieldFrom x = x
            member inline this.Combine(f1,f2) = combine f1 f2
            member inline this.Delay f = Internal.ofEvent (LazyList.delayed (Internal.toEvent << f))
    
        let feed = Builder()

///Debugging functionality for Feed
module Debug =
    ///Debugging functions only for feed
    module Feed =
        
        ///maps and prints events when they are observered, if they are never observed then print will never occur
        let inline traceMap f fr = Feed.map (fun (t,v) -> printfn "%A: %A" (Time.toDateTime t) (f v); v) (Feed.timeStamp fr)

        ///Same a traceMap but just tries to print the event
        let inline trace fr = traceMap id fr