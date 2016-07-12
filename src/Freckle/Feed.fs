[<AutoOpen>]
module Freckle.Feed

open FSharp.Helpers
open LazyList

[<AutoOpen>]
module Types =
    open System

    type Future<'e> = Time * 'e

    type Feed<'e> =
        { Event : LazyList<Time * 'e>
        }
        with override x.ToString() = sprintf "%A" x

module Future = 
    let map f (t, a) = (t, f a)

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
            
            let realiseTime t (l : LazyList<Time * 'a>) : LazyList<Time * 'a> =
                LazyList.map (fun (ta,a) -> (Time.realise t ta, a)) l

            let mapFirst f l =
                 match l with
                 | Cons(a, r) -> cons (f a) r
                 | Nil -> empty

            let mapSecond f l =
                 match l with
                 | Cons(a, (Cons(b, r))) -> cons a (cons (f b) r)
                 | _ -> l 
            
                
            let pretty fr = fr |> toList |> List.map (fun (t, a) -> t.Ticks, a)
            let superPretty a = map (fun (ta,((t1,t2),aa)) -> (ta.Ticks, ((t1.Ticks,t2.Ticks), aa))) a |> toList
            let superPretty2 a = map (fun (_,((t1,t2),aa)) -> (t1.Ticks,t2.Ticks, aa)) a |> toList |> List.head


            let inline capMax maxAge (l : LazyList<Time * LazyList<Time * 'a>>) : LazyList<Time * LazyList<Time * 'a>> = 
                map (fun (t,la) -> (t, discardAfterIncl maxAge la)) (discardAfterIncl maxAge l)

            let inline capMin minAge (l : LazyList<Time * LazyList<Time * 'a>>) : LazyList<Time * LazyList<Time * 'a>> = 
                map (fun (t,la) -> (t, discardBeforeExcl minAge la)) (discardBeforeExcl minAge l)
            
            let rec findMin minAge  (l : LazyList<Time * Time>) =
                match l with
                | Cons((t,_),_) when t < minAge -> minAge
                | Cons((t, ta), tail) ->
                    findMin (max (min t ta) minAge) tail
                | Nil -> minAge

            let inline toTime (l : LazyList<Time * LazyList<Time * 'a>>) =
                let toTime (t,la) = 
                    match la with
                    | Cons((ta,_),_) -> (t, ta)
                    | Nil -> (t, Time.maxValue)
                map toTime l
                
            let agify ll = map (fun (t, l) -> map (fun (t2, a) -> (max t t2, a)) l) ll

            let inline combineAll (l : LazyList<LazyList<Time * 'a>>) =
               Seq.fold (fun s a -> merge' s a) LazyList.empty l

            let rec join' (l : LazyList<Time * LazyList<Time * 'a>>) : LazyList<Time * 'a> =
                let minAge = findMin Time.minValue (toTime l)
                if minAge > Time.minValue
                then 
                    let res = 
                        capMin minAge l
                        |> agify
                        |> combineAll
                    LazyList.append res (join' (capMax minAge l))
                else LazyList.empty
                
            let rec inner (ageMax : Time) t1Max t2Max (l : LazyList<LazyList<Time * ((Time * Time) * 'a)>>) () : LazyList<Time * 'a> =
                match l with      
                | Nil -> LazyList.empty
                | Cons(la, Nil) -> filter (fun (age, ((t1,t2),_)) -> age <= ageMax && t1 <= t1Max && t2 <= t2Max ) la |> map (fun (age, (_,a)) -> (age, a))
                | Cons(la, (Cons(lb, tail2) as tail1)) ->
                    match la, lb with
                    | Nil, _ -> inner ageMax t1Max t2Max tail1 ()

                    | _, Nil -> inner ageMax t1Max t2Max (cons la tail2) ()

                    | Cons((ageA,((t1a,t2a),_)), tailA), _ when ageA > ageMax || t1a > t1Max || t2a > t2Max ->
                        inner ageMax t1Max t2Max (cons tailA tail1) ()

                    | la, Cons((ageB,((t1b,t2b),_)), tailB) when ageB > ageMax || t1b > t1Max || t2b > t2Max ->
                        inner ageMax t1Max t2Max (cons la (cons tailB tail2)) ()
                    
                    | Cons((ageA,(((t1a,t2a)),a)), tailA),  Cons((_,((t1b,t2b),_)),_) when t1a >= t1b && t2a >= t2b  ->
                        printfn "1a: %A" (superPretty2 la, superPretty2 lb)
                        consDelayed (ageA,a) <| inner (min ageA ageMax) (min t1a t1Max) (min t2a t2Max)  (cons tailA tail1)
                    
                    | Cons((ageA,(((t1a,t2a)),a)), tailA),  Cons((_,((t1b,t2b),_)),_) when max t1a t2a < max t1b t2b  ->
                        printfn "1c: %A" (superPretty2 la, superPretty2 lb)
                        consDelayed (ageA,a) <| inner (min ageA ageMax) (min t1a t1Max) (min t2a t2Max)  (cons tailA tail1)

                    | Cons((ageA,(((t1a,t2a)),a)), tailA),  Cons((_,((t1b,t2b),_)),_) when max t1a t2a = max t1b t2b && (abs (t1a.Ticks - t2a.Ticks) <= abs (t1b.Ticks - t2b.Ticks)) ->
                        printfn "1d: %A" (superPretty2 la, superPretty2 lb)
                        consDelayed (ageA,a) <| inner (min ageA ageMax) (min t1a t1Max) (min t2a t2Max)  (cons tailA tail1)
//                    | Cons((ageA,((t1a,t2a),a)), tailA),  Cons((_,((t1b,t2b),_)),_) when t2a > t1a && t2a >= t2b && t1a >= t1b ->
//                        printfn "1b: %A" (superPretty2 la, superPretty2 lb)
//                        consDelayed (ageA,a) <| inner (min ageA ageMax) (min t1a t1Max) (min t2a t2Max)  (cons tailA tail1)


                    | Cons((ageA,((t1a,t2a),a)), tailA),  Cons((_,((t1b,t2b),_)),_) when t2a = t1a && t1a >= (min t1b t2b) ->
                        printfn "1e: %A" (superPretty2 la, superPretty2 lb)
                        consDelayed (ageA,a) <| inner (min ageA ageMax) (min t1a t1Max) (min t2a t2Max)  (cons tailA tail1)

//                    | Cons((ageA,((t1a,t2a),a)), tailA),  Cons((_,((t1b,t2b),_)),_) when t2a = t1a && t1b >= t1a && t2a >= t2b ->
//                        printfn "1d: %A" (superPretty2 la, superPretty2 lb)
//                        consDelayed (ageA,a) <| inner (min ageA ageMax) (min t1a t1Max) (min t2a t2Max)  (cons tailA tail1)
                    
                    | _, _ -> 
                        printfn "2x: %A" (superPretty2 la, superPretty2 lb)
                        inner ageMax t1Max t2Max tail1 ()

//                    | Cons((_,((t1a,t2a),_)), tailA), Cons((ageB,((t1b,t2b),b)),tailB) when (t1b >= t2b && t1b >= t1a && (t2b >= (min t1a t2a) )) ->
//                        consDelayed (ageB,b) <| inner (min ageB ageMax) (min t1b t1Max) (min t2b t2Max)  (cons tailA (cons tailB tail2))
//                    
//                    | Cons((_,((t1a,t2a),_)), tailA), Cons((ageB,((t1b,t2b),b)),tailB) when (t2b >= t1b && t2b >= t2a && (t1b >= (min t1a t2a) )) ->
//                        consDelayed (ageB,b) <| inner (min ageB ageMax) (min t1b t1Max) (min t2b t2Max) (cons tailA (cons tailB tail2))
//
//                    | _ -> failwith <| sprintf "FAK!\n %A" (superPretty la, superPretty lb)

            let inline join' (list : LazyList<Time * LazyList<Time * 'a>>) : LazyList<Time * 'a> =
               
                
                let res = inner Time.max Time.max Time.max (agify list)
                printfn "fuk"
//                printfn "%A" (pretty list |> List.map (fun (t, a) -> (t, pretty a)))
//                printfn "%A" (map superPretty (agify list) |> toList)
//                printfn "%A" (res () |> pretty)
//                printfn "fak"
                res () |> toList |> ignore
                delayed <| res

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
            feed (LazyList.ofList [(Time.max, a)])

        let inline map (f : 'a -> 'b) (fr : Feed<'a>) : Feed<'b> = 
            updateEvent (LazyList.map (fun (t,a) -> (t, f a))) fr

        let inline join (fr : Feed<Feed<'a>>) : Feed<'a> =
            setEvent (join' <| toEvent (map toEvent fr)) fr            
    
        let inline bind (f : 'a -> Feed<'b>) : Feed<'a> -> Feed<'b> =
            join << (map f)

        let inline bind_ (f : 'a -> Feed<unit>) : Feed<'a> -> Feed<'a> =
            bind (fun a -> map (const' a) (f a))
             
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

module Operator =
    
    let (>>=) m f = Feed.bind f m

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
module Signal =
    module Act =
        open System

        let pulse (pulsePerSecond : uint32) : Sample<Feed<Time>> =
            sample {
                let! finish = Sample.finish
                let! beginning = Sample.beginning
                let rec inner dist time ()  = 
                    if time.Ticks < dist || time <= beginning
                    then LazyList.empty 
                    else LazyList.consDelayed (time, time) (inner dist (Time.time (time.Ticks - dist)))
                let ticks = Time.ticks finish
                let tps = TimeSpan.TicksPerSecond
                let pulseDistance = tps / (int64 pulsePerSecond)
                let ticksSincePulse = (ticks % pulseDistance)
                let lastPulse = ticks - ticksSincePulse
                
                return Feed.feed (LazyList.delayed (inner pulseDistance (Time.time lastPulse)))
            }

        let period : Sample<Feed<Period>> =
            sample {
                let! period = Sample.period
                return single period.Finish period
            }



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

        ///Discards all events before the sample (inclusive) and all events after the sample (Exclusive)
        let between fr =
            sample {
                let! beginning = Sample.beginning
                let! finish = Sample.finish
                return fr |> discardYoungerExcl finish |> discardOlderIncl beginning
            }

[<AutoOpen>]
module Grouping =
    module Feed =

        let span (f : 'a -> bool) (fr : Feed<'a>) : (Feed<'a> * Feed<'a>) = 
            (Feed.takeWhile f fr, Feed.skipWhile f fr)
                        

[<AutoOpen>] 
module Folding =
    module Feed =
        
        let mapScan (f : 's -> 'a -> ('s * 'b)) (state : 's) (fr : Feed<'a>) : (Sample<Feed<'s * 'b>>) =
            sample {
                let! fr' = Feed.between fr
                let (l', _) = Seq.mapFoldBack (fun (t,a) s -> let (s', b) = f s a in ((t, (s', b))), s') (toEvent fr') state 
                return setEvent (LazyList.ofSeq l') fr'
            }

        let mapFold (f : 's -> 'a -> ('s * 'b)) (state : 's) (fr : Feed<'a>) : (Sample<'s * Feed<'b>>) =
            sample {
                let! fr' = Feed.between fr
                let (l', state') = Seq.mapFoldBack (fun (t,a) s -> let (s', b) = f s a in ((t, b), s')) (toEvent fr') state 
                return (state', setEvent (LazyList.ofSeq l') fr')
            }

        let scan (f : 's -> 'a -> 's) (state : 's) (fr : Feed<'a>) : Sample<Feed<'s>> =
            sample {
                let! fr' = Feed.between fr
                let (l, _) = Seq.mapFoldBack (fun (t,a) s -> let s' = f s a in ((t, s'), s')) (toEvent fr') state 
                return setEvent (LazyList.ofSeq l) fr'
            }

        let fold (f : 's -> 'a -> 's) (state : 's) (fr : Feed<'a>) : Sample<'s> =
            sample {
                let! fr' = Feed.between fr
                return Seq.foldBack (fun (_, a) s -> f s a) (toEvent fr') state
            }
            
[<AutoOpen>]
module Planning =
    module Feed =
        open Feed.Internal

        let plan (fullFeed : Feed<Async<'a>>) : Sample<Async<Feed<'a>>> =
            sample {
                let! fr = Feed.between fullFeed
                let folder (t,ma) (newV) =
                    async {
                        let! newV' = newV
                        let! a = ma
                        return LazyList.cons (t,a) newV'
                    }
                let folded =  (Seq.foldBack folder (toEvent fr)) (async.Return LazyList.empty)
                return Async.map (flip setEvent fr) folded
            }
        
        let transition (f : 's -> 'a -> Async<'s>) (state : 's) (allFeed : Feed<'a>)  : Sample<Async<'s>> =
            sample {
                let! fr = Feed.between allFeed
                let inner ms a =
                    async {
                        let! s = ms
                        return! f s a
                    }
                return! Feed.fold inner (async.Return state) fr
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
    let trace fr = Feed.map (fun (t,v) -> printfn "%A: %A" t v; v) (Feed.timeStamp fr)
