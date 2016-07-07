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
            
            let mapFirst f l =
                 match l with
                 | Cons(a, r) -> cons (f a) r
                 | Nil -> empty

            let mapSecond f l =
                 match l with
                 | Cons(a, (Cons(b, r))) -> cons a (cons (f b) r)
                 | _ -> l 
                
            let pretty fr = fr |> toList |> List.map (fun (t, a) -> t.Ticks, a)
            let prettyPretty fr = fr |> toList |> List.map (fun (t, a) -> t.Ticks, pretty a)
            let superPretty a = map (fun (ta,((t1,t2),aa)) -> (ta.Ticks, ((t1.Ticks,t2.Ticks), aa))) a |> toList
            let superPretty2 a = map (fun (_,((t1,t2),aa)) -> (t1.Ticks,t2.Ticks, aa)) a |> toList |> List.head

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

//                if t1a > minB && t1a <= maxB && t2a >= minB && t1a >= t1b then true
//                elif t2a > minB && t2a <= maxB && t1a >= minB && t2a >= t2b then true
//                else false

            let isMuchBetter (t1a,t2a) (t1b,t2b) =
                let res = isBetter (t1a,t2a) (t1b,t2b) //&& not (isBetter (t1b,t2b) (t1a,t2a))

//                printfn "(%A vs %A) %A better than %A = %A" a b (t1a.Ticks,t2a.Ticks) (t1b.Ticks,t2b.Ticks) res
                res


            let inline capMax maxAge l =
                (discardAfterIncl maxAge l)
                |> map (fun  (t, la) -> (t, discardAfterIncl maxAge la))
                //|> filter (fun (_, la) -> not <| isEmpty la)
            
//            let 

            let rec findNext minAge ((t1Best,t2Best,bests) as best) (l : LazyList<Time * LazyList<Time * 'a>>) =
                match l with
                | Cons((t1, la), rest) when t1 >= minAge -> 
                    match la with
                    | Cons((t2,_),_) when t2 < minAge -> findNext minAge best rest
                    | Cons((t2,a), rest2) when (t1 = t1Best && t2 = t2Best) || (t2 = t1Best && t1 = t2Best) -> 
                        findNext minAge (t1, t2, a :: bests) (cons (t1, rest2) rest)
                    | Cons((t2,a), rest2) when isMuchBetter (t1, t2) (t1Best, t2Best) -> 
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
