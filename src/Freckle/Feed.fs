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
          //Id    : TimeId
        }
        with static member time t = { Ticks = t } //; Id = 0u }
             static member origin = Time.time 0L
             static member ticks t = t.Ticks
             static member incId t tOld =
                match tOld with
                | tOld' when t = tOld'.Ticks -> { Ticks = t } //; Id = tOld'.Id + 1u }
                | _ -> Time.time t             
             static member toDateTime t = DateTime(Time.ticks t)
             static member max = { Ticks = Int64.MaxValue } //; Id = UInt32.MaxValue }
             static member realise t1 t2 = if t2.Ticks = 0L then t1 else t2
             override x.ToString() = sprintf "%A" x

    type Now = 
        { Current : Time
          Past : Time
        }
        with static member beginning = { Current = Time.origin; Past = Time.origin }
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
            
            let agify ll = map (fun (t, l) -> map (fun (t2, a) -> ((max t t2), ((t2,t), a))) (realiseTime t l)) ll
                
            let pretty fr = fr |> toList |> List.map (fun (t, a) -> t.Ticks, a)
            let superPretty a = map (fun (ta,((t1,t2),aa)) -> (ta.Ticks, ((t1.Ticks,t2.Ticks), aa))) a |> toList
            let superPretty2 a = map (fun (_,((t1,t2),aa)) -> (t1.Ticks,t2.Ticks, aa)) a |> toList |> List.head

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

        let betweenNow (now : Now) fr =
            fr |> discardYoungerExcl now.Current |> discardOlderIncl now.Past
            
[<AutoOpen>]
module Grouping =
    module Feed =

        let span (f : 'a -> bool) (fr : Feed<'a>) : (Feed<'a> * Feed<'a>) = 
            (Feed.takeWhile f fr, Feed.skipWhile f fr)
                        

[<AutoOpen>] 
module Folding =
    module Feed =
        
        let mapFold (now : Now) (f : 's -> 'a -> ('s * 'b)) (state : 's) (fr : Feed<'a>) : (Feed<'s * 'b>) =
            let fr' = Feed.betweenNow now fr
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
