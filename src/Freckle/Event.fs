namespace Freckle

open System

type Time = 
    | Indeterminable
    | Time of int64
type Freck<'e> = Freck of LazyList<Time * 'e>

module Fvent =
    
    let time = fst
    let map f (t, a) = (t, f a)
    let set v = map (const' v)

module Freck =  
    open LazyList

    let rec private merge l1 l2 : LazyList<Time *'e> =
        match (l1, l2) with
        | Nil, vs -> vs
        | us, Nil -> us
        | LazyList.Cons((ta,a), ps), LazyList.Cons((tb,_), _) when ta >= tb ->           
            let mergePs () = merge ps l2
            LazyList.consDelayed (ta,a) mergePs
        | _, LazyList.Cons((tb,b), qs) ->           
            let mergeQs () = merge l1 qs
            LazyList.consDelayed (tb,b) mergeQs

    let pure' (a : 'a) : Freck<'a> = 
        Freck <| LazyList.ofList [(Indeterminable, a)]

    let map (f : 'a -> 'b) (Freck fr : Freck<'a>) : Freck<'b> = 
        fr |> LazyList.map (Fvent.map f) |> Freck

    let filter (f : 'a -> bool) (fr : Freck<'a>) : Freck<'a> = undefined
    
    let choose (f : 'a -> 'b option): Freck<'a> -> Freck<'b> = 
        map f >> filter ((<>) None) >> map Option.get
    

    let timed (fr : Freck<'a>) : Freck<Time * 'a> = undefined 
    
    let dateTimed (Freck fr : Freck<'a>) : Freck<DateTime * 'a> = undefined

    let join (Freck fr : Freck<Freck<'a>>) : Freck<'a> = 
       LazyList.map (fun (t, Freck cfr) -> LazyList.map (tuple t) (LazyList.map snd cfr)) fr
       |> LazyList.concat
       |> Freck
    
    let bind (f : 'a -> Freck<'b>) : Freck<'a> -> Freck<'b> =
        join << (map f) 

            
    let partition (f : 'a -> bool) (fr : Freck<'a>) : (Freck<'a> * Freck<'a>) = (filter f fr, filter (not << f) fr)

                
    let listenTo<'a> (fr : Freck<obj>) : Freck<'a> = undefined

    let combine (frA : Freck<'a>) (frB : Freck<'a>) : Freck<'a> = undefined

    let mapAccumNow (f : 's -> 'a -> ('s * 'b)) (s : 's) (fr : Freck<'a>) : ('s * Freck<'b>) = 
        undefined

    let inline mapAccumNow' (f : 's -> 'a -> 's) (s : 's) (fr : Freck<'a>) : (Freck<'s>) = 
        snd <| mapAccumNow (fun s a -> let s' = f s a in (s', s')) s fr


    let foldNowAsync (f : 's -> 'a -> Async<'s>) (s : 's) (fr : Freck<'a>) : Freck<Async<'s>> = 
        undefined

    let planNow (fr : Freck<Async<'a>>) : Async<Freck<'a>> = undefined

    let moveForward (t : DateTime) (evts : 'a list)  (frA : Freck<'a>) : Freck<'a> = undefined

    let latest (frA : Freck<'a>) : 'a = undefined