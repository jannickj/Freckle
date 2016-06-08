namespace Freckle

open System

type Time = 
    | Indeterminable
    | Time of int64
type Freck<'e> = Freck of LazyList<Time * 'e>

type Statemachine<'s,'e> = Statemachine of ('s -> ('s * Freck<'e>))

module Fvent =
    
    let time = fst
    let map f (t, a) = (t, f a)
    let set v = map (const' v)

module Freck =  
    open LazyList

    let pure' (a : 'a) : Freck<'a> = 
        Freck <| LazyList.ofList [(Indeterminable, a)]

    let map (f : 'a -> 'b) (Freck fr : Freck<'a>) : Freck<'b> = 
        fr |> LazyList.map (Fvent.map f) |> Freck

    let filter (f : 'a -> bool) (Freck l : Freck<'a>) : Freck<'a> =
        let rec inner l =
            match l with
            | LazyList.Cons((t,a), rest) when f a -> LazyList.consDelayed (t,a) (fun () -> inner rest)
            | LazyList.Cons(_, rest) -> inner rest
            | Nil -> LazyList.empty
        Freck (inner l)

    let choose (f : 'a -> 'b option): Freck<'a> -> Freck<'b> = 
        map f >> filter (Option.isSome) >> map Option.get
    
    let timed (Freck fr : Freck<'a>) : Freck<Time * 'a> = 
        Freck <| LazyList.map (fun (t,a) -> (t,(t,a))) fr
    
    let join (Freck fr : Freck<Freck<'a>>) : Freck<'a> = 
       LazyList.map (fun (t, Freck cfr) -> LazyList.map (tuple t) (LazyList.map snd cfr)) fr
       |> LazyList.concat
       |> Freck
            
    let partition (f : 'a -> bool) (fr : Freck<'a>) : (Freck<'a> * Freck<'a>) = (filter f fr, filter (not << f) fr)
    
    let bind (f : 'a -> Freck<'b>) : Freck<'a> -> Freck<'b> =
        join << (map f) 
                
    let listenTo<'a> (fr : Freck<obj>) : Freck<'a> = choose safeUnbox fr

    let combine (frA : Freck<'a>) (frB : Freck<'a>) : Freck<'a> = undefined
    
    let until (f : 'a -> bool) 

module Statemachine =

    let transition (f : 's -> 'a -> ('s * 'b)) (fr : Freck<'a>) : Statemachine<'s,'b> = 
        let t =
 
    let inline mapAccumNow' (f : 's -> 'a -> 's) (s : 's) (fr : Freck<'a>) : (Freck<'s>) = 
        snd <| mapAccumNow (fun s a -> let s' = f s a in (s', s')) s fr


    let foldNowAsync (f : 's -> 'a -> Async<'s>) (s : 's) (fr : Freck<'a>) : Freck<Async<'s>> = 
        undefined

    let planNow (fr : Freck<Async<'a>>) : Async<Freck<'a>> = undefined

    let moveForward (t : DateTime) (evts : 'a list)  (frA : Freck<'a>) : Freck<'a> = undefined

    let latest (frA : Freck<'a>) : 'a = undefined