namespace Freckle

open System

type Time = 
    | Indeterminable
    | Time of DateTime
type Fvent<'e> = Time * 'e
type Freck<'e> = Freck of LazyList<Fvent<'e>>

module Fvent =
    
    let time = fst
    let map f (t, a) = (t, f a)
    let set v = map (const' v)

module Freck =  
    open LazyList

    let join (fr : Freck<Freck<'a>>) : Freck<'a> = undefined

    let pure' (a : 'a) : Freck<'a> = Freck <| LazyList.ofList [(Indeterminable, a)]

    let map (f : 'a -> 'b) (Freck fr : Freck<'a>) : Freck<'b> = fr |> LazyList.map (Fvent.map f) |> Freck

    let bind (f : 'a -> Freck<'b>) (Freck fr : Freck<'a>) : Freck<'b> =
        join <| Freck (LazyList.map (Fvent.map f) fr) 

    let filter (f : 'a -> bool) (fr : Freck<'a>) : Freck<'a> = undefined
    
    let timed (fr : Freck<'a>) : Freck<DateTime * 'a> = undefined

    let partition (f : 'a -> bool) (fr : Freck<'a>) : (Freck<'a> * Freck<'a>) = (filter f fr, filter (not << f) fr)

    let choose (f : 'a -> 'b option) (fr : Freck<'a>) : Freck<'b> = undefined
                
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