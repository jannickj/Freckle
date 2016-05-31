namespace Freckle

type Time2 = Time2 of int
type Freck<'e> = Freck of (Time2 * 'e) list

module Freck =  

    let pure' (a : 'a) : Freck<'a> = undefined

    let map (f : 'a -> 'b) (fr : Freck<'a>) : Freck<'b> = undefined

    let bind (f : 'a -> Freck<'b>) (fr : Freck<'a>) : Freck<'b> = undefined

    let filter (f : 'a -> bool) (fr : Freck<'a>) : Freck<'a> = undefined

    let partition (f : 'a -> bool) (fr : Freck<'a>) : (Freck<'a> * Freck<'a>) = undefined

    let foldNow (f : 's -> 'a -> 's) (s : 's) (fr : Freck<'a>) : Freck<'s> = 
        undefined

    let foldNowAsync (f : 's -> 'a -> Async<'s>) (s : 's) (fr : Freck<'a>) : Freck<Async<'s>> = 
        undefined

    let planNow (fr : Freck<Async<'a>>) : Async<Freck<'a>> = undefined

    let foldUntil (f : 's -> 'a -> 's option) (s : 's) (fr : Freck<'a>) : Freck<'s> = 
        undefined
    
    let foldUntilAync (f : 's -> 'a -> Async<'s> option) (s : 's) (fr : Freck<'a>) : Freck<'s> = 
        undefined

    let afterNow (fr : Freck<'a>) : Freck<'a> = undefined

    let interval (t : Time2) (fr : Freck<'a>) : Freck<'a list> = undefined

    let listenTo<'a> (fr : Freck<obj>) : Freck<'a> = undefined

    let combine (frA : Freck<'a>) (frB : Freck<'a>) : Freck<'a> = undefined

    let moveForward (t : Time2) (evts : 'a list)  (frA : Freck<'a>) : Freck<'a> = undefined

    let latest (frA : Freck<'a>) : 'a = undefined