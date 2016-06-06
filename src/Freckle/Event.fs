namespace Freckle

open System

    
type Fvent<'e> = DateTime * 'e
type Freck<'e> = Freck of LazyList<Fvent<'e>> 

module Fvent =
    
    let time = fst
    let map f (t, a) = (t, f a)
    let set v = map (const' v)

module Freck =  
    open LazyList

    let pure' (a : 'a) : Freck<'a> = undefined

    let map (f : 'a -> 'b) (fr : Freck<'a>) : Freck<'b> = undefined

    let bind (f : 'a -> Freck<'b>) (fr : Freck<'a>) : Freck<'b> = undefined

    let filter (f : 'a -> bool) (fr : Freck<'a>) : Freck<'a> = undefined
    
    let timed (fr : Freck<'a>) : Freck<DateTime * 'a> = undefined

    let partition (f : 'a -> bool) (fr : Freck<'a>) : (Freck<'a> * Freck<'a>) = (filter f fr, filter (not << f) fr)

    let mapAccumNow (f : 's -> 'a -> ('s * 'b)) (s : 's) (fr : Freck<'a>) : ('s * Freck<'b>) = 
        undefined

    let inline mapAccumNow' (f : 's -> 'a -> 's) (s : 's) (fr : Freck<'a>) : (Freck<'s>) = 
        snd <| mapAccumNow (fun s a -> let s' = f s a in (s', s')) s fr

    let foldNowAsync (f : 's -> 'a -> Async<'s>) (s : 's) (fr : Freck<'a>) : Freck<Async<'s>> = 
        undefined

    let planNow (fr : Freck<Async<'a>>) : Async<Freck<'a>> = undefined

    let foldUntil (f : 's -> 'a -> 's option) (state : 's) (Freck lazyList : Freck<'a>) : Freck<'s> =
        let rec folder s lz =
            match lz with
            | Cons(a, rest) -> 
                match f s a with
                | Some s' -> folder s' rest
                | None -> Some (s, lz)
            | nil -> None 

        let rec inner s oldLz =
            match oldLz with
            | Cons(a, rest) -> 
                match f s a with
                | Some s' -> LazyList.cons (Fvent.set s' a) (inner s' rest)
                | None -> inner state rest
            | Nil -> undefined
        undefined
    
    let foldUntilAync (f : 's -> 'a -> Async<'s> option) (s : 's) (fr : Freck<'a>) : Freck<'s> = 
        undefined

    let afterNow (fr : Freck<'a>) : Freck<'a> = undefined

    let interval (time : TimeSpan) (fr : Freck<'a>) : Freck<'a list> = 
//        let folder (ts as t,l) (te,e) = if ts - te < 0 then None else Some (t, e :: l)
//        map snd <| foldUntil folder (time,[]) (timed fr)
        undefined
        
    let listenTo<'a> (fr : Freck<obj>) : Freck<'a> = undefined

    let combine (frA : Freck<'a>) (frB : Freck<'a>) : Freck<'a> = undefined

    let moveForward (t : DateTime) (evts : 'a list)  (frA : Freck<'a>) : Freck<'a> = undefined

    let latest (frA : Freck<'a>) : 'a = undefined