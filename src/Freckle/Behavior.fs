namespace Freckle

type Behavior<'t, ^k, 'a when ^k : (static member (+) : ^k * ^k -> ^k)> = Core.Behavior<'t, Knowledge< ^k,'a option>>

module Behavior =
        
    let inline pure' (x : 'a) : Behavior<_, _, 'a> =
        Option.pure' x
        |> Knowledge.pure'
        |> Core.Behavior.pure'
        
    let inline map (f : 'a -> 'b) (ma : Behavior<_,_,'a>) : Behavior<_,_,'b> =
        Core.Behavior.map (Knowledge.map (Option.map f)) ma

    let inline ap (mf : Behavior<'t,'k,'a -> 'b>) (ma : Behavior<'t,'k,'a>) : Behavior<_,_,'b> = undefined

    let inline bind (fm : 'a -> Behavior<'t,'k,'b>) (ma : Behavior<'t,'k,'a>) : Behavior<'t,'k,'b> = undefined

    let inline (>>=) ma fm = bind fm ma

    let inline combine (a : Behavior<'t,'k,'x>) (b : Behavior<'t,'k,'x>) : Behavior<'t,'k,'x> = undefined 


