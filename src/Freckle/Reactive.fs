namespace Freckle

type Reactive<'e, 't, ^k, 'a when ^k : (static member (+) : ^k * ^k -> ^k)> = Core.Reactive<'e, 't, Knowledge< ^k,'a option>>

module Reactive =

    let inline pure' (x : 'a) : Reactive<_,_,_,'a> = undefined

    let inline map (f : 'a -> 'b) (ma : Reactive<_,_,_,'a>) : Reactive<_,_,_,'b> = undefined

    let inline ap (mf : Reactive<'e,'t,'k,'a -> 'b>) (ma : Reactive<'e,'t,'k,'a>) : Reactive<'e,'t,'k,'b> = undefined

    let inline bind (fm : 'a -> Reactive<'e,'t,'k,'b>) (ma : Reactive<'e,'t,'k,'a>) : Reactive<'e,'t,'k,'b> = undefined

    let inline (>>=) ma fm = bind fm ma

    let inline combine (a : Reactive<'e, 't,'k,'x>) (b : Reactive<'e, 't,'k,'x>) : Reactive<'e, 't,'k,'x> = undefined 

    let inline bindBehavior (fm : 'a -> Reactive<'e,'t,'k,'b>) (ma : Behavior<'t,'k,'a>) : Reactive<'e,'t,'k,'b> = undefined


    

