namespace Freckle

type StateMachine<'s, 'e, 't, ^k, 'a when ^k : (static member (+) : ^k * ^k -> ^k)> = Core.StateMachine<'s, 'e, 't, Knowledge< ^k,'a>>

module StateMachine =

    let inline pure' (x : 'a) : StateMachine<'s,'e,'t,'k,'a> = undefined

    let inline map (f : 'a -> 'b) (ma : StateMachine<'s,'e,'t,'k,'a>) : StateMachine<'s,'e,'t,'k,'b> = undefined

    let inline ap (mf : StateMachine<'s,'e,'t,'k,'a -> 'b>) (ma : StateMachine<'s,'e,'t,'k,'a>) : StateMachine<'s,'e,'t,'k,'b> = undefined

    let inline bind (fm : 'a -> StateMachine<'s,'e,'t,'k,'b>) (ma : StateMachine<'s,'e,'t,'k,'a>) : StateMachine<'s,'e,'t,'k,'b> = undefined

    let inline (>>=) ma fm = bind fm ma

