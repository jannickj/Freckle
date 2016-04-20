namespace Freckle

type Knowledge<'k, 'v when ^k : (static member (+) : ^k * ^k -> ^k)> = 'k -> ('k * 'v)

module Knowledge =

    let pure' x = fun k -> (k, x)

    let inline map f ma : Knowledge<_,_> = fun k -> let (k', a) = ma k in (k', f a)

    let inline ap (mf : Knowledge<_,'a -> 'b>) (ma : Knowledge<_,'a>) : Knowledge<_,'b> = 
        fun k -> 
            let (k1, f) = mf k 
            let (k2, a) = ma k
            (k1 + k2, f a)
