namespace Freckle

type Reactive<'e, 't, ^k, 'a when ^k : (static member (+) : ^k * ^k -> ^k)> = Core.Reactive<'e, 't, Knowledge< ^k,'a option>>

module Reactive =
    
    let inline skip<'e, 't, ^k,'a when ^k : (static member (+) : ^k * ^k -> ^k)> : Reactive<'e, 't, ^k,'a> = 
        fun _ k -> (k, None) 
            
    let inline pure' (x : 'a) : Reactive<_,_,_,'a> = fun _ k -> (k, Some x) 

    let inline map (f : 'a -> 'b) (ma : Reactive<'e,'t,'k,'a>) : Reactive<'e,'t,'k,'b> = 
        fun e k ->
            match ma e k with
            | (k', Some a) -> (k', Some (f a))
            | (k', None) -> (k', None)

    let inline ap (mf : Reactive<'e,'t,'k,'a -> 'b>) (ma : Reactive<'e,'t,'k,'a>) : Reactive<'e,'t,'k,'b> = undefined

    let inline bind (fm : 'a -> Reactive<'e,'t,'k,'b>) (ma : Reactive<'e,'t,'k,'a>) : Reactive<'e,'t,'k,'b> = 
        fun e k ->            
            match ma e k with
            | (k', Some a) -> fm a e k'
            | (k', None) -> (k', None)

    let inline (>>=) ma fm = bind fm ma

    let inline combine (a : Reactive<'e, 't,'k,'x>) (b : Reactive<'e, 't,'k,'x>) : Reactive<'e, 't,'k,'x> = undefined 
    
    let inline foldp (f : 'ping  -> 'state -> 'state) (state : 'state) (m : Reactive<'e, 't, ^k,'ping>) : Reactive<'e, 't, ^k,'state> = 
        fun (Events events) knowledge ->
            let folder evt (evts, k, s) =
                let evts' = (evt :: evts)
                match m (Events evts') k with
                | (k', Some p) -> 
                   (evts', k', f p s)
                | k', None ->  (evts', k', s)
            let (_, knowledge', state') = List.foldBack folder events ([], knowledge, state) 
            (knowledge', Some state')

    
    let inline timedEvent<'e, 't, ^k when ^k : (static member (+) : ^k * ^k -> ^k)> : Reactive<'e, 't, ^k, ('e * 't)> = 
        fun (Events evts) -> Knowledge.pure' (List.tryHead evts)

    let inline event<'e, 't, ^k when ^k : (static member (+) : ^k * ^k -> ^k)> : Reactive<'e, 't, ^k, 'e> = 
        map fst timedEvent

    let inline time<'e, 't, ^k when ^k : (static member (+) : ^k * ^k -> ^k)> : Reactive<'e, 't, ^k, 't> = 
        map snd timedEvent

    let inline filter (p : 'a -> bool) (m : Reactive<'e, 't, ^k,'a>) : Reactive<'e, 't, ^k,'a> = 
        bind (fun a -> if p a then pure' a else skip) m