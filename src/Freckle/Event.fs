namespace FSharp

module Freckle =

    open System

    type Time = Time of int64
        with static member origin = Time 0L
    type Freck<'e> = Freck of LazyList<Time * 'e>
    type StateFreck<'s,'a> = (Time -> 's -> (Time * 's * Freck<'a>))

    module Freck =  
        open LazyList
        open Freckle

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
            Freck <| LazyList.ofList [(Time.origin, a)]

        let map (f : 'a -> 'b) (Freck fr : Freck<'a>) : Freck<'b> = 
            fr |> LazyList.map (fun (t,a) -> (t, f a)) |> Freck

        let filter (f : 'a -> bool) (Freck l : Freck<'a>) : Freck<'a> =
            let rec inner l =
                match l with
                | LazyList.Cons((t,a), rest) when f a -> LazyList.consDelayed (t,a) (fun () -> inner rest)
                | LazyList.Cons(_, rest) -> inner rest
                | Nil -> LazyList.empty
            Freck (inner l)

        let choose (f : 'a -> 'b option): Freck<'a> -> Freck<'b> = 
            map f >> filter (Option.isSome) >> map Option.get
        
        let timed (Freck l : Freck<'a>) : Freck<Time * 'a> = 
            Freck <| LazyList.map (fun (t,a) -> (t,(t,a))) l

        let ticks fr =
            map (fun (Time t,a) -> (t,a)) (timed fr)

        let dateTimed fr =
            map (fun (t,a) -> (DateTime(t),a)) (ticks fr)
    
        let join (Freck fr : Freck<Freck<'a>>) : Freck<'a> = 
           LazyList.map (fun (t, Freck cfr) -> LazyList.map (tuple t) (LazyList.map snd cfr)) fr
           |> LazyList.concat
           |> Freck
            
        let partition (f : 'a -> bool) (fr : Freck<'a>) : (Freck<'a> * Freck<'a>) = (filter f fr, filter (not << f) fr)
    
        let bind (f : 'a -> Freck<'b>) : Freck<'a> -> Freck<'b> =
            join << (map f) 
                
        let listenTo<'a> (fr : Freck<obj>) : Freck<'a> = choose safeUnbox fr

        let combine (Freck lA : Freck<'a>) (Freck lB : Freck<'a>) : Freck<'a> = 
            Freck (merge lA lB)
    
        let until (f : 'a -> bool) (Freck l : Freck<'a>) : Freck<'a> = 
            let rec inner l () =
                match l with
                | LazyList.Cons((t,h), rest) when f h -> LazyList.consDelayed (t,h) (inner rest)
                | _ -> LazyList.empty

            Freck (inner l ())

        let cutBefore time fr = 
            timed fr
            |> until (fun (t,_) -> t > time)
            |> map snd


        let inline mapFoldp (t : Time) (f : 's -> 'a -> ('s * 'b)) (state : 's) (fr : Freck<'a>) : ('s * Freck<'b>) = 
            let (Freck l) = cutBefore t fr
            let (l', s') = Seq.mapFoldBack (fun (t,a) s -> let (s',b) = f s a in ((t,b), s')) l state
            (s', Freck <| LazyList.ofSeq l')

        let planp (t : Time) (fr : Freck<Async<'a>>) : Async<Freck<'a>> =
            let (Freck l) = cutBefore t fr
            let folder (t,ma) (newV) =
                async {
                    let! newV' = newV
                    let! a = ma
                    return LazyList.cons (t,a) newV'
                }
            Async.map Freck <| Seq.foldBack folder l (async.Return LazyList.empty)


        let foldp (t : Time) (f : 's -> 'a -> 's) (s : 's) (fr : Freck<'a>) : Freck<'s> =
            snd <| mapFoldp t (fun s a -> let s' = f s a in (s', s')) s fr

        let tryHead (Freck l) =
            match l with
            | LazyList.Cons ((_,h), _) -> Some h
            | _ -> None
     
    open Freck
    type FreckBuilder() =
        member inline this.Return(x : 'T) = pure' x
        member inline this.ReturnFrom(x) = x
        member inline this.Bind(ma, f) = bind f ma
        member inline this.Zero () = pure' ()

    let freckle = FreckBuilder()
    
    module StateFreck =
        open Freckle
    
        let ofFreck (fr : Freck<'a>) : StateFreck<'s, 'a> =
            fun t s -> 
                let fr' = Freck.cutBefore t fr
                match Freck.tryHead (Freck.timed fr') with
                | Some (t', _) -> (t', s, fr')
                | None -> (t, s, fr')
        
        let mapFreck (f : Freck<'a> -> Freck<'b>) (sf : StateFreck<'s,'a>) : StateFreck<'s, 'b> =
            fun t s ->
                let (t', s', fr) = sf t s
                (t', s', f fr)

        let get : StateFreck<'s, 's> = 
            fun t s -> (t, s, Freck.pure' s)
        
        let put (s : 's) : StateFreck<'s, unit> = 
            fun t _ -> (t, s, Freck.pure' ())

        let edit f : StateFreck<'s,unit> =
            fun t s -> (t, f s, Freck.pure' ())

        let pure' x = fun t s -> (t, s, Freck.pure' x)

        let join (sf : StateFreck<'s,StateFreck<'s,'a>>) : StateFreck<'s, 'a> = 
            fun t s ->
                let (t', s', fr) = sf t s
                let (s'''', fr') = Freck.mapFoldp t (fun s'' sf -> let (_,s''',a) = sf t s'' in (s''', a) ) s' fr
                (t', s'''', Freck.join fr')

        let map (f : 'a -> 'b) (sf : StateFreck<'s,'a>) : StateFreck<'s,'b> = 
            mapFreck (Freck.map f) sf

        let filter (f : 'a -> bool) (sf : StateFreck<'s,'a>) : StateFreck<'s,'a> =
            mapFreck (Freck.filter f) sf

        let choose (f : 'a -> 'b option): StateFreck<'s,'a> -> StateFreck<'s,'b> = 
            map f >> filter (Option.isSome) >> map Option.get

        let bind (f : 'a -> StateFreck<'s,'b>) (x : StateFreck<'s,'a>) : StateFreck<'s,'b> =
            join (map f x)

        let fold (f : 's -> 'a -> 's) (fs : StateFreck<'s,'a>) : StateFreck<'s,'s> =
            fun time state ->
                let (t',s,frA) = fs time state
                let frS = Freck.foldp time f s frA
                let (t'', s') = Option.default' (t',s) <| Freck.tryHead (Freck.timed frS)
                (t'', s', frS)
        
        let mapFold (f : 's -> 'a -> ('s * 'b)) (fs : StateFreck<'s,'a>) : StateFreck<'s,'b> =
            fun time state ->
                let (t',s,frA) = fs time state
                let (s', frB) = Freck.mapFoldp time f s frA
                (t', s', frB)
        
        let execute (fs : Freck<'e> -> StateFreck<'s,Async<'s -> 's>>) (timer : Async<Time>) (events : Async<'e>) : Async<unit> =
            
            undefined

        let attach (setter : 'b -> 'a -> 'b) (getter : 'b -> 'a) (m : StateFreck<'a,'x>) : StateFreck<'b,'x> =
            fun t s -> 
                let (t', a, fr) = m t (getter s)
                (t', setter s a, fr)

        let attachAsTuple (m : StateFreck<'a,'x>) : StateFreck<'b * 'a,'x> =
            attach (fun t a -> (fst t, a)) snd m

    open StateFreck
    type StateFreckBuilder() =
        member inline this.Return(x : 'T) = pure' x
        member inline this.ReturnFrom(x) = x
        member inline this.Bind(ma, f) = bind f ma
        member inline this.Zero () = pure' ()

    let statefreck = StateFreckBuilder()