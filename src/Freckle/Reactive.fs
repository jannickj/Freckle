namespace Freckle

type Reactive<'e, 't, 'a> = Core.Reactive<'e, 't, 'a option>
type Signal<'t,'e> = Reactive<'e,'t,'e>

type TimeSpan = TimeSpan of uint64
type Time = uint64

type Narrative = Narrative of Time
    with 
        static member beginNew = Narrative 0uL

type Strip<'d> = Strip of Time * 'd

module Strip =
    let time (Strip (t,_)) = t
    let data (Strip (_,d)) = d
    let timeData (Strip (t,d)) = (t,d)

type TapeHead = TapeHead of Time
    with
        static member fromStrip (Strip (t,_)) = TapeHead t 
        static member isBefore (Strip (s, _)) (TapeHead t) = t < s //Option.default' false <| Option.map (fun t -> ) ot 
        static member isAt (Strip (s, _)) (TapeHead t) =  t = s   //Option.default' false <| Option.map (fun t -> ) ot
        static member time (TapeHead t) = t
        static member ofTime t = TapeHead t

type Event<'d> = Event of List<Time * 'd>
type Freckle<'a> = Event<obj> -> Narrative -> 'a list

module Event =
    let ofList strips = Event (List.map Strip.timeData strips)

    let asList (Event tape) = (List.map Strip tape)
    
    let box (Event evt) = Event (List.map (fun (t, e) -> (t, e :> obj)) evt)

    let find head tape = 
        match List.partition (fun strip -> TapeHead.isBefore strip head) (asList tape) with
        | (newer, current::older) when (TapeHead.isAt current head) -> (ofList older, current, ofList newer)
        | _ -> failwith <| sprintf "missing strip at %A, tape is broken" (TapeHead.time head)

    let findCurrent head tape =
        let (_,cur,_) = find head tape
        Strip.data cur

    let attach (Strip (t,s)) (Event tape) = (Event ((t,s) :: tape))

    let asHeads (Event tape) = (List.map (TapeHead.ofTime << fst) tape)

    let empty = Event []

    let choose f (Event tape) = Event <| List.choose (fun (t,d) -> Option.map (fun d' -> (t,d')) (f d)) tape

    let append strip value (Event j) = (Event ((strip, value) :: j))
    
    let plan (Event p : Event<Async<'a>>) : Async<Event<'a>> =
        let prog (s, result) =
            async {
                match s with
                | (t,ma) :: rest ->
                    let! a = ma
                    return Some (rest, (t,a) :: result)
                | [] -> return None
            }
        Async.recursion prog (List.rev p, []) 
        |> Async.map (snd >> Event)

    let statemachine (Event stm :  Event<'s -> Async<'s>>) (intialState : 's) : Async<'s> =
        let prog (l, s) =
            async {
                match l with
                | f :: rest -> 
                    let! s' = f s
                    return Some (rest, s')
                | [] -> return None
            }
        Async.recursion prog (List.map snd <| List.rev stm, intialState)
        |> Async.map snd

module Freckle =
    
    let listenTo<'s> : Freckle<'s> =
        fun (Event evt) _ ->
            let up (e : obj) =
                match e with
                | :? 's as ev -> Some ev
                | _ -> None
            List.choose (up << snd) evt

    let pure' (a : 'a) : Freckle<'a> = undefined

    let map (f : 'a -> 'b) (fr : Freckle<'a>) : Freckle<'b> =
        //fun tape h -> f (recorder tape h) 
        undefined

    let bind (f : 'a -> Freckle<'b>) (fr : Freckle<'a>) : Freckle<'b> =  undefined
    
    let step (events : Event<_>) (nt : Narrative) (fr : Freckle<'a>) : (Narrative * Event<'a>) =
        undefined

    
    let assemble (updateEvents : Event<_> -> Async<Event<_>>) (stepper : Event<'e> -> 's  -> Async<'s>) (fr : Freckle<'e>) (state : 's) : Async<_> =
        let prog (evts, nt, s) =
            async {
                let! evts' = updateEvents evts
                let (nt', step) = step evts' nt fr
                let! s' = stepper step s
                return (evts', nt', s')
            }
        Async.forever prog (Event.empty, Narrative.beginNew, state)

    let assembleStatemachine eventUpdater stm fr =
        let fr = map stm fr
        assemble eventUpdater Event.statemachine fr


//    let find (f : 'a -> Option<'b>) (r : Recorder<'a>)
//    let stepBack (f : 'a -> 'b )  (r : Recorder<'a>) : Recorder<'b>  = 
//        fun tape strip ->
//            //let (older, newer) = Tape.find strip tape
//            undefined
//    let inline skip<'e, 't, 'a> : Reactive<'e, 't, 'a> = 
//        fun _ -> None 
//            
//    let inline pure' (x : 'a) : Reactive<_,_,'a> = fun _ -> Some x 
//
//    let inline map (f : 'a -> 'b) (ma : Reactive<'e,'t,'a>) : Reactive<'e,'t,'b> = 
//        fun e ->
//            match ma e with
//            | Some a -> Some (f a)
//            | None -> None
//
//    let inline ap (mf : Reactive<'e,'t,'a -> 'b>) (ma : Reactive<'e,'t,'a>) : Reactive<'e,'t,'b> = undefined
//
//    let inline bind (fm : 'a -> Reactive<'e,'t,'b>) (ma : Reactive<'e,'t,'a>) : Reactive<'e,'t,'b> = 
//        fun e ->            
//            match ma e with
//            | Some a -> fm a e
//            | None -> None
//
//    let inline (>>=) ma fm = bind fm ma
//    
//    let inline foldp (f : 'ping  -> 'state -> 'state) (state : 'state) (m : Reactive<'e, 't, 'ping>) : Reactive<'e, 't, 'state> =
//        failwith ""
////        fun (Events events) knowledge ->
////            let folder evt (evts, k, s) =
////                let evts' = (evt :: evts)
////                match m (Events evts') k with
////                | (k', Some p) -> 
////                   (evts', k', f p s)
////                | k', None ->  (evts', k', s)
////            let (_, knowledge', state') = List.foldBack folder events ([], knowledge, state) 
////            (knowledge', Some state')
//
//    
//    let inline timedEvent<'e, 't> : Reactive<'e, 't, ('e * 't)> = 
//        fun (Events evts) -> List.tryHead evts
//
//    let inline event<'e, 't> : Reactive<'e, 't, 'e> = 
//        map fst timedEvent
//
//    let inline time<'e, 't> : Reactive<'e, 't, 't> = 
//        map snd timedEvent

//    let inline filter (p : 'a -> bool) (m : Reactive<'e, 't, 'a>) : Reactive<'e, 't, 'a> = 
//        bind (fun a -> if p a then pure' a else skip) m