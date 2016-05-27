namespace Freckle

type Reactive<'e, 't, 'a> = Core.Reactive<'e, 't, 'a option>
type Signal<'t,'e> = Reactive<'e,'t,'e>

type TimeSpan = TimeSpan of uint64
type Time = uint64


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

type Tape<'d> = Tape of List<Time * 'd>

module Tape =
    let ofList strips = Tape (List.map Strip.timeData strips)

    let asList (Tape tape) = (List.map Strip tape)

    let find head tape = 
        match List.partition (fun strip -> TapeHead.isBefore strip head) (asList tape) with
        | (newer, current::older) when (TapeHead.isAt current head) -> (ofList older, current, ofList newer)
        | _ -> failwith <| sprintf "missing strip at %A, tape is broken" (TapeHead.time head)

    let findCurrent head tape =
        let (_,cur,_) = find head tape
        Strip.data cur

    let attach (Strip (t,s)) (Tape tape) = (Tape ((t,s) :: tape))

    let asHeads (Tape tape) = (List.map (TapeHead.ofTime << fst) tape)

    let empty = Tape []

    let choose f (Tape tape) = Tape <| List.choose (fun (t,d) -> Option.map (fun d' -> (t,d')) (f d)) tape

type Recorder<'a> = Tape<obj> -> 'a list

type Track<'d,'a> = Track of List<Strip<'d> * 'a>
    
module Track =
    let append strip value (Track j) = (Track ((strip, value) :: j))
    let fresh<'d,'a> : Track<'d,'a> = Track []

module Recorder =
    open System

//    let private mapEvent f (t,e) = (t, f e)
//
//    let inline map f t =
//        List.map (mapEvent f) t
//
//    let inline foldp (f : 'ping -> 'state ->'state) (state : 'state) (m : Signal<'ping>) : Signal<'state> =
//        [List.foldBack (fun (t,p) (_,s) -> (t, f p s)) m (0uL, state)]
    
    let map f (recorder : Recorder<'a>) : Recorder<'b> =
        //fun tape h -> f (recorder tape h) 
        undefined

    let foldp (f: )
//    let read<'a> : Recorder<'a> =
//        fun tape h -> Tape.findCurrent h tape

    let record fullTape  (recorder : Recorder<'a>) : Track<'d,'a> =
//        let folder strip (tape, journey) =
//            let tape' = Tape.attach strip tape
//            let head = TapeHead.fromStrip strip
//            let journey' = Track.append strip (recorder tape' head) journey
//            (tape', journey')
//        snd <| List.foldBack folder (Tape.asList fullTape) (Tape.empty, Track.fresh)
        undefined

    let spoolBackwardUntil (f : 'a -> bool) (recorder : Recorder<'a>) : Recorder<'a> =
//        fun tape head ->
//           let (older,_,_) = Tape.find head tape
//           let head = List.find (f << (recorder tape)) (Tape.asHeads older)
//           recorder tape head
        undefined

    let choose (recorder : List<Recorder<'a option>>) : Recorder<'a> =
//        fun tape _ ->
//            let heads = Tape.asHeads tape
//            let res = List.zip heads <| List.map (recorder tape) heads
//            undefined
        undefined

    let when' (f : 'a -> bool) (recorder : Recorder<'a>) : Recorder<bool> =
        
//        let ff a = if f a then Some a else None
//        map ff recorder
//        |> choose 
        undefined

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