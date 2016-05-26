namespace Freckle

type Reactive<'e, 't, 'a> = Core.Reactive<'e, 't, 'a option>
type Signal<'t,'e> = Reactive<'e,'t,'e>

type TimeSpan = TimeSpan of uint64

type Strip = Strip of uint64 * obj
    with 
        static member time (t,_) = t
type Tape = Tape of List<uint64 * obj>
    with 
        static member find (s,_) t = 
            match List.partition (fun (t,_) -> t > s) t with
            | (_::older, newer) -> (older, newer)
            | _ -> failwith "missing tape is broken"
        static member attach (Strip (t,s)) (Tape tape) = (Tape ((t,s) :: tape))
        static member asList (Tape tape) = (List.map Strip tape)

type Recorder<'a> = Tape -> Strip -> 'a

type Journey<'a> = List<Strip * 'a>

module Reactive =
    open System
    
//    let private mapEvent f (t,e) = (t, f e)
//
//    let inline map f t =
//        List.map (mapEvent f) t
//
//    let inline foldp (f : 'ping -> 'state ->'state) (state : 'state) (m : Signal<'ping>) : Signal<'state> =
//        [List.foldBack (fun (t,p) (_,s) -> (t, f p s)) m (0uL, state)]
    let play fullTape  (recoder : Recorder<'a>) : Journey<'a> =
        let folder strip (tape, journey) =
            let tape' = Tape.attach strip tape
            let journey' = (strip, recoder tape strip) :: journey
            (tape', journey')
        List.foldBack folder (Tape.asList fullTape) []

    let filter (f : 'a -> bool) (r : Recorder<'a>) : Recorder<'a> =
        fun tape _ -> undefined
            

    let stepBack (f : 'a -> 'b )  (r : Recorder<'a>) : Recorder<'b>  = 
        fun tape strip ->
            //let (older, newer) = Tape.find strip tape
            undefined
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

    let inline filter (p : 'a -> bool) (m : Reactive<'e, 't, 'a>) : Reactive<'e, 't, 'a> = 
        bind (fun a -> if p a then pure' a else skip) m