namespace FSharp

module FrpAirlockExample =
    open Freckle
    open System
    open Freckle.Computation


    type Door = InnerDoor
              | OuterDoor

    type DoorStatus = Opened
                    | Closed
    
    type AirLockEvent = PressButton
                      | DoorOpened of Door
                      | DoorClosed of Door
                      | Pressurized
                      | Depressurized

    type AirLockState =
        | Pressurizing
        | Depressurizing
        | IsPressurized
        | IsDepressurized

    type Airlock =
        { Open : Door -> Async<unit>
          Close : Door -> Async<unit>
          Pressurize : Async<unit>
          Depressurize : Async<unit> 
          ShowTerminal : string -> Async<unit> 
          DoNothing : Async<unit>
        }
     
     type State =
        { Click   : DateTime option
          Airlock : AirLockState
        }
    

    let stm (airlock : Airlock) state event =
        match state, event with
        | IsPressurized  , PressButton          -> (Depressurizing , airlock.Close InnerDoor)
        | Depressurizing , DoorClosed InnerDoor -> (Depressurizing , airlock.Depressurize)
        | Depressurizing , Depressurized        -> (Depressurizing , airlock.Open OuterDoor)
        | Depressurizing , DoorOpened OuterDoor -> (IsDepressurized, airlock.DoNothing)

        | IsDepressurized, PressButton          -> (Pressurizing   , airlock.Close OuterDoor)
        | Pressurizing   , DoorClosed OuterDoor -> (Pressurizing   , airlock.Pressurize)
        | Pressurizing   , Pressurized          -> (Pressurizing   , airlock.Open InnerDoor)        
        | Pressurizing   , DoorOpened InnerDoor -> (IsPressurized  , airlock.DoNothing)

        | _                                     -> (state          , airlock.DoNothing)

    let asyncStm (stm : 'a -> 'e -> 'a * Async<_>) (s :'a) (evt : 'e) : Async<'a> =
        async {
            let (s',ma) = stm s evt
            let! a = ma
            return s'
        }

    let doubleClickTime = TimeSpan.FromMilliseconds 500.0

    let isDoubleClick cs (time,e) =
        match cs with
        | Some lastTime when time - lastTime < doubleClickTime -> (None, Some e)
        | _ -> (Some time, None)
    
    let doublePress evts = 
        let (buttonEvts, others) = Freck.partition ((=) PressButton) evts
        buttonEvts
        |> Freck.dateTimed
        |> StateFreck.ofFreck
        |> StateFreck.mapFold isDoubleClick
        |> StateFreck.choose id
        |> StateFreck.mapFreck (Freck.combine others)


//    let doublePress time clickState evts  =
//        let (buttonEvts, others) = Freck.partition ((=) PressButton) evts
//        
//        let (clickState', doublePresses) =
//            buttonEvts
//            |> Freck.dateTimed
//            |> Freck.mapFoldp time isDoubleClick clickState
//        doublePresses
//        |> Freck.choose id
//        |> Freck.combine others
//        |> tuple clickState'


    let astm airlock ms e =
        async {
            let! s = ms
            let (s', ma) = stm airlock s e
            do! ma
            return s'
        }
    let test : StateFreck<int,int> = undefined

    let airlockProg (airlock : Airlock) evts =
        statefreck {
            let! e = doublePress evts
                     |> StateFreck.attachAsTuple
            
            let! (s, c) = StateFreck.get
            let (s', ma) = stm airlock s e
            do! StateFreck.put (s', c)
            return ma
        }
    
    let airlockProg2 (airlock : Airlock) evts =
        statefreck {
            let! e = doublePress evts
                     |> StateFreck.attachAsTuple
            let! (
            return async {
                        let! s = astm airlock s e
                        return  
                   }
        }



    
//    let airlockProg (airlock : Airlock) events (s : State) =
//        let folder = asyncStm (stm airlock)
//        let (clickState', events') =
//            events
//            |> doublePress s.Click
//        events'
//        |> Freck.foldNowAsync folder s.Airlock
//        |> Freck.planNow
//        |> (Async.map Freck.latest)
//        |> Async.map (fun airlock -> { s with Airlock = airlock; Click = clickState'})

    
    let program2 (airlock : Airlock) clock event =
        let transform (a, c) = (async.Return a, c)
        StateFreck.execute (airlockProg2 airlock) clock event

    let program (airlock : Airlock) clock newEvents (events, state) =
        async {
            let! evts = newEvents
            let! time = clock
            let sss = airlockProg2 airlock evts state time
            return ()
//            let events' = Freck.moveForward time evts events
//            let! state' = airlockProg airlock events' state
//            return (events', state')
        }