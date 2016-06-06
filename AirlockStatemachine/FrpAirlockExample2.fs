namespace Freckle

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

        
    let doublePress evts clickState  =
        let (buttonEvts, others) = Freck.partition ((=) PressButton) evts

        buttonEvts
        |> Freck.timed
        |> Freck.foldNow (fun s a -> s) clickState
        |> Freck.map snd
        |> Freck.combine others

    let airlockProg (airlock : Airlock) events s =
        let folder = asyncStm (stm airlock)
        events
        |> doublePress
        |> Freck.foldNowAsync folder s
        |> Freck.planNow


    let program (airlock : Airlock) clock newEvents (events, state) =
        async {
            let! evts = newEvents
            let! time = clock
            let events' = Freck.moveForward time evts events
            let! state' = airlockProg airlock events' state
            return (events', Freck.latest state')
        }