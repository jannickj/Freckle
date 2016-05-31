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

//    type Action =
//        | DoNothing
//        | Pressurize
//        | Depressurize
//        | Open of Door
//        | Close of Door

//    type Knowledge = int
//    type Time = int
//    type AirlockEvent = Events<Time, AirLockEvent>
//    type AirlockBehavior<'a>     = Behavior<Time, Knowledge,'a>
//    type AirlockReactive<'a>     = Reactive<AirLockEvent, Time,'a>
//    type AirlockStateMachine<'a> = StateMachine<AirLockState, AirLockEvent, Time, Knowledge,'a>
    
//    module Wishes =        
//        module Reactive =
//            let react : AirlockReactive<AirLockEvent> = undefined 
//
//        module StateMachine =
//            let spoolEvents<'s> : ('s -> AirlockReactive<'s>) -> StateMachine<'s, AirLockEvent, Time, Knowledge,unit> = undefined
//            let spoolTime<'s> : ('s -> AirlockBehavior<'s>) -> StateMachine<'s, AirLockEvent, Time, Knowledge,unit> = undefined
//
//    open Wishes
//
    type Airlock =
        { Open : Door -> Async<unit>
          Close : Door -> Async<unit>
          Pressurize : Async<unit>
          Depressurize : Async<unit>
          ShowTerminal : string -> Async<unit>
        }
    
//    let tt  () : AirlockReactive<int> =        
//        Reactive.event
//        |> Reactive.filter ((=) (DoorOpened InnerDoor))
//        |> Reactive.map (const' 1)        
//        |> Reactive.foldp (+) 0
//    
    open Async

    let stm (airlock : Airlock) event state =
        match state, event with
        | IsPressurized  , PressButton          -> (Depressurizing , airlock.Close InnerDoor)
        | Depressurizing , DoorClosed InnerDoor -> (Depressurizing , airlock.Depressurize *>> airlock.ShowTerminal "Depressurizing")
        | Depressurizing , Depressurized        -> (Depressurizing , airlock.Open OuterDoor)
        | Depressurizing , DoorOpened OuterDoor -> (IsDepressurized, airlock.ShowTerminal "Depressurization completed")

        | IsDepressurized, PressButton          -> (Pressurizing   , airlock.Close OuterDoor)
        | Pressurizing   , DoorClosed OuterDoor -> (Pressurizing   , airlock.Pressurize *>> airlock.ShowTerminal "Pressurizing")
        | Pressurizing   , Pressurized          -> (Pressurizing   , airlock.Open InnerDoor)        
        | Pressurizing   , DoorOpened InnerDoor -> (IsPressurized  , airlock.ShowTerminal "Pressurization completed")

        | _ -> (state, airlock.ShowTerminal <| sprintf "event %A in state %A is not supported" event state)
    
    let asyncStm f state =
        let (state', action) = f state
        action *>> async.Return state'
    

    let program (airlock : Airlock) eventUpdater =
        Freckle.assembleStatemachine eventUpdater (asyncStm << stm airlock) Freckle.listenTo<AirLockEvent>
        


//    let transition (airlock : Airlock) state : AirlockReactive<AirLockState> =
//        reactive {
//            let! evt = Reactive.react
//            match evt, state with
//            | PressButton, IsPressurized ->
//                do! airlock.Depressurize
//                return Depressurizing
//
//            | PressButton, IsDepressurized -> 
//                do! airlock.Pressurize
//                return Pressurizing
//
//            | Pressurized, Pressurizing -> 
//                do! airlock.OpenDoor InnerDoor
//                return IsPressurized
//
//            | Depressurized, Depressurizing -> 
//                do! airlock.OpenDoor OuterDoor
//                return IsDepressurized
//
//            | _ -> 
//                do! airlock.ShowTerminal <| sprintf "Event %A is not supported in State %A" evt state
//                return! Reactive.skip
//        }
//
//    let stm (airlock : Airlock) : AirlockStateMachine<unit> =
//        statemachine {
//            do! transition airlock
//                |> StateMachine.spoolEvents
//            return ()
        