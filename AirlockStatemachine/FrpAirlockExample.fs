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

    type Action =
        | DoNothing
        | Pressurize
        | Depressurize
        | Open of Door
        | Close of Door

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
//    type Airlock =
//        { OpenDoor : Door -> AirlockReactive<unit>
//          OpenClose : Door -> AirlockReactive<unit>
//          Pressurize : AirlockReactive<unit>
//          Depressurize : AirlockReactive<unit> 
//          ShowTerminal : string -> AirlockReactive<unit> 
//        }
    
//    let tt  () : AirlockReactive<int> =        
//        Reactive.event
//        |> Reactive.filter ((=) (DoorOpened InnerDoor))
//        |> Reactive.map (const' 1)        
//        |> Reactive.foldp (+) 0
//    
    let events : Recorder<AirLockState * AirLockEvent> = undefined

    let stm state event =
        match state, event with
        | IsPressurized  , PressButton          -> (Depressurizing , Close InnerDoor)
        | Depressurizing , DoorClosed InnerDoor -> (Depressurizing , Depressurize)
        | Depressurizing , Depressurized        -> (Depressurizing , Open OuterDoor)
        | Depressurizing , DoorOpened OuterDoor -> (IsDepressurized, DoNothing)

        | IsDepressurized, PressButton          -> (Pressurizing   , Close OuterDoor)
        | Pressurizing   , DoorClosed OuterDoor -> (Pressurizing   , Pressurize)
        | Pressurizing   , Pressurized          -> (Pressurizing   , Open InnerDoor)        
        | Pressurizing   , DoorOpened InnerDoor -> (IsPressurized  , DoNothing)

        | _                                     -> (state          , DoNothing)

    let actions =
        Recorder.map (fun (s,e) -> stm s e) <| events

    let program () =
        


        ()

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
        