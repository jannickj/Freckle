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

    type Knowledge = int
    type Time = DateTime
    type AirlockEvent = Events<Time, AirLockEvent>
    type AirlockBehavior<'a>     = Behavior<Time, Knowledge,'a>
    type AirlockReactive<'a>     = Reactive<AirLockEvent, Time, Knowledge,'a>
    type AirlockStateMachine<'a> = StateMachine<AirLockState, AirLockEvent, Time, Knowledge,'a>
    
    module Wishes =        
        module Reactive =
            let react : AirlockReactive<AirLockEvent> = undefined 
            let skip<'a> : AirlockReactive<'a> = fun _ _ k -> (k,None)

        module StateMachine =
            let spoolEvents<'s> : ('s -> AirlockReactive<'s>) -> StateMachine<'s, AirLockEvent, Time, Knowledge,unit> = undefined
            let spoolTime<'s> : ('s -> AirlockBehavior<'s>) -> StateMachine<'s, AirLockEvent, Time, Knowledge,unit> = undefined

    open Wishes

    type Airlock =
        { OpenDoor : Door -> AirlockBehavior<unit>
          OpenClose : Door -> AirlockBehavior<unit>
          Pressurize : AirlockBehavior<unit>
          Depressurize : AirlockBehavior<unit> 
          ShowTerminal : string -> AirlockBehavior<unit> 
        }

    let transition (airlock : Airlock) state : AirlockReactive<AirLockState> =
        reactive {
            let! evt = Reactive.react
            match evt, state with
            | PressButton, IsPressurized ->
                do! airlock.Depressurize
                return Depressurizing

            | PressButton, IsDepressurized -> 
                do! airlock.Pressurize
                return Pressurizing

            | Pressurized, Pressurizing -> 
                do! airlock.OpenDoor InnerDoor
                return IsPressurized

            | Depressurized, Depressurizing -> 
                do! airlock.OpenDoor OuterDoor
                return IsDepressurized

            | _ -> 
                do! airlock.ShowTerminal <| sprintf "Event %A is not supported in State %A" evt state
                return! Reactive.skip
        }

    let stm (airlock : Airlock) : AirlockStateMachine<unit> =
        statemachine {
            do! transition airlock
                |> StateMachine.spoolEvents
            return ()
        }
        
[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    0 // return an integer exit code
