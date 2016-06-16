﻿open FrpAirlockExample
open System
open FSharp.Helpers
open FSharp.Freckle

[<EntryPoint>]
let main argv = 
    
    let readConsole =
        async {
            while true do
                let s = System.Console.ReadLine()
                do! ExternalAirlock.press
        }

    let eventMap s =
        match s with
        | "depressurized" -> Depressurized
        | "pressuarized" -> Pressurized
        | "openedDoorInner" -> DoorOpened InnerDoor
        | "openedDoorOuter" -> DoorOpened OuterDoor
        | "closedDoorInner" -> DoorClosed InnerDoor
        | "closedDoorOuter" -> DoorClosed OuterDoor
        | evt -> failwith <| sprintf "unknown event %s" evt

    let events = Async.map eventMap ExternalAirlock.dequeue

    let currentTime =
        async {
            return Time (DateTime.UtcNow.Ticks)
        }

    let openDoor door =
        match door with
        | InnerDoor -> ExternalAirlock.openInner
        | OuterDoor -> ExternalAirlock.openOuter
    
    let closeDoor door =
        match door with
        | InnerDoor -> ExternalAirlock.closeInner
        | OuterDoor -> ExternalAirlock.closeOuter
        
    let airlock : Airlock =
        { Open = openDoor
          Close = closeDoor
          Pressurize = ExternalAirlock.pressurize
          Depressurize = ExternalAirlock.depressurize
          ShowTerminal = fun str -> async { return printf "%s" str |> ignore }
        }
        
    Async.Start readConsole
    let state = { Click = ClickState None; Airlock = AirLockState.IsDepressurized }
    Freck.execute (setup airlock) state currentTime events
    |> Async.Start
    printfn "%A" argv
    0 // return an integer exit code
