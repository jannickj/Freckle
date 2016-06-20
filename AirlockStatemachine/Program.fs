open FrpAirlockExample
open System
open FSharp.Helpers
open FSharp.Freckle

[<EntryPoint>]
let main argv = 
    let readConsole =
        async {
            while true do
                let s = System.Console.ReadLine()
                System.Console.SetCursorPosition(0, (System.Console.CursorTop - 1))
                do! ExternalAirlock.press
        }

    let eventMap s =
        match s with
        | "depressurized"   -> Depressurized
        | "pressuarized"    -> Pressurized
        | "openedDoorInner" -> DoorOpened InnerDoor
        | "openedDoorOuter" -> DoorOpened OuterDoor
        | "closedDoorInner" -> DoorClosed InnerDoor
        | "closedDoorOuter" -> DoorClosed OuterDoor
        | "pressbutton"     -> PressButton
        | evt -> failwith <| sprintf "unknown event %s" evt


    let events = Async.map eventMap ExternalAirlock.dequeue
    
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
          ShowTerminal = fun str -> async { return printfn "%s" str |> ignore }
        }
    Async.Start readConsole
    let state = { Click = ClickState None; Airlock = AirLockState.IsDepressurized }
    Freck.execute (setup airlock) state events
    |> Async.RunSynchronously
    printfn "%A" argv
    0 // return an integer exit code
