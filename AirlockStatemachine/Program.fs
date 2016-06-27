open FrpAirlockExample
open System
open FSharp.Helpers
open Freckle

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
        { Open = openDoor >> Act.ofAsync
          Close = closeDoor >> Act.ofAsync
          Pressurize = ExternalAirlock.pressurize |> Act.ofAsync
          Depressurize = ExternalAirlock.depressurize |> Act.ofAsync
          ShowTerminal = fun str -> act { return printfn "%s" str |> ignore }
        }
    async {

        let! _ = readConsole |> Async.StartChild
        let! mb = Mailbox.create (Clock.systemUtc)
        do! Mailbox.listenTo events mb
        let state = { Click = ClickState None; Airlock = AirLockState.IsDepressurized }
        do! Act.runRecursive mb (setup airlock) state
    } |> Async.RunSynchronously
    0 // return an integer exit code
