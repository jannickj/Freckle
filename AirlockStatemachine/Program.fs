open FrpAirlockExample
open System
open FSharp.Helpers
open Freckle

[<EntryPoint>]
let main argv = 
    
    let writeConsole s =
        async {
            return ignore <| ExternalAirlock.printfn' "%s" s        
        } |> Act.ofAsync

    let writeStatus s =
        async {
            let crazy () =
                let pos = System.Console.CursorTop
                let posLeft = System.Console.CursorLeft
                System.Console.SetCursorPosition(40, 0)
                ignore <| printf "STATUS: %s" s
                System.Console.SetCursorPosition(posLeft, pos)
            return lock ExternalAirlock.consoleLock crazy
        } |> Act.ofAsync
    
    let readConsole =
        async {
            while true do
                let s = System.Console.ReadLine()
                lock ExternalAirlock.consoleLock (fun () -> System.Console.SetCursorPosition(0, (System.Console.CursorTop - 1)))
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
          ShowTerminal = writeConsole
          ShowStatus = writeStatus
        }
    async {

        let! _ = readConsole |> Async.StartChild
        let! mb = Mailbox.create (Clock.systemUtc)
        do! Mailbox.listenTo events mb
        let state = { Click = ClickState None; Airlock = AirLockState.IsDepressurized; ActionAt = None }
        do! Act.runRecursive mb (setup airlock) state
    } |> Async.RunSynchronously
    0 // return an integer exit code
