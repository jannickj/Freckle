open FrpAirlockExample
open System
open FSharp.Helpers
open Freckle

[<EntryPoint>]
let main argv = 
    
    let writeConsole s =
        async {
            return ignore <| ExternalAirlock.printfn' "%s" s        
        }

    let writeStatus s =
        async {
            let crazy () =
                let pos = System.Console.CursorTop
                let posLeft = System.Console.CursorLeft
                System.Console.SetCursorPosition(40, 1)
                ignore <| printf "STATUS: %s" s
                System.Console.SetCursorPosition(posLeft, pos)
            return lock ExternalAirlock.consoleLock crazy
        }
    
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
        { Open = openDoor
          Close = closeDoor
          Pressurize = ExternalAirlock.pressurize
          Depressurize = ExternalAirlock.depressurize
          ShowTerminal = writeConsole
          ShowStatus = writeStatus
        }
    async {
        printfn "Hi! and welcome to the Airlock example, to start double press enter."
        let! _ = readConsole |> Async.StartChild
        let! mb = Mailbox.createWithExpiration (Never) Clock.systemUtc
        do! Mailbox.listenTo events mb
        let state = {  Airlock = AirLockState.IsDepressurized; ActionAt = None; LastDoubleClick = Time.origin }
        let runner = 
            setup mb airlock
            >> SampleAsync.doAsync (Async.Sleep 1)
        do! Sampling.sampleForever Clock.systemUtc state runner
    } |> Async.RunSynchronously
    0 // return an integer exit code
