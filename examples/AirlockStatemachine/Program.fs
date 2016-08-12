open FrpAirlockExample
open System
open FSharp.Helpers
open Freckle
open System.Threading.Tasks
open System.Threading

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
                System.Console.SetCursorPosition(40, 3)
                ignore <| printf "STATUS: %s" s
                System.Console.SetCursorPosition(posLeft, pos)
            return lock ExternalAirlock.consoleLock crazy
        }
    
    let writeTime time =
        async {
            let crazy () =
                let pos = System.Console.CursorTop
                let posLeft = System.Console.CursorLeft
                System.Console.SetCursorPosition(40, 1)
                ignore <| printf "Current Time: %s" ((Time.toDateTime time).ToString("yyyy-MM-dd HH:mm:ss.fff", System.Globalization.CultureInfo.InvariantCulture))
                System.Console.SetCursorPosition(posLeft, pos)
            return lock ExternalAirlock.consoleLock crazy
        }

    let writeFps p =
        async {
            let crazy () =
                let pos = System.Console.CursorTop
                let posLeft = System.Console.CursorLeft
                System.Console.SetCursorPosition(40, 2)
                let ticks = ((Period.finish p |> Time.toDateTime) - (Period.beginning p |> Time.toDateTime)) 
                let fps = 1.0 / ticks.TotalSeconds
                ignore <| printfn "Fps: %8.5f | Sampling Delay: %.2f ms" fps ticks.TotalMilliseconds
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
          ShowTerminal = writeConsole >> Async.startFreeChild
          ShowStatus = writeStatus >> Async.startFreeChild
          SetClock = writeTime >> Async.startFreeChild
          SetFps = writeFps  >> Async.startFreeChild
        }
    async {
        printfn "Hi! and welcome to the Airlock example, to start double press enter."
        let! _ = readConsole |> Async.StartChild
        let syncUtcClock = Clock.synchronized Clock.systemUtc
        let! mb = Mailbox.createWithExpiration (Never) syncUtcClock
        do! Mailbox.listenTo events mb
        let state = {  Airlock = AirLockState.IsDepressurized; ActionAt = None; LastDoubleClick = Time.origin }

        let resolution = 
            Async.awaitAny [ Async.pulseMax
                             Mailbox.awaitMail mb
                           ]
        let runner = 
            setup mb airlock
            >> SampleAsync.doAsync resolution
        do! Sample.sampleForever syncUtcClock runner state
    } |> Async.RunSynchronously
    0
