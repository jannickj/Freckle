﻿module FrpAirlockExample
    open FSharp
    open System
    open FSharp.Helpers
    open Freckle


    type Door = InnerDoor
              | OuterDoor

    type DoorStatus = Opened
                    | Closed
    
    type AirLockEvent = PressButton
                      | DoublePressButton
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
        { Open          : Door -> Act<unit>
          Close         : Door -> Act<unit>
          Pressurize    : Act<unit>
          Depressurize  : Act<unit> 
          ShowTerminal  : string -> Act<unit>
          ShowStatus    : string -> Act<unit>
        }
     
     type ClickState = ClickState of DateTime option
     type State =
        { Click   : ClickState
          Airlock : AirLockState
        }
    

    let stm (airlock : Airlock) state event =
        match state, event with
        | IsPressurized  , DoublePressButton    -> (Depressurizing , airlock.Close InnerDoor)
        | Depressurizing , DoorClosed InnerDoor -> (Depressurizing , airlock.Depressurize)
        | Depressurizing , Depressurized        -> (Depressurizing , airlock.Open OuterDoor)
        | Depressurizing , DoorOpened OuterDoor -> (IsDepressurized, airlock.ShowTerminal "Depressurized room")

        | IsDepressurized, DoublePressButton    -> (Pressurizing   , airlock.Close OuterDoor)
        | Pressurizing   , DoorClosed OuterDoor -> (Pressurizing   , airlock.Pressurize)
        | Pressurizing   , Pressurized          -> (Pressurizing   , airlock.Open InnerDoor)        
        | Pressurizing   , DoorOpened InnerDoor -> (IsPressurized  , airlock.ShowTerminal "Pressurized room")

        | _                                     -> (state          , Act.doNothing)

    
    let doubleClickTime = TimeSpan.FromMilliseconds 500.0

    let isDoubleClick (ClickState cs) (time,e) =
        match cs with
        | Some lastTime when time - lastTime < doubleClickTime -> (ClickState None, DoublePressButton)
        | _ -> (ClickState <| Some time, PressButton)
    
    let doublePress now clickState evts =
        let (buttonEvts, others) = Feed.partition ((=) PressButton) evts
        let sndOpt (s,d) = Option.map (fun d' -> (s,d')) d
        Feed.dateTimed buttonEvts
        |> Feed.mapFold now isDoubleClick clickState
        |> Feed.weave (fun a b -> (Option.mapDefault clickState fst a, b)) others
        

   
    let airlockProg (airlock : Airlock) s (cs, e) =
        act {
            let (airlock, ma) = stm airlock s.Airlock e
            let! _ = ma |> Act.startChild
            return { s with Airlock = airlock; Click = cs }
        }

    let status p airlock s =
        act {
            match s with
            | Pressurizing ->
                let! _ = Feed.planNow (Feed.map (fun t -> airlock.ShowStatus <| sprintf "Pressurizing %d" (Time.ticks t))  p)
                return ()
            | Depressurizing ->
                let! _ = Feed.planNow (Feed.map (fun t -> airlock.ShowStatus <| sprintf "Depressurizing %d" (Time.ticks t))  p)
                return ()
            | _ -> return () 
        }
    
    let setup airlock s  =       
        act {
            let! p = 
                match s.Airlock with
                | Pressurizing ->
                    Act.pulse 30u
                | _ -> Act.pure' (Feed.empty)

            printfn "%A" p
            let! evts = Act.react
            let! now= Act.now
            let! s =  evts
                      |> doublePress now s.Click
                      |> Feed.transitionNow (airlockProg airlock) s
            return s
        }