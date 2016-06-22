module FrpAirlockExample
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
        { Open          : Door -> Async<unit>
          Close         : Door -> Async<unit>
          Pressurize    : Async<unit>
          Depressurize  : Async<unit> 
          ShowTerminal  : string -> Async<unit>
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

        | _                                     -> (state          , Async.doNothing)

    
    let doubleClickTime = TimeSpan.FromMilliseconds 500.0

    let isDoubleClick (ClickState cs) (time,e) =
        match cs with
        | Some lastTime when time - lastTime < doubleClickTime -> (ClickState None, DoublePressButton)
        | _ -> (ClickState <| Some time, PressButton)
    
    let doublePress now clickState evts =
        let (buttonEvts, others) = Freck.partition ((=) PressButton) evts
        let sndOpt (s,d) = Option.map (fun d' -> (s,d')) d
        Freck.dateTimed buttonEvts
        |> Freck.mapFold now isDoubleClick clickState
        |> Freck.weave (fun a b -> (Option.mapDefault clickState fst a, b)) others


   
    let airlockProg (airlock : Airlock) s (cs, e) =
        async {
            let (airlock, ma) = stm airlock s.Airlock e
            let! _ = ma
            return { s with Airlock = airlock; Click = cs }
        }
    
    let setup airlock now evts s  =        
        (Freck.Debug.trace evts)
        |> doublePress now s.Click
        |> Freck.transition now (airlockProg airlock) s