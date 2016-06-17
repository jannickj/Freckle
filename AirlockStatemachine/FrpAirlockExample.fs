module FrpAirlockExample
    open FSharp.Freckle
    open FSharp
    open System
    open FSharp.Helpers


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

    type Airlock =
        { Open : Door -> Async<unit>
          Close : Door -> Async<unit>
          Pressurize : Async<unit>
          Depressurize : Async<unit> 
          ShowTerminal : string -> Async<unit>
        }
     
     type ClickState = ClickState of DateTime option
     type State =
        { Click   : ClickState
          Airlock : AirLockState
        }
    

    let stm (airlock : Airlock) state event =
        match state, event with
        | IsPressurized  , PressButton          -> (Depressurizing , airlock.Close InnerDoor)
        | Depressurizing , DoorClosed InnerDoor -> (Depressurizing , airlock.Depressurize)
        | Depressurizing , Depressurized        -> (Depressurizing , airlock.Open OuterDoor)
        | Depressurizing , DoorOpened OuterDoor -> (IsDepressurized, Async.doNothing)

        | IsDepressurized, PressButton          -> (Pressurizing   , airlock.Close OuterDoor)
        | Pressurizing   , DoorClosed OuterDoor -> (Pressurizing   , airlock.Pressurize)
        | Pressurizing   , Pressurized          -> (Pressurizing   , airlock.Open InnerDoor)        
        | Pressurizing   , DoorOpened InnerDoor -> (IsPressurized  , Async.doNothing)

        | _                                     -> (state          , Async.doNothing)

    
    let doubleClickTime = TimeSpan.FromMilliseconds 500.0

    let isDoubleClick (ClickState cs) (time,e) =
        match cs with
        | Some lastTime when time - lastTime < doubleClickTime -> (ClickState None, Some e)
        | _ -> (ClickState <| Some time, None)
    
    let doublePress evts clickState =
        let (buttonEvts, others) = Freck.partition ((=) PressButton) evts
        let sndOpt (s,d) = Option.map (fun d' -> (s,d')) d
        Freck.dateTimed buttonEvts
        |> Freck.mapFoldNow isDoubleClick clickState
        |> Freck.choose sndOpt
        |> Freck.merge (fun (s, _) b -> (s, b)) others

        
    let airlockProg (airlock : Airlock) s (cs,e) =
        async {
            let (airlock, ma) = stm airlock s.Airlock e
            do! ma
            return { s with Airlock = airlock; Click = cs }
        }
    
    let setup airlock evts s =
        doublePress evts s.Click
        |> Freck.transitionNow (airlockProg airlock) s