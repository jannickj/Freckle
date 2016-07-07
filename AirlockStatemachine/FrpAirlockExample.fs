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
          ActionAt : Ticks option
        }
    

    let stm (airlock : Airlock) state event =
        match state, event with
        | IsPressurized  , DoublePressButton    -> (Depressurizing , true, airlock.Close InnerDoor)
        | Depressurizing , DoorClosed InnerDoor -> (Depressurizing , false, airlock.Depressurize)
        | Depressurizing , Depressurized        -> (Depressurizing , false, airlock.Open OuterDoor)
        | Depressurizing , DoorOpened OuterDoor -> (IsDepressurized, false, airlock.ShowTerminal "Depressurized room")
                                                                   
        | IsDepressurized, DoublePressButton    -> (Pressurizing   , true, airlock.Close OuterDoor)
        | Pressurizing   , DoorClosed OuterDoor -> (Pressurizing   , false, airlock.Pressurize)
        | Pressurizing   , Pressurized          -> (Pressurizing   , false, airlock.Open InnerDoor)        
        | Pressurizing   , DoorOpened InnerDoor -> (IsPressurized  , false, airlock.ShowTerminal "Pressurized room")
                                                                   
        | _                                     -> (state          , false, Act.doNothing)

    
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

//    let doublePress now clickState evts =
//        let (buttonEvts, others) = Feed.partition ((=) PressButton) evts
//        let  doubleClicks = Feed.dateTimed buttonEvts
//                            |> Feed.mapFold now isDoubleClick clickState
//        feed {
//            let! b = others
//            let! (s, a) = doubleClicks
//            return (s, b)
//        }
//        |> Feed.combine doubleClicks

    let airlockProg (airlock : Airlock) s (cs, e) =
        act {
            let (airlock, beganActions, ma) = stm airlock s.Airlock e
            let! _ = ma |> Act.startChild
            let! now = Act.now
            return { s with Airlock = airlock; Click = cs; ActionAt = if beganActions then Some now.Current.Ticks else s.ActionAt }
        }

    let status airlock s =
        act {
            match s.ActionAt with
            | Some ticks when s.Airlock = Pressurizing || s.Airlock = Depressurizing ->
                let! p = Act.pulse 2u
                let pctDone t = min 100.0 <| (float (t - ticks) * 100.0 ) / float (TimeSpan.TicksPerSecond * 9L)
                let! _ = Feed.planNow (Feed.map (fun t -> airlock.ShowStatus <| sprintf "%A %.2f%%   " s.Airlock (pctDone (Time.ticks t)))  p)
                return ()
            | _ -> return ()
        }
    
    let setup airlock s  =       
        act {
            do! status airlock s
            let! evts = Act.react
            let! now= Act.now
            let! s =  evts
                      |> doublePress now s.Click
                      |> Feed.transitionNow (airlockProg airlock) s
            return s
        }