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
          ShowStatus    : string -> Async<unit>
          SetClock      : Time -> Async<unit>
          SetFps        : Period -> Async<unit>
        }
     
     type State =
        { Airlock : AirLockState
          ActionAt : Ticks option
          LastDoubleClick     : Time
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
                                                                   
        | _                                     -> (state          , false, Async.doNothing)

   
    let doubleClickTime = Time.ofMilliseconds 500

    let clicks clickFeed =
        feed {
            let! c1 = clickFeed
                     |> Feed.groupBy (fun t1 t2 -> Time.between t1 t2 < doubleClickTime)            
            let len = Feed.testLength 2 c1            
            if len = 2 
            then return DoublePressButton
            else return PressButton
        } 

    let doublePress' last evts =
        let (buttonEvts, others) = Feed.partition ((=) PressButton) evts
        buttonEvts
        |> Feed.discardOlderIncl last
        |> Feed.delay doubleClickTime
        |> Feed.discardFuture
        |> Sample.map (Feed.time >> clicks >> Feed.combine others)

    let airlockProg (airlock : Airlock) now s e =
        async {
            let (airlock, beganActions, ma) = stm airlock s.Airlock e            
            let! _ = ma |> Async.StartChild
            return { s with Airlock = airlock; ActionAt = if beganActions then Some now else s.ActionAt }
        }

    let status airlock s =
        sampleAsync {
            match s.ActionAt with
            | Some ticks when s.Airlock = Pressurizing || s.Airlock = Depressurizing ->
                let pctDone t = min 100.0 <| (float (t - ticks) * 100.0 ) / float (TimeSpan.TicksPerSecond * 9L)
                do! Feed.pulseUpto 30u
                    |> Sample.map (Feed.map (fun t -> airlock.ShowStatus <| sprintf "%A %.2f%%   " s.Airlock (pctDone (Time.ticks t))))
                    |> Sample.bind Feed.plan_                       
            | _ -> return ()
        }
    
    let setup mb airlock s  =       
        sampleAsync {           
            let! p = Sample.period |> SampleAsync.ofSample
            do! status airlock s
            do! Feed.pulseUpto 10u
                |> Sample.map (Feed.map (const' <| airlock.SetClock p.Finish))
                |> Sample.bind Feed.plan_
            do! Feed.pulseUpto 10u
                |> Sample.map (Feed.map (const' <| airlock.SetFps p))
                |> Sample.bind Feed.plan_
            let! evts = Mailbox.read mb |> SampleAsync.ofAsync
            let! evts' =  doublePress' s.LastDoubleClick evts |> SampleAsync.ofSample
            let! last' = Feed.foldPast (fun s (t,a) -> if a = DoublePressButton then t else s) s.LastDoubleClick (Feed.timeStamp evts') |> SampleAsync.ofSample      
            return! Feed.transition (airlockProg airlock p.Finish.Ticks) { s with LastDoubleClick = last' }  evts'
        }