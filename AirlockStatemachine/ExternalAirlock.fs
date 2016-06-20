// Implementation meant to simulate outside mutable state
// Warning ugly code inside, this is by design, in order to show the power of freckle
module ExternalAirlock
open System.Collections.Concurrent
open System.Threading.Tasks.Dataflow

let eventQueue = BufferBlock()

type BlockImp = { ObjectInUse : bool ref
                  Name : string
                  Status : bool ref
                }
                                         
let doorInner = { ObjectInUse = ref false 
                  Name = "DoorInner"
                  Status = ref false 
                }
            
let doorOuter = { ObjectInUse = ref false
                  Name = "DoorOuter"
                  Status = ref true 
                }

let vent = { ObjectInUse = ref false
             Name = "Vent"
             Status = ref false 
           }
           
let enqueue evt =
    async {
        let! _ =  eventQueue.SendAsync(evt) |> Async.AwaitTask
        return ()
    }

let dequeue =
    async {
        let! msg = eventQueue.ReceiveAsync() |> Async.AwaitTask
        return msg
    }
  
let press =
    async { 
        printfn "button pressed"
        do! enqueue "pressbutton"
    } 


let pressurize =
    async { 
        if !vent.Status then
            failwith "room was already pressuarized YOU HAVE LOST"
                    
        if !vent.ObjectInUse then
            failwith (vent.Name + " already in use YOU HAVE LOST")
                                            
        vent.ObjectInUse.Value <- true
        printfn "@@@ Beginning to pressurize"
        vent.Status := true
        do! Async.Sleep 5000
        vent.ObjectInUse.Value <- false
        printfn "@@@ Room has been pressuarized"
        do! enqueue "pressuarized"
    }

let depressurize =
    async { 
        if not !vent.Status then
            failwith "room was already depressuarized YOU HAVE LOST"
                        
        if !vent.ObjectInUse then
            failwith (vent.Name + " already in use YOU HAVE LOST")

        vent.ObjectInUse.Value <- true
        vent.Status := false
        printfn "@@@ Beginning to depressurize"
        do! Async.Sleep 5000
        printfn "@@@ Room has been depressurized"
        vent.ObjectInUse.Value <- false
        do! enqueue "depressurized"

    }


let open' (door : BlockImp) = 
    async { 
        if !door.Status then
            failwith "door was already open YOU HAVE LOST"
                        
        if !door.ObjectInUse then
            failwith (door.Name + " already in use YOU HAVE LOST")
                            
        door.ObjectInUse.Value <- true                         
        printfn "@@@ Opening %s" door.Name 
        door.Status := true
        do! Async.Sleep 2000
        printfn "@@@ Door %s has opened" door.Name 
        door.ObjectInUse.Value <- false 
        do! enqueue ("opened" + door.Name)
    }
            
let openInner = open' doorInner
let openOuter = open' doorOuter

let close (door : BlockImp) = 
    async { 
        if not !door.Status then
            failwith "door was already closed YOU HAVE LOST"
                        
        if !door.ObjectInUse then
            failwith (door.Name + " already in use YOU HAVE LOST")
                        
        door.ObjectInUse.Value <- true 
        printfn "@@@ Closing %s" door.Name 
        door.Status := false
        do! Async.Sleep 2000
        printfn "@@@ Door %s has closed" door.Name 
        door.ObjectInUse.Value <- false 
        do! enqueue ("closed" + door.Name)
    }

let closeInner = close doorInner
let closeOuter = close doorOuter
