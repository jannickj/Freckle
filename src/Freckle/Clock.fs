﻿[<AutoOpen>]
///Clock is used to generate the current time
module Freckle.Clock
open FSharp.Helpers

[<AutoOpen>]
///The includes the clock type
module Types =
    
    ///The clock type, representing a clock that can generate the current time.
    ///To be used for FRP it must guarantee a that time is changed when time has actually changed (e.g. DateTime.Now does not guarantee this)
    type Clock = Clock of Async<Time>
    
///Clock is used to generate the current time
module Clock =
    open System
    
    ///Generates a clock based of async generating a new time.
    ///If the time generated is the same as last then the time will be incremented by 10μs
    let ofAsync ma =
        let last = ref Time.origin
        let ma' =
            async {
                let! time = ma
                return lock last (fun () ->
                                    let newT =
                                        if Time.ticks !last >= time 
                                        then { Ticks = Time.ticks !last + 1L }
                                        else Time.time time
                                    last := newT
                                    newT)
                
            }
        Clock ma'

    ///A system clock, is recommended to just always use this
    let systemUtc = ofAsync (async { return DateTime.UtcNow.Ticks })

    ///A clock that always return the same time, mainly for testing purposes
    let alwaysAt ticks = Clock (async { return ticks })

    ///Get the current time
    let now (Clock m) = m