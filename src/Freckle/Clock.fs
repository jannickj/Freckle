[<AutoOpen>]
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
    
    ///Generates a clock guaranteed to always be sequential.
    ///This is accomplished by the fact that if the time generated is the same as last then the time will be incremented by 10μs
    let synchronized (Clock ma) =
        let last = ref Time.origin
        let ma' =
            async {
                let! time = ma
                return lock last (fun () ->
                                    let newT =
                                        if !last >= time 
                                        then { Ticks = Time.ticks !last + 1L }
                                        else time
                                    last := newT
                                    newT)
                
            }
        Clock ma'

    ///A system clock, is recommended to just always use this
    let systemUtc = Clock (async { return Time.ofDateTime DateTime.UtcNow })

    ///A clock that always return the same time, mainly for testing purposes
    let alwaysAt ticks = Clock (async { return ticks })

    ///Get the current time
    let now (Clock m) = m
