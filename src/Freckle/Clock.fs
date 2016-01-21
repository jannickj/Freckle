[<AutoOpen>]
module Freckle.Clock
open FSharp.Helpers

[<AutoOpen>]
module Types =

    type TimeId = uint64
    type Ticks = int64

    type Time = 
        { Ticks            : Ticks
        } 
        with override x.ToString() = sprintf "%A" x

    type Clock = Clock of Async<Time>
    
module Time =
    open System

    let time t = { Ticks = t; }
    let origin = time 0L
    let ticks t = t.Ticks
    let toDateTime t = DateTime(ticks t)
    let maxValue = { Ticks = Int64.MaxValue  }
    let minValue = { Ticks = Int64.MinValue }
    let ofDateTime (d : DateTime) = time d.Ticks
    let ofMicroseconds (ms : int32) = time <| 10L * int64 ms
    let ofMilliseconds (ms : int32) = time <| TimeSpan.TicksPerMillisecond * int64 ms
    let ofSeconds (sec : int32) = time <| TimeSpan.TicksPerSecond * int64 sec
    let ofMinutes (min : int32) = time <| TimeSpan.TicksPerMinute * int64 min
    let ofHours (hour : int32) = time <| TimeSpan.TicksPerHour * int64 hour
    let ofDays (days : int32) = time <| TimeSpan.TicksPerDay * int64 days
    let between a b =  
        let a' = ticks a
        let b' = ticks b in time ((max a' b') - (min a' b'))

    let delay t withTime = { Ticks = ticks t + ticks withTime }

module Clock =
    open System
    
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

    let systemUtc = ofAsync (async { return DateTime.UtcNow.Ticks })
    let alwaysAt ticks = Clock (async { return ticks })

    let now (Clock m) = m
