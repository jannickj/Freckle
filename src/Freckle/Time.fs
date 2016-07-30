[<AutoOpen>]
module Freckle.Time
open FSharp.Helpers

[<AutoOpen>]
module Types =

    ///Time is meassured in ticks a which is a dotNet concept 1 tick = 10μs
    type Ticks = int64

    ///Time the base unit for all time meassurements for FRP
    type Time = 
        { Ticks            : Ticks
        } 
        with override x.ToString() = sprintf "%A" x
            
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

