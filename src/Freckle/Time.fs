[<AutoOpen>]
///Time is the meassurement used for all FRP reasoning
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
             static member (+) (t1,t2) = { Ticks = t1.Ticks + t2.Ticks }
             static member (-) (t1,t2) = { Ticks = t1.Ticks - t2.Ticks }

///Time is the meassurement used for all FRP reasoning
module Time =
    open System

    ///Creates a time 
    let time t = { Ticks = t; }

    //Creates an epoch time
    let origin = time 0L

    ///Get the current tick count
    let ticks t = t.Ticks

    ///Converts time to dotNet's DateTime
    let toDateTime t = DateTime(ticks t)

    ///The max time, tied to DateTime and is the year 9999.
    let maxValue = { Ticks = DateTime.MaxValue.Ticks  }

    ///Converts DateTime to Time
    let ofDateTime (d : DateTime) = time d.Ticks
    
    ///From microseconds get a time
    let ofMicroseconds (microSec : int32) = time <| 10L * int64 microSec

    ///From milliseconds get a time
    let ofMilliseconds (ms : int32) = time <| TimeSpan.TicksPerMillisecond * int64 ms

    ///From seconds get a time
    let ofSeconds (sec : int32) = time <| TimeSpan.TicksPerSecond * int64 sec

    ///From minutes get a time
    let ofMinutes (min : int32) = time <| TimeSpan.TicksPerMinute * int64 min

    ///From hours get a time
    let ofHours (hour : int32) = time <| TimeSpan.TicksPerHour * int64 hour

    ///From days get a time
    let ofDays (days : int32) = time <| TimeSpan.TicksPerDay * int64 days

    ///Gets the time between two times
    ///Note: This operation is cumulative
    let between a b =  
        let a' = ticks a
        let b' = ticks b in time ((max a' b') - (min a' b'))


