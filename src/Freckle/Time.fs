namespace Freckle
open FSharp.Helpers
open System

///Time is meassured in ticks a which is a dotNet concept 1 tick = 10μs
type Ticks = int64

///Time is the meassurement used for all FRP reasoning
type Time = 
    {   ///Time is meassured in ticks a which is a dotNet concept 1 tick = 10μs
        Ticks            : Ticks
    } 
    with override x.ToString() = sprintf "%A" x
         static member (+) (t1,t2) = { Ticks = t1.Ticks + t2.Ticks }
         static member (-) (t1,t2) = { Ticks = t1.Ticks - t2.Ticks }
         ///Creates a time 
         static member time t = { Ticks = t; }
         
         ///Creates an epoch time
         static member origin = Time.time 0L
         
         ///Get the current tick count
         static member ticks t = t.Ticks
         
         ///Converts time to dotNet's DateTime
         static member toDateTime t = DateTime(Time.ticks t)
         
         ///The max time, tied to DateTime and is the year 9999.
         static member maxValue = { Ticks = DateTime.MaxValue.Ticks  }
         
         ///Converts DateTime to Time
         static member ofDateTime (d : DateTime) = Time.time d.Ticks
         
         ///From microseconds get a time
         static member ofMicroseconds (microSec : int32) = Time.time <| 10L * int64 microSec
         
         ///From milliseconds get a time
         static member ofMilliseconds (ms : int32) = Time.time <| TimeSpan.TicksPerMillisecond * int64 ms
         
         ///From seconds get a time
         static member ofSeconds (sec : int32) = Time.time <| TimeSpan.TicksPerSecond * int64 sec
         
         ///From minutes get a time
         static member ofMinutes (min : int32) = Time.time <| TimeSpan.TicksPerMinute * int64 min
         
         ///From hours get a time
         static member ofHours (hour : int32) = Time.time <| TimeSpan.TicksPerHour * int64 hour
         
         ///From days get a time
         static member ofDays (days : int32) = Time.time <| TimeSpan.TicksPerDay * int64 days
         
         ///Gets the time between two times.
         ///This is not the same as (-) as this operation is cumulative
         static member between a b = 
             let a' = Time.ticks a
             let b' = Time.ticks b
             Time.time ((max a' b') - (min a' b'))

