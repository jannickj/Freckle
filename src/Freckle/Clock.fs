[<AutoOpen>]
module Freckle.Clock
open FSharp.Helpers

[<AutoOpen>]
module Types =
    open System

    type TimeId = uint64
    type Ticks = int64

    type Time = 
        { Ticks            : Ticks
          Infinitesimal    : TimeId
        }
        with static member time t = { Ticks = t; Infinitesimal = 0UL }
             static member origin = Time.time 0L
             static member ticks t = t.Ticks           
             static member toDateTime t = DateTime(Time.ticks t)
             static member maxValue = { Ticks = Int64.MaxValue; Infinitesimal = UInt64.MaxValue }
             static member minValue = { Ticks = Int64.MinValue; Infinitesimal = UInt64.MinValue }
             override x.ToString() = sprintf "%A" x

    type Clock = Clock of Async<Time>

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
                                        then { Ticks = Time.ticks !last; Infinitesimal = (!last).Infinitesimal + 1UL }
                                        else Time.time time
                                    last := newT
                                    newT)
                
            }
        Clock ma'

    let systemUtc = ofAsync (async { return DateTime.UtcNow.Ticks })
    let alwaysAt ticks = Clock (async { return ticks })

    let now (Clock m) = m
