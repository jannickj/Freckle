[<AutoOpen>]
module Freckle.Mailbox
open FSharp.Helpers

[<AutoOpen>]
module Types =
    open System.Threading

    type Clock = Clock of Async<Ticks>
    type EventSource = EventSource of Time * Map<SortedType, Feed<obj>>
    type Mailbox = 
        { LiveEvents  : Ref<EventSource> 
          EventLock   : AutoResetEvent
          Clock       : Clock
          MailAwait   : AutoResetEvent
        }

module Clock =
    open System
    
    let systemUtc = Clock (async { return DateTime.UtcNow.Ticks })
    let alwaysAt ticks = Clock (async { return ticks })

    let now (Clock m) = m

module EventSource =
    
    let empty = EventSource (Time.origin, Map.empty)

    let lastEventTime (EventSource (t,_)) = t

    let read<'e> (EventSource (_,es) : EventSource) : Feed<'e> = 
        let t = typeof<'e>
        let st = SortedType(t)
        match Map.tryFind st es with
        | Some fr -> Feed.map unbox fr
        | None -> Feed.empty

    let push<'e> ticks (evt: 'e) (EventSource (oldTime,es) : EventSource) : EventSource = 
        let t = typeof<'e>
        let st = SortedType(t)
        let time = Time.incId ticks oldTime
        match Map.tryFind st es with
        | Some fr -> EventSource (time, Map.add st (Feed.Internal.unsafePush time (box evt) fr) es)
        | None -> EventSource (time, Map.add st (Feed.singleton time (box evt)) es)
   
    let setTimeId tick evtsource =
        let evtTime = lastEventTime evtsource
        Time.incId tick evtTime
        
 
[<AutoOpen>]
module Core =
    module Mailbox =
        open System.Threading
        
        let currentTick (mb : Mailbox) = Clock.now mb.Clock

        let create clock =
            async {
                let ars = new AutoResetEvent(true)
                let mawait = new AutoResetEvent(false)
                let! _ = Async.OnCancel (fun () -> ars.Dispose(); mawait.Dispose())
                return { LiveEvents = ref EventSource.empty
                         EventLock = ars
                         Clock = clock
                         MailAwait = mawait
                       }
            }

        let post evt (mb : Mailbox) =
            async {
                AutoResetEvent.wait mb.EventLock
                let! time = currentTick mb
                mb.LiveEvents := EventSource.push time evt !mb.LiveEvents
                AutoResetEvent.release mb.EventLock
                AutoResetEvent.release mb.MailAwait
            }
        
        let awaitMailTimeout (ticks : Ticks) (mb : Mailbox) =
            async {
                return mb.MailAwait.WaitOne(System.TimeSpan(ticks)) |> ignore
            }

        let awaitMail (mb : Mailbox) =
            async {
                return mb.MailAwait.WaitOne() |> ignore
            }

        let receive (mb : Mailbox) = async { return !mb.LiveEvents }
        
        let listenTo eventStream (mb : Mailbox) =
            async  {
                while true do
                    let! evt = eventStream
                    do! post evt mb
            } |> Async.StartChild
              |> Async.map ignore