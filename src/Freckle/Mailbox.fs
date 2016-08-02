namespace Freckle

open System

type Expire = Never
            | After of Time

type MailTrigger = delegate of obj * EventArgs -> unit

type Mailbox<'e> = 
    { LiveEvents  : Ref<Feed<'e> * Feed<'e>>
      EventLock   : obj
      MailEvent   : Event<MailTrigger, EventArgs>
      Expiration  : Expire
      Clock       : Clock
    }

module Mailbox =
    open FSharp.Helpers
    open System.Threading

    module Internal =
    
        let inline read' (m : Mailbox<'e>) =
            let (inc, out) = !m.LiveEvents
            Feed.combine inc out

        let inline push' time evt (m : Mailbox<_>) () : unit =
            let (inc, out) = !m.LiveEvents
            match m.Expiration, Feed.tryHead (Feed.time out) with
            | After _, None -> 
                m.LiveEvents := (Feed.empty, Feed.Internal.unsafePush time evt inc)
            | After t, Some outTime when t.Ticks > time.Ticks - outTime.Ticks -> 
                m.LiveEvents := (Feed.Internal.unsafePush time evt inc, Feed.empty)
            | _ -> m.LiveEvents := (Feed.Internal.unsafePush time evt inc, Feed.empty)
            m.MailEvent.Trigger(null, null)
     
    
        
    let createWithExpiration expire clock =
        async {
            return { LiveEvents = ref (Feed.empty, Feed.empty)
                     EventLock = obj()
                     Expiration = expire
                     MailEvent = new Event<_,_>()
                     Clock = clock
                    }
        }
            
    let post evt (m : Mailbox<_>) : Async<unit> =
        async {
            let! time = Clock.now m.Clock
            return lock m.EventLock (Internal.push' time evt m)
        }
        
    let read (m : Mailbox<'e>) : Async<Feed<'e>> = 
        async {
            return Internal.read' m 
        }
        
    let awaitMail (mb : Mailbox<_>) =
        Async.AwaitEvent mb.MailEvent.Publish
        |> Async.Ignore

        
    let listenTo eventStream (mb : Mailbox<_>) =
        async  {
            while true do
                let! evt = eventStream
                do! post evt mb
        } |> Async.StartChild
            |> Async.map ignore

    let dropAll (mb : Mailbox<_>) =
        async {
            return lock mb.EventLock (fun () -> mb.LiveEvents := (Feed.empty, Feed.empty))
        }
