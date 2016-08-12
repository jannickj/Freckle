namespace Freckle

open System

///Automatically delete events from a mailbox by setting an expiration
type Expire = Never
            | After of Time

            
///The mailbox is the concept of messages passed from external parts of the system
///A mailbox should preferably should only have one consumer and one or more producers
type Mailbox<'e> = 
    { Post : 'e -> Async<unit>
      Read : Async<Feed<'e>>
      Clear : Async<unit>
      AwaitMail : Async<unit>
    }

///The Mailbox module contains all functions to work with external events, such as post, read and suc.
module Mailbox =
    open FSharp.Helpers
    open System.Threading

    ///This module contains internally used functions, these are suceptible to change even with minor updates. 
    ///Is not recommended for use in a production environment.
    module Internal =
        
        type MailTrigger = delegate of obj * EventArgs -> unit

        let inline read' (m : Ref<Feed<'e> * Feed<'e>>) =
            async {
                let (inc, out) = !m
                return Feed.combine inc out
            }

        let inline push' time evt (trigger : Event<MailTrigger, EventArgs>) (expire : Expire)  (m : Ref<Feed<'e> * Feed<'e>>) () : unit =
            let (inc, out) = !m
            match expire, Feed.tryHead (Feed.time out) with
            | After _, None -> 
                m := (Feed.empty, Feed.Internal.unsafePush time evt inc)
            | After t, Some outTime when t.Ticks > time.Ticks - outTime.Ticks -> 
                m := (Feed.Internal.unsafePush time evt inc, Feed.empty)
            | _ -> m := (Feed.Internal.unsafePush time evt inc, Feed.empty)
            trigger.Trigger(null, null)
                    
        let post mLock clock trigger expire evts evt : Async<unit> =
            async {
                let! time = Clock.now clock
                return lock mLock (push' time evt trigger expire evts)
            }

        let dropAll mLock (evts : Ref<Feed<'e> * Feed<'e>>) =
            async { return lock mLock (fun () -> evts := (Feed.empty, Feed.empty)) }

        let awaitMail (evtTrigger : Event<MailTrigger, EventArgs>) =
            Async.AwaitEvent evtTrigger.Publish
            |> Async.Ignore

    /// Creates a mailbox used for sending and recieving events between different samplers and/or outside state
    let createWithExpiration expire clock =
        async {
            let evts = ref (Feed.empty, Feed.empty)
            let mlock = new obj()
            let evtTrigger = new Event<_,_>()
            return { Post = Internal.post mlock clock evtTrigger expire evts
                     Read = Internal.read' evts
                     Clear = Internal.dropAll mlock evts
                     AwaitMail = Internal.awaitMail evtTrigger
                   }
        }        

    /// Post an event to a mailbox
    let post evt (mb : Mailbox<_>) = mb.Post evt

    /// Post an event to many mailboxes
    let postMany evt mbs = 
        async {
            for mb in mbs do
                do! post evt mb
        }

    /// Read all events from a mailbox
    let read (mb : Mailbox<_>) = mb.Read

    /// Discard all events currently in mailbox
    let clear (mb : Mailbox<_>) = mb.Clear

    /// wait until a mailbox recieves an event
    let awaitMail (mb : Mailbox<_>) = mb.AwaitMail
                
    /// setup a mailbox for listening to an external event stream
    let listenTo eventStream (mb : Mailbox<_>) =
        async  {
            while true do
                let! evt = eventStream
                do! post evt mb
        } |> Async.StartChild
            |> Async.map ignore

    
