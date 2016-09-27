namespace Freckle

open System

///The mailbox is the concept of messages passed from external parts of the system
///A mailbox should preferably should only have one consumer and one or more producers
type Mailbox<'e> = 
    { Post : 'e -> unit
      Read : Sample<Feed<'e>>
      Clear : unit -> unit
      AwaitMail : Async<unit>
    }

///The Mailbox module contains all functions to work with external events, such as post, read and suc.
module Mailbox =
    open FSharp.Helpers
    open System.Threading
    open SampleAsync.ComputationalExpression
    open Sample.ComputationalExpression

    ///This module contains internally used functions, these are suceptible to change even with minor updates. 
    ///Is not recommended for use in a production environment.
    module Internal =

        ///Automatically delete events from a mailbox by setting an expiration
        type Expire = Never
                    | After of Time
        
        type MailTrigger = delegate of obj * EventArgs -> unit

        let inline read' mLock (m : Ref<Feed<'e> * Feed<'e>>) =
            let (inc, out) = lock mLock (fun _ -> !m)
            Feed.combine inc out

        let inline push' time evt (trigger : Event<MailTrigger, EventArgs>) (expire : Expire)  (m : Ref<Feed<'e> * Feed<'e>>) : unit =
            let (inc, out) = !m
            match expire, Feed.tryHead (Feed.time out) with
            | After _, None -> 
                m := (Feed.empty, Feed.Internal.unsafePush time evt inc)
            | After t, Some outTime when t.Ticks < time.Ticks - outTime.Ticks -> 
                m := (Feed.Internal.unsafePush time evt inc, Feed.empty)
            | _ -> m := (Feed.Internal.unsafePush time evt inc, out)
            trigger.Trigger(null, null)
                    
        let post mLock clock trigger expire evts evt =
            lock mLock (fun () -> let time = Clock.nowSynced clock in push' time evt trigger expire evts)

        let dropAll mLock (evts : Ref<Feed<'e> * Feed<'e>>) () =
            lock mLock (fun () -> evts := (Feed.empty, Feed.empty))

        let awaitMail (evtTrigger : Event<MailTrigger, EventArgs>) =
            Async.AwaitEvent evtTrigger.Publish
            |> Async.Ignore

        let create expire clock =
            let evts = ref (Feed.empty, Feed.empty)
            let mlock = new obj()
            let evtTrigger = new Event<_,_>()
            { Post = post mlock clock evtTrigger expire evts
                     Read = sample { return read' mlock evts }
                     Clear = dropAll mlock evts
                     AwaitMail = awaitMail evtTrigger
            }

    /// Creates a mailbox used for sending and recieving events between different samplers and/or outside state, events has a time they live before they are cleaned
    let createWithTTL time clock =
        Internal.create (Internal.Expire.After time) clock

    /// Creates a mailbox used for sending and recieving events between different samplers and/or outside state, NB: has to be cleared manually or timeleaks will occur, otherwise use createWithTTL
    let create clock =
        Internal.create (Internal.Expire.Never) clock

    // Reads from an mailbox synchronized
    let inline readSync (mb : Mailbox<_>) = mb.Read  |> Sample.bind Feed.discardFuture

    /// Post an event to a mailbox
    let inline postSync evt (mb : Mailbox<_>) = mb.Post evt
        
    let inline post evt (mb : Mailbox<_>) = async { return mb.Post evt }
    
    // Post an event not yet realized
    let inline postPromise (ma : Async<_>) mb =
        async {
            let! a = ma
            do! post a mb
        } |> Async.StartChild |> Async.Ignore

    // Post an event not yet realized, but start immidately
    let inline postPromiseSilent (ma : Async<_>) mb =
        postPromise ma mb |> Async.StartImmediate

    /// Post an event to many mailboxes
    let inline postMany evt mbs = 
        async {
            for mb in mbs do
                do! post evt mb
        }

    /// Read all events from a mailbox
    let inline read (mb : Mailbox<_>) = 
        sampleAsync {
            return! mb.Read 
                    |> Sample.bind Feed.discardFuture 
                    |> SampleAsync.ofSample
        }

    /// Discard all events currently in mailbox
    let inline clear (mb : Mailbox<_>) = mb.Clear

    /// wait until a mailbox recieves an event
    let inline awaitMail (mb : Mailbox<_>) = mb.AwaitMail
                
    /// setup a mailbox for listening to an external event stream
    let inline listenTo eventStream (mb : Mailbox<_>) =
        async  {
            while true do
                let! evt = eventStream
                do! post evt mb
        } |> Async.StartChild
            |> Async.map ignore
    
    /// setup a mailbox for listening to an external event stream
    let inline listenTo' (mb : Mailbox<_>) eventStream = listenTo eventStream mb

    /// setup a mailbox for listening to an external dotnet event
    let inline listenToEvent event (mb : Mailbox<_>) =
        listenTo (event |> Async.AwaitEvent) mb

    /// etup a mailbox for listening to an dotnet event and perform a map before being posted to the mailbox
    let inline ListenToMappedEvent f event (mb : Mailbox<_>) =
        listenTo (event |> Async.AwaitEvent |> Async.map f) mb
    
