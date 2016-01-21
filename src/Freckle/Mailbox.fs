namespace Freckle
[<AutoOpen>]
module Mailbox =
    open FSharp.Helpers

    [<AutoOpen>]
    module Types =
        open System.Threading

        type Expire = Never
                    | After of Time
                    | WhenRead

        type Mailbox<'e> = 
            { LiveEvents  : Ref<Feed<'e> * Feed<'e>>
              EventLock   : obj
              MailAwait   : AutoResetEvent
              Expiration  : Expire
              Clock       : Clock
            }


    module Internal =
    
        let inline read' (m : Mailbox<'e>) =
            let (inc, out) = !m.LiveEvents
            Feed.combine inc out

        let inline readAndRemove' (m : Mailbox<'e>) () =
            let (inc, out) = !m.LiveEvents
            m.LiveEvents := (Feed.empty, Feed.empty)
            Feed.combine inc out
            

        let inline push' time evt (m : Mailbox<_>) () : unit =
            let (inc, out) = !m.LiveEvents
            match m.Expiration, Feed.tryHead (Feed.time out) with
            | After _, None -> 
                m.LiveEvents := (Feed.empty, Feed.Internal.unsafePush time evt inc)
            | After t, Some outTime when t.Ticks > time.Ticks - outTime.Ticks -> 
                m.LiveEvents := (Feed.Internal.unsafePush time evt inc, Feed.empty)
            | _ -> m.LiveEvents := (Feed.Internal.unsafePush time evt inc, Feed.empty)
            AutoResetEvent.release m.MailAwait

 
    [<AutoOpen>]
    module Core =
        module Mailbox =
            open System.Threading
        
            let createWithExpiration expire clock =
                async {
                    let ars = new AutoResetEvent(true)
                    let mawait = new AutoResetEvent(false)
                    let! _ = Async.OnCancel (fun () -> ars.Dispose(); mawait.Dispose())
                    return { LiveEvents = ref (Feed.empty, Feed.empty)
                             EventLock = ars
                             Expiration = expire
                             MailAwait = mawait
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
                    match m.Expiration with
                    | WhenRead -> return lock m.EventLock (Internal.readAndRemove' m)
                    | _ -> return Internal.read' m 
                }

            let awaitMailTimeout (ticks : Ticks) (mb : Mailbox<_>) =
                async {
                    return mb.MailAwait.WaitOne(System.TimeSpan(ticks)) |> ignore
                }

            let awaitMail (mb : Mailbox<_>) =
                async {
                    return mb.MailAwait.WaitOne() |> ignore
                }

        
            let listenTo eventStream (mb : Mailbox<_>) =
                async  {
                    while true do
                        let! evt = eventStream
                        do! post evt mb
                } |> Async.StartChild
                  |> Async.map ignore