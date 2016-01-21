[<AutoOpen>]
module Freckle.Sample
open LazyList
open FSharp.Helpers

[<AutoOpen>]
module Types =
    open System
    open Clock

    type Period = 
        { Finish    : Time
          Beginning : Time
        }

    type Sample<'a> = Period -> 'a

    type SamplingSpecification =
        { Next : Time option
        }

[<AutoOpen>]
module Core =
    module Sample =
        
        let inline pure' (a : 'a) : Sample<'a> = const' a

        let inline map (f : 'a -> 'b) (sample : Sample<'a>) : Sample<'b> = sample >> f

        let inline join (sample : Sample<Sample<'a>>) : Sample<'a> = fun p -> sample p p
    
        let inline bind (f : 'a -> Sample<'b>) (m : Sample<'a> ) : Sample<'b> =
            join ((map f) m)
        
        let period : Sample<Period> = id
        
        let beginning (p : Period) = p.Beginning
        
        let finish (p : Period) = p.Finish
        
        let realise p (s : Sample<'a>) : 'a = s p

[<AutoOpen>]
module ComputationalExpression =
    type Builder() =
        member inline this.Return(x : 'T) = Sample.pure' x
        member inline this.ReturnFrom(x) = x
        member inline this.Bind(ma, f) = Sample.bind f ma
        member inline this.Zero () = Sample.pure' ()
    
    let sample = Builder()

[<AutoOpen>]
module Sampling =

    let sampleForever (clock : Clock) state (sampler : 's -> Sample<Async<'s>>) : Async<_> =
        let inner (last, s) =
            async {
                let! time = Clock.now clock
                let! s' = sampler s { Beginning = last; Finish = time }
                return (time, s')
            }
        async {
            let! time = Clock.now clock
            return! Async.forever inner (time, state)
        }

    let sampleUntil (clock : Clock) (stopFunc : 's -> bool) state (sampler : 's -> Sample<Async<'s>>) : Async<'s> =
        let inner (last, s) =
            async {
                let! time = Clock.now clock
                let! s' = sampler s { Beginning = last; Finish = time }
                return (stopFunc s', (time, s'))
            }
        async {
            let! time = Clock.now clock
            let! (_,state') = Async.recursion inner (time, state)
            return state'
        }

[<AutoOpen>]
module SampleAsync =

    [<AutoOpen>]
    module Core =
        module SampleAsync =
            let pure' a = (Sample.pure' (async.Return a))
            let map f m = Sample.map (Async.map f) m
            let join (m : Sample<Async<Sample<Async<'a>>>>) : Sample<Async<'a>> = 
                fun p -> Async.join <| Async.map ((|>) p) (m p)

            let ignore s = map ignore s
    
            let bind f m = join (map f m)   
            
            let ofAsync x : Sample<Async<_>> = Sample.pure' x  
            let ofSample x : Sample<Async<_>> = Sample.map async.Return x
            let doAsync das sa =
                Sample.map (Async.bind (fun a -> Async.map (const' a) das)) sa
               
    [<AutoOpen>]
    module ComputationalExpression =
        type Builder() =
            member inline this.Return(x : 'T) = SampleAsync.pure' x
            member inline this.ReturnFrom(x) = x
            member inline this.Bind(ma, f) = SampleAsync.bind f ma
            member inline this.Zero () = SampleAsync.pure' ()
            member inline this.Combine(m1, m2) = SampleAsync.bind (fun () -> m2) m1
            member inline this.Delay(fm : unit -> Sample<Async<'a>>) : Sample<Async<'a>> = 
                sample {
                    let! res = fm ()
                    return res
                }
        let sampleAsync = Builder()

