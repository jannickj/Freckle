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

    let instantiate p (s : Sample<_>) = s p

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
