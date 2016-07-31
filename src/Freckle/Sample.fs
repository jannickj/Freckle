namespace Freckle

///A period is two times, a beginning time and a finish time.
///Beginning time will always be considered as non-inclusive and finish will always be considered as inclusive
type Period = 
    { ///Defines when a period has ended (incl)
      Finish    : Time
      ///Defines when a period has started (excl)
      Beginning : Time
    }
    with ///get the beginning time
         static member beginning (p : Period) = p.Beginning        
         ///get the finish time 
         static member finish (p : Period) = p.Finish

///Sample is a function from Period to some value.
///you can think of Sample as (Time -> Behavior)
type Sample<'a> = Period -> 'a

///Based on Behavior, Sample generates values from a span of time rather than a single time
module Sample =     
    open FSharp.Helpers  
    
    ///Get a sample from a value (it's just the const function)
    let inline pure' (a : 'a) : Sample<'a> = const' a

    ///Maps a sample from one result to another
    let inline map (f : 'a -> 'b) (sample : Sample<'a>) : Sample<'b> = sample >> f

    ///Joins nested samples
    let inline join (sample : Sample<Sample<'a>>) : Sample<'a> = fun p -> sample p p
    
    ///Binds a sample function into another sample
    let inline bind (f : 'a -> Sample<'b>) (m : Sample<'a> ) : Sample<'b> =
        join ((map f) m)
        
    ///Get the period of sample (it's just the id function)
    let period : Sample<Period> = id
        
    ///Provided a period a Sample will generate a value.
    ///Remember for impure Samples, if the period finishes in the past then the program can't magically go back in time and undo things.
    let realise p (s : Sample<'a>) : 'a = s p

    ///Samples the same sample function forever transfering the state to the next sample
    let sampleForever (clock : Clock) (sampler : 's -> Sample<Async<'s>>) state : Async<_> =
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

    ///Samples the sample function until the stop function is satisfied, then returns the last state
    let sampleUntil (clock : Clock) (stopFunc : 's -> bool) (sampler : 's -> Sample<Async<'s>>) state : Async<'s> =
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

    ///Setup for computational expression(Do Notation in haskell) for Sample
    module ComputationalExpression = 
        type Builder() =
            member inline this.Return(x : 'T) = pure' x
            member inline this.ReturnFrom(x) = x
            member inline this.Bind(ma, f) = bind f ma
            member inline this.Zero () = pure' ()
    
        let sample = Builder()

///SampleAsync module is designed to help working with samples that specifically returns Async
module SampleAsync =
    open FSharp.Helpers

    ///Get a SampleAsync from a value
    let pure' a = (Sample.pure' (async.Return a))

    ///Maps a SampleAsync from one result to another
    let map f m = Sample.map (Async.map f) m

    ///Joins nested SampleAsync into a single sampleAsync
    let join (m : Sample<Async<Sample<Async<'a>>>>) : Sample<Async<'a>> = 
        fun p -> Async.join <| Async.map ((|>) p) (m p)

    ///Discards the result of a SampleAsync
    let ignore s = map ignore s
    
    ///Bind for the monad type class
    let bind f m = join (map f m)   
    
    ///Transforms an async type into a SampleAsync
    let ofAsync x : Sample<Async<_>> = Sample.pure' x  

    ///Transforms a Sample type into a SampleAsync
    let ofSample x : Sample<Async<_>> = Sample.map async.Return x

    ///Attach an async operation at the end of a SampleAsync without changing it's output.
    ///Can be useful if you want to add a polling delay
    let doAsync das sa =
        Sample.map (Async.bind (fun a -> Async.map (const' a) das)) sa
               
    ///Setup for computational expression(Do Notation in haskell) for SampleAsync
    module ComputationalExpression =
        open Sample.ComputationalExpression

        type Builder() =
            member inline this.Return(x : 'T) = pure' x
            member inline this.ReturnFrom(x) = x
            member inline this.Bind(ma, f) = bind f ma
            member inline this.Zero () = pure' ()
            member inline this.Combine(m1, m2) = bind (fun () -> m2) m1
            member inline this.Delay(fm : unit -> Sample<Async<'a>>) : Sample<Async<'a>> = 
                sample {
                    let! res = fm ()
                    return res
                }
        let sampleAsync = Builder()