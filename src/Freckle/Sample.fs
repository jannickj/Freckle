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
         ///Creates a period from two times, the order is irrelevant as the younger of the two is picked as finish time and vice verse
         static member period t1 t2 = if t1 > t2 then { Finish = t1; Beginning = t2 } else { Finish = t2; Beginning = t1 }

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

    ///Get the finish of sample
    let finish : Sample<Time> = fun p -> p.Finish

    ///Get the beginning of sample
    let begining : Sample<Time> = fun p -> p.Beginning
        
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

    ///Provides iterative sampling where time and state has to be stored and set manually
    let sampleOnce clock sampler  (lastTime, state) =
        let now = Clock.nowSynced clock
        let state' = sampler state
                     |> realise ({ Finish = now;  Beginning = lastTime })
        (now, state')

    ///Provides iterative sampling where time has to be stored and set manually
    let sampleOnce_ clock sampler  lastTime =
        let now = Clock.nowSynced clock
        sampler |> realise ({ Finish = now;  Beginning = lastTime })
        now

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
            member inline this.Combine(m1, m2) = bind (fun () -> m2) m1
            member inline this.Delay(fm : unit -> Sample<'a>) : Sample<'a> = fun p -> fm () p
    
        let sample = Builder()

///Adds more advanced delays
module Async =     
    open System.Threading.Tasks
    open System.Threading
    open System

    ///Delays sampling with a pulse.
    ///NOTICE on most machines running windows you can atmost reach 60 samples per seconds or a thread delay of 15 ms, if you require higher resolution, use pulseDelayBusy instead.
    let pulseDelay (hz : double) = 
        let delay = int (1000.0 / hz)
        let delay' = if delay <= 0 then 1 else delay
        Async.Sleep delay'
    
    let pulseMax = Async.Sleep 1


    let pulseDelayBusy hz =
        let delay = int64 (float TimeSpan.TicksPerSecond / hz)
        let delay' = if delay <= 0L then 1L else delay
        async {
            let watch = Diagnostics.Stopwatch()
            watch.Start()  
            while watch.Elapsed.Ticks < delay' do
                Thread.SpinWait(50000) |> ignore
        }

    let awaitAny (acs : seq<Async<unit>>) =
        async {
            use source = new CancellationTokenSource()
            let at a = Async.StartAsTask(a, cancellationToken = source.Token)
            let tasks = Seq.map at acs
            
            do! Async.AwaitTask(Task.WhenAny tasks)
                |> Async.Ignore
            source.Cancel()
        } 

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