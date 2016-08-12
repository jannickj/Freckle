(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I __SOURCE_DIRECTORY__
#r "../../packages/FSPowerPack.Core.Community/Lib/Net40/FSharp.PowerPack.dll"
#r "System.Core.dll"
#r "System.dll"
#r "System.Numerics.dll"
#r "../../bin/Freckle/Freckle.dll"
#r "../../packages/example/Microsoft.Tpl.Dataflow/lib/portable-net45+win8+wpa81/System.Threading.Tasks.Dataflow.dll"
open System.Threading.Tasks.Dataflow

(**
Freckle
======================

Simple monadic Functional Reactive Programming for F#

<div class="row">
  <div class="span1"></div>
  <div class="span6">
    <div class="well well-small" id="nuget">
      The Freckle library can be <a href="https://nuget.org/packages/Freckle">installed from NuGet</a>:
      <pre>PM> Install-Package Freckle</pre>
    </div>
  </div>
  <div class="span1"></div>
</div>

*)

(**

Quick Introduction
-----------------------

### Namespace and referecing

*)
open Freckle

(**
### Setting up a sampler and running
Create a sampler that prints the timespan it samples over
*)

let sampler count =
    sample {
        let! currentSpan = Sample.period
        let startedAt = (Time.toDateTime currentSpan.Beginning)
        let endedAt = (Time.toDateTime currentSpan.Finish)
        printfn "%d: Sampling %A -> %A" count startedAt endedAt
        return count + 1
    } |> SampleAsync.ofSample

Sample.sampleForever Clock.systemUtc sampler 0
|> Async.RunSynchronously


(**
### Guaranteed Fixed interval updates
Create a sampler that counts every second from 0 and is resistentent to computer lagspike
*)

let countSecondsSampler count =
    sample {
        let! pulses = Feed.pulse 1
        return! Feed.foldPast (fun c _ -> c + 1) count pulses
    } |> SampleAsync.ofSample

Sample.sampleForever Clock.systemUtc countSecondsSampler 0
|> Async.RunSynchronously

(**
### Best attempt Fixed interval updates
Creates a sampler that does something every second, but doesn't care if a step is skipped due to a lagspike
*)
let render =
    async {
        return printfn "Pretend like this is graphic rendering" 
    }

let renderingSampler () =
    sample {
        let! pulses = Feed.pulseUpto 1 //Notice it's pulseUpto
        return! Feed.transition (fun _ _ -> render) () pulses
    }

Sample.sampleForever Clock.systemUtc renderingSampler ()
|> Async.RunSynchronously


(**
### Listening to events
Listen to external events and print their content to the console

*)

async {
    let syncClock = Clock.synchronized Clock.systemUtc
    let! mb = Mailbox.createWithExpiration (Expire.After (Time.ofSeconds 30))  syncClock

    let renderingSampler mb () =
        sampleAsync {
            let! events = Mailbox.read mb |> SampleAsync.ofAsync
            do! Mailbox.clear |> SampleAsync.ofAsync
            return! Feed.foldPast (fun _ msg -> printfn "revieved %s" msg) () events
                    |> SampleAsync.ofSample
        }

    do! Sample.sampleForever syncClock (renderingSampler mb) ()
}
|> Async.RunSynchronously

(**

Further Reading
---------------

 * [Tutorial](tutorial.html) contains a further explanation of this sample library.

 * [API Reference](reference/index.html) contains automatically generated documentation for all types, modules
   and functions in the library. This includes additional brief samples on using most of the
   functions.
 
Contributing and copyright
--------------------------

The project is hosted on [GitHub][gh] where you can [report issues][issues], fork 
the project and submit pull requests. If you're adding a new public API, please also 
consider adding [samples][content] that can be turned into a documentation. You might
also want to read the [library design notes][readme] to understand how it works.

The library is available under Public Domain license, which allows modification and 
redistribution for both commercial and non-commercial purposes. For more information see the 
[License file][license] in the GitHub repository. 

  [content]: https://github.com/fsprojects/Freckle/tree/master/docs/content
  [gh]: https://github.com/fsprojects/Freckle
  [issues]: https://github.com/fsprojects/Freckle/issues
  [readme]: https://github.com/fsprojects/Freckle/blob/master/README.md
  [license]: https://github.com/fsprojects/Freckle/blob/master/LICENSE.txt
*)
