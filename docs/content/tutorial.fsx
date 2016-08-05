(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "..\\..\\bin\\Freckle\\"
let update () = ()
type Time = Time of unit
    with static member (-) (_,_) : Time = failwith ""
let now () : Time = failwith ""
let time _ : Time = failwith ""
let wait : Time -> unit = failwith ""
let updateState () = ()
let render () = ()
#I __SOURCE_DIRECTORY__
#load "load-references-debug.fsx"
#load "../Helpers.fs"
      "../Time.fs"
      "../Clock.fs"
      "../Sample.fs"
      "../Feed.fs"
      "../Mailbox.fs"
      "../ComputationExpression.fs"
(**
# Part 1: Understanding Functional Reactive Programming

Functional Reactive Programming (or short FRP), is often mired in abstract languge and theoretical functional programming lingo.
In this tutorial we will attempt to keep it simple and easy to follow.

To begin with lets define FRP.
In order for something to be classified as frp it must be:

 * Composable and Declarative (i.e. functional)
 * Reactive
 * Sampling Resolution Insensitive

The first two requirements are fairly easy to understand, i.e. of course it must be functional and be able to react to events occuring in the system.
However the 3. requirement **Sampling Resolution Insensitive**, requires that we understand what sampling resolution is.

In short the sampling resolution is analogous to the the span of time between each sample, in video games this is often refered to as the tick rate.

However let us see how this problem plays out in an actual application.
Imagine we have the following program
*)

while true do
    update ()

(**
This program updates its state and nothing more, the sampling resolution of this program depends entirely on computer running it and how costly the function `update` is.
If we assume that it always takes 16 ms to execute `update` then we get a sampling resolution at
```
1000 / 16 ms ~ 60 ticks per seconds
```
Meaning that update is called about 60 times per seconds.

However as is obvious there is no guarantees given that `update` will take exactly 16 ms to execute. Which might problem if an application needs to update at precise interval, 
old programs have sometimes ignored this meaning when executed on modern computers they run extremly fast or have completely stopped functioning properly.

The next naive solution to account for a fixed sampling rate would be to delay the update difference. e.g.
*)
let desiredDelay = time 16

while true do
    let startTime = now ()
    update ()
    let diffTime = now () - startTime
    wait (desiredDelay - diffTime)

(**
Initially this provides us with what we required, the program operates consistently at a fixed sampling rate.
However as you can probably already observe, if for any reason at any point during the life time of the program, `update` takes longer than expected.
Meaning that `diffTime > desiredDelay` i.e. requring us to wait negative, then the program will become out of sync forever.
If it was because the `desiredDelay` was set shorter than update is in average, then there is nothing we can do.

This is because the resolution of the program is too low to support it, much like if we have a picture with a bad resolution we try to show on a large screen, that too will prevent anything meaningful.
However if in average `update` is far below the `desiredDelay` but once in a while goes over `desiredDelay`, then we essentially skip certain important computations.
e.g. imagine a program simulating a car but with the expectation that an `updateCar` function is called 10 times a second, then skipping a couple of calculations can have a huge impact on the result.

To solve this problem let us propose the next naive solution
*)
while true do
    let startTime = now ()
    if lastDiff < 0 then
        update ()
    update ()
    let diffTime = now () - startTime
    wait (desiredDelay - diffTime)

(**
This solution solves the problem by calling `update` an extra time in the event it becomes out of sync.
So job done? Not quite... because depending on how long a single execution takes it might still cause update to become out of sync.

Imagine that `update` was for a graphical application and looked something like this:
*)
let update () =
    let state = updateState() //Fairly fast state update
    render state              //Expensive graphic render function

(**
As you might imagine rendering two times in a row doesn't change anything from an application perspective, however it is a huge drain on the resources of the cpu.
So what we in reality want is something that allows us to define that if we miss an `updateState` but if we miss a `render` then that should just be skipped.

Before we move onto how frp would handle these problems, let's highlight one thing that has been overlooked so far.

Remember the **Insensitive** in Sampling Resolution Insensitive, thus far the proposed solutions have completely overlooked this aspect.
All we have achieved is uniform(kinda..) sampling however consider what would happen if we changed the `desiredDelay` to half its current value.
Essentially this would mean `update` is called twice as many times, or in other words the application is **Sampling Resolution Sensitive**.

On reflection if we consider all the prior problems they have all been the victim of some form of **Sampling Resolution Sensitivity**.
This is why **Sampling Resolution Insensitivity** is and must be a property of FRP.

So far the problems we have shown are

 1. Unpredictable Sampling
 2. Single samplings causing out of sync
 3. Redudant sampling
 4. The root to all of these being **Sampling Resolution Sensitivity**

*)

(**
# Part 2: Using Freckle

Now that we understand the problem of **Sampling Resolution Sensitivity**, let's see how code using freckle would look.

Lets assume we are building a simple count app, that will count and show the result in the console.
*)

//Simple count function
let updateState s = s + 1

//Wrapping a simple print in async, might seem strange if you are used to C#.
//However if you tried haskell's IO syntax this should feel right at home
let render at state = 
    async {
        //is the time when render was called
        return printfn "%A: %d" (Time.toDateTime at).TimeOfDay state
    } 

//The app function is our declarative description of what our application does
let app (state : int) : Sample<Async<int>>=
    //sampleAsync is the same as Sample<Async<_>>
    sampleAsync {
        
        let! state' = 
            Feed.pulse 1u //We generate a pulse at one tick per second
            |> Sample.bind (Feed.foldPast (fun s _ -> updateState s) state) //Fold over all pulses generated in this sample
            |> SampleAsync.ofSample //Convert our Sample<int> to Sample<Async<int>>
        
        //the Upto suffix inform the sampler that if we miss a pulse then we don't care
        do! Feed.pulseUpto 1u
            |> Sample.map (Feed.map (fun at -> render at state')) //Change the pulses to the render async
            |> Sample.bind Feed.plan_ //plan converts a feed of async into an async of feed 
                                      //plan_ is the same but it discards the resulting feed
        return state'
    }

(**

To set a bounded resolution we simply append a delay 100ms ~ 10 ticks per second
*)
let boundResolutionApp state = 
    app state
    |> SampleAsync.doAsync (Async.Sleep 100) 


(**

With this all that should be left is simply to begin sampling
*)
//the sampler needs a clock to meassure time, the app function and a start state in this case zero
Sample.sampleForever Clock.systemUtc boundResolutionApp 0
|> Async.RunSynchronously

//Run 1
> 00:00:48: 1
> 00:00:49: 2
> 00:00:50: 3
> 00:00:51: 4
....

(**
So far so good we see that even though we sample 10 times more times than we need our app remains consistent.

Question: what happens if the resolution is below our needs?
Let's see what occurs when we set the delay to 2 seconds
*)

let boundResolutionApp state = 
    app state
    |> SampleAsync.doAsync (Async.Sleep 2000)

//Run 2
> 00:00:23: 2
> 00:00:25: 4
> 00:00:27: 6
> 00:00:29: 8
....


(**
As we can see the prints (the call to our render function) only happens every other second. However our counting remains correct.
You could say that our application changed behavior duo to changing of the resolution, and thus is not **Sampling Resolution Insensitive**.
However this is from the app's perspective completely expected and counted for behavior, as it explicitly `pulseUpto` for the printout.
In truth the app only cares about getting a correct count, printout is a secondary concern and it is only interested in it occuring at some point.
Thus the app maintains its property of being **Insensitive**

For a fun exercise try and run the code yourself!, try and see what happens if the resolution is unbounded.
*)