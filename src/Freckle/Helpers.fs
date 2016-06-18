namespace FSharp

module Helpers =

    let inline undefined<'a> : 'a = failwith "undefined"
    
    let inline swap (a,b) = (b,a)
    
    let inline flip f a b = f b a

    let inline const' k = fun _ -> k
    
    let safeUnbox (o : obj) =
        match o with
        | :? 'a as a -> Some a
        | _ -> None

    let tuple fst snd = (fst, snd)

    module Async =
        open System.Threading
        open System.Threading.Tasks
        
        let (>>=) ma f = async.Bind(ma, f)
        let ( *>>) ma mb = async.Bind(ma, (fun _ -> mb))

        let doNothing = async.Zero()

        type Signal<'a> = Continue of 'a
                        | Completed of 'a
                        | Stop

        let recursion (f : 's -> Async<Signal<'s>>) (state : 's)  : Async<'s> =
            async {
                let mutable s = state
                let mutable sad = true
                while sad do
                    let! sa = Async.TryCancelled(f s, fun _ -> ())
                    match sa with
                    | Continue a -> s <- a
                    | Stop -> sad <- false
                    | Completed a -> 
                        s <- a
                        sad <- false
                return s
            }

        let recursionWithCancel (cancel : CancellationToken) (f : 's -> Async<Signal<'s>>) (state : 's)  : Async<'s> =
            async {
                let! cancelSelf = Async.CancellationToken
                use source = CancellationTokenSource.CreateLinkedTokenSource(cancelSelf, cancel)
                let cancelOutside = source.Token
                let mutable s = state
                let mutable sad = true
                let mutable first = true
                while sad do
                    let asyncSa = f s
                    let usetoken =
                        if first
                        then cancelSelf
                        else cancelOutside
                    first <- false
                    let sa = Async.StartAsTask(asyncSa, cancellationToken = usetoken)
                    try
                        sa.Wait usetoken
                    with _ -> ()

                    match sa.Status with
                    | TaskStatus.RanToCompletion ->
                        let sa' = sa.Result
                        match sa' with
                        | Continue a -> s <- a
                        | Stop -> sad <- false
                        | Completed a -> 
                            s <- a
                            sad <- false
                    | TaskStatus.Canceled -> 
                        sad <- false 
                    | TaskStatus.Faulted ->
                        raise sa.Exception
                    | _ -> failwith <| sprintf "unknonwn fail state for task %A" sa
                return s
            }

        let forever (f : 's -> Async<'s>) (state : 's)  : Async<_> =
            async {
                let mutable s = state
                while true do
                    let! s' = f s
                    s <- s'
                return undefined
            }

        let map f ma =
            async {
                let! a = ma
                return f a
            }

        let bind f ma = async.Bind(ma, f)
            
    module List =

        let tryHead l =
            match l with
            | a :: _ -> Some a
            | [] -> None

    module Option =
        
        let pure' x = Some x

        let default' x o =
            match o with
            | Some a -> a
            | None -> x

        let ap mf ma = 
            match mf, ma with
            | Some f, Some a -> Some (f a)
            | _ -> None

        let bind fm ma =
            match ma  with
            | Some a -> fm a
            | None -> None