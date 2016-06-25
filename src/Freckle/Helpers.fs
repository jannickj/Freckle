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

        
    type SortedType(t) =
        member x.Type : System.Type = t
        override x.Equals(yobj) =
            match yobj with
            | :? SortedType as y -> (x.Type = y.Type)
            | _ -> false
        override x.GetHashCode() = hash x.Type
        interface System.IComparable with
            member x.CompareTo yobj =
                match yobj with
                | :? SortedType as y -> compare (hash x.Type) (hash y.Type)
                | _ -> invalidArg "yobj" "cannot compare values of different types"

    module Async =
        open System.Threading
        open System.Threading.Tasks
        
        let (>>=) ma f = async.Bind(ma, f)
        let ( *>>) ma mb = async.Bind(ma, (fun _ -> mb))

        let doNothing = async.Zero()

        type Signal<'a> = Continue of 'a
                        | Completed of 'a
                        //| Stop

        let recursion (f : 's -> Async<Signal<'s>>) (state : 's)  : Async<'s> =
            async {
                let mutable s = state
                let mutable sad = true
                while sad do
                    let! sa = Async.TryCancelled(f s, fun _ -> ())
                    match sa with
                    | Continue a -> s <- a
                    //| Stop -> sad <- false
                    | Completed a -> 
                        s <- a
                        sad <- false
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

        let mapDefault x f o =
            Option.map f o |> default' x

        let ap mf ma = 
            match mf, ma with
            | Some f, Some a -> Some (f a)
            | _ -> None

        let bind fm ma =
            match ma  with
            | Some a -> fm a
            | None -> None