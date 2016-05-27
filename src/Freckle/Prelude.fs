namespace Freckle

[<AutoOpen>]
module Prelude =

    let inline undefined<'a> : 'a = failwith "undefined"

    let inline const' k = fun _ -> k

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