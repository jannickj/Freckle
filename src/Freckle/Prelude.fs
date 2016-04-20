namespace Freckle

[<AutoOpen>]
module Prelude =

    let inline undefined<'a> : 'a = failwith "undefined"

    module Option =
        
        let pure' x = Some x

        let ap mf ma = 
            match mf, ma with
            | Some f, Some a -> Some (f a)
            | _ -> None

        let bind fm ma =
            match ma  with
            | Some a -> fm a
            | None -> None