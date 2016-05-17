namespace Freckle
open FSharp

type Events<'e, 't> = Events of ('e * 't) list