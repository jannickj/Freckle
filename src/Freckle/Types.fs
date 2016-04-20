namespace Freckle
open FSharp

type Events<'t, 'e> = Events of ('t * 'e) list