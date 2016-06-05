namespace Freckle

type SiriConfig = { ThreadCount : int }

type Siri<'a> =  SiriConfig -> Async<'a>

module Siri =

    
    let pure' (x : 'a) : Siri<'a> = fun _ -> async.Return x