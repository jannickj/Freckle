namespace Freckle
module Core =
    open FSharp
  
    type Behavior<'t, 'a> = 't -> 'a

    module Behavior =
        
        let pure' x : Behavior<_,'a> = fun _ -> x

        let map f ma = f << ma

    type Reactive<'e, 't, 'a> = Events<'e, 't> -> 'a

    type StateMachine<'s, 'e, 't, 'a> = 's -> Events<'e, 't> -> 't -> ('s * 'a)
