namespace Freckle

module Computation =

    module Behavior =
        open Behavior
        type Builder() =
            member inline this.Return(x : 'a) = pure' x
            member inline this.ReturnFrom(x) = x
            member inline this.Bind(ma, f) = bind f ma
            member inline this.Combine(a,b) = combine a b
            member inline this.Zero () = pure' ()

    let behavior = Behavior.Builder()

    module Freckle =
        open Freckle
        type Builder() =
            member inline this.Return(x : 'T) = pure' x
            member inline this.ReturnFrom(x) = x
            member inline this.Bind(ma, f) = bind f ma
            member inline this.Zero () = pure' ()

    let freckle = Freckle.Builder()

    module Freck =
        open Freck
        type Builder() =
            member inline this.Return(x : 'T) = pure' x
            member inline this.ReturnFrom(x) = x
            member inline this.Bind(ma, f) = bind f ma
            member inline this.Zero () = pure' ()

    let freck = Freck.Builder()

    module StateMachine =
        open StateMachine
        type Builder() =
            member inline this.Return(x : 'T) = pure' x
            member inline this.ReturnFrom(x) = x
            member inline this.Bind(ma, f) = bind f ma
            member inline this.Zero () = pure' ()

    let statemachine = StateMachine.Builder()


