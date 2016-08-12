/// Wrapper for all computational expressions for each type.
/// In haskell lingo this is what provides do notation
[<AutoOpen>]
module Freckle.ComputationExpression

/// The sample expression
let sample = Sample.ComputationalExpression.sample

/// The sample returning Async operations expression
let sampleAsync = SampleAsync.ComputationalExpression.sampleAsync

/// The feed expression
let feed = Feed.ComputationalExpression.feed
