# Values inside a context

- Maybe monad adds a context of possible failure to values.
- Either monad adds a context of possible failure with a message why to values
- List monad lets us introduce non-determinism
- IO monad allows us to tame IO related side-effects
- Tuple-based monads allow us to manage extranal env and state
- Functions can add a context of partially applied value
- Writer is for values that have another attached that acts as a log of sorts


A type in a context is often said to represent a computation. For example, `Maybe` type represents a context where a value may or may not exists. Maybe, `Either` and lists can represent computations with a varying number of results - Maybe indicates a computation that can fail somehow or has 1 result, while a list can represent computations that fail or that have many (infinite even) results.

Maybe and `IO` imply, for different reasons, a layer of indirection in reaching the corresponding value. The key difference between the two is that with Maybe the *indeterminacy is only apparent*, because it is possible to find out in advance (e.g. using pattern matching) whether a value actually exists; more precisely, this is possible as long as a value inside a Maybe does not depend on IO. But unlike Maybe, there is a *fundamental indeterminacy* associated with the I/O actions. Respecting this indeterminacy is necessary for preserving referential transparency and this is achieved with the `IO` type.

The `Writer` monad is for values that have another value attached that acts as a sort of log value. Writer allows us to do computations while making sure that all the log values are combined into one log value that then gets attached to the result.
