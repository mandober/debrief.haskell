# Haskell by Topic :: Monad Transformers :: Introduction

## TL/DR

In Haskell, monads are a successful solution for managing side effects. External side effects must be manged by the `IO` monad, but internal side effects - which need not even be necessarily effectful - are manged by a slew of monads among which the most well-known are `State`, `Reader` and `Writer`.

## Side effects

Side effects come in two forms: external and internal. *External side effects* include things related to input/output, like communicating over a network, reading environment variables, manipulating files, printing to the screen, and they require effectful computations; there is no going around it, even if such computations are actually described using Haskell's (pure) functions and then passed to the dirty RTS for execution (so technically Haskell always remains pure). These are the bona fide side effects and they cannot be managed any other way except with the `IO` monad.

On the other hand, *internal side effects* (not an "official" name) are those computations that are usully and easily (especially in imperative languages) solved using effectful functions. Things in this category have to do with the maintainance of a global value.

## Global state

Maintaining a *global state* is the canonical example of such computations, where a global variable holds some state made available to all parts of the code that need it, not only for reading but for writing as well. Having a shared mutable value is always a bad idea however, even with single-threading applications, so it requires a lot of attention not to screw up (in imperative languages). Instead of reaching for side effects, it turns out it is possible and quite feasable to implement such computations in a pure setting.

Haskell does not even support mutation, so mutable values are not a concern, but there is still a need to model a computation based on a global mutable state. Fortunately this can be done with pure functions just by passing the state around - called *threading the state*. Each function will need to declare an extra parameter to bind the state, and then it can read or even change it, but it will also have to return it - as an additional output value, which is easily solved with tuples.

This way we can thread the state around to all functions that require it. However, doing this manually is not onlyt error prone but makes the code very busy, and a more elegant solution comes in the form of the `State` monad.

## State, Reader, Writer

The `State` monad models computations that involve a global value that needs to be both readable and writable. When the read access is sufficient, the `Reader` moand handles such pattern. It is also known as the *environment monad* because it exposes a globally accessibile environment - which usually contains the *configuration* information for the application. When the write access is sufficient, the `Writer` monad is used. It handles the patterns when a log is required - a global variable that holds a log so functions can append their (debugging) output to it. The `Writer` monad handles the threading of the log and the concatenation of data to it - this is why the `Writer` monad usually involves `Monoid` types somewhere in the pipeline - each monoid has a way of combining (concatenating) values. The thing with the `Writer` that manages a log is that log need not be readable by functions - the permission to append content is enough.

## Precursor monads

With each monad specializing in handling one specific task, it becomes necessary to combine different monads to integrate handling of various computations. An application may require global configuration (handled by the `Reader` monad), a log for debugging purposes (handled by the `Writer`), access to a global mutable state (handled by the `State` monad) - in fact, these things are so frequently required that Haskell provides the monad `RWS` that combines them all.

In general, however, programmers must combine their monads themselves, arranging them in the so-called *monadic stacks*. One monad is designated as the *base monad* in a monad stack - this is usually the `Identity` monad in case a pure computation is modelled, otherwise the `IO` monad. Other monads, like `Maybe` (provide lightweight exceptions), `Except` (provides exceptions and mechanisms for throwing and catching), `List` (for nondeterministic computations), and many others are than stacked one on top of the other.

In fct, this is a lie - monads cannot be stacked like that, there is jsut no way to combine them - and this is exactly where monad transformers come in. Each monad transformer is based on a particular monad - called the *precursor monad* in this role. A MT extends the precursor monad by providing a "slot" - as a type variable `m` - into which we can plug another monad.

```hs
-- precursor monad
newtype Reader  r   a = Reader  { runReader  :: r ->   a }
-- monad transformers based on it
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
```

Except > StateT > ReaderT > IO

We can take the `ExceptT` monad 
and plug the `StateT` into the slot `m` (i.e. into the slot `m` of ExceptT), 
than plug the `ReaderT` into that (into StateT's slot)
than plug the `IO` monad into that (into ReaderT's slot)
thus building a computation pipeline.

Note that the base monad - the monad that terminates the monad stack - is a precursor monad, here the `IO` monad. The `IO` monad does not have a monad transformer variant anyway - there is no such thing as `IOT`.
