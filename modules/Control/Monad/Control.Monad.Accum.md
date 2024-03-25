# Control.Monad.Trans.Accum

https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Accum.html

- Computation type:
  Accumulation, either append-only state, or 
  writer with the ability to read all previous input
- Binding strategy:
  Binding a function to a monadic value monoidally accumulates the subcomputations (that is, using `<>`)
- Useful for: Logging, patch-style tracking
- Zero and plus: None
- Example type: `Accum w a`


## On commutativity

>Some effects are commutative: it doesn't matter which you resolve first, as all possible orderings of commutative effects are isomorphic.

Consider, for example, the reader and state effects, as exemplified by `ReaderT` and `StateT`, respectively. 

```hs
newtype ReaderT r m a = ReaderT (a -> m r)
newtype Reader  r   a = ReaderT (a ->   r)

newtype StateT s m a = StateT (s -> m (a, s))
newtype State  s   a = State  (s ->   (a, s))
```

If we have `ReaderT r (State s) a`, this is effectively 
`r -> State s a` ~ `r -> s -> (a, s)`.

If we instead have `StateT s (Reader r) a`, this is effectively 
`s -> Reader r (a, s)` ~ `s -> r -> (a, s)`.


Since we can always reorder function arguments (for example, using `flip`, as in this case) without changing the result, these are *isomorphic*, showing that *reader and state are commutative, or, more precisely, commute with each other*.

However, this isn't generally the case. Consider instead the error and state effects, as exemplified by `MaybeT` and `StateT` respectively.

If we have `MaybeT (State s) a`, this is effectively 
`State s (Maybe a)` ~ `s -> (Maybe a, s)`

put simply, the error can occur only in the result, but not the state, which always "survives".

On the other hand, if we have `StateT s Maybe a`, this is instead 
`s -> Maybe (a, s)`

here, if we error, we lose both the state and the result! Thus, *error and state effects do not commute with each other*.

As the MTL is capability-based, we support any ordering of non-commutative effects on an equal footing. Indeed, if you wish to use `MonadState`, for example, whether your final monadic stack ends up being `MaybeT (State s) a`, `StateT s Maybe a`, or anything else, you will be able to write your desired code without having to consider such differences.

However, the way we implement these capabilities for any given transformer (or rather, any given transformed stack) is affected by this ordering unless the effects in question are commutative.

We note in this module which effects the accumulation effect does and doesn't commute with; we also note on implementations with non-commutative transformers what the outcome will be.

Note that, depending on how the "inner monad" is structured, this may be more complex than we note: we describe only what impact the "outer effect" has, not what else might be in the stack.
