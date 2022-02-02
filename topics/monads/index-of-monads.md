# Index of monads

* Properties
  - commutative monads (Reader, Maybe)
  - lazy and strict API flavors
  - set of laws for instances to uphold
  - monad transformers
  - Free Monad, Freer Monad

* Monads: Index
- Monad
- MonadPlus
- MonadFail
- MonadFix
- MonadZip
- MonadBase
- MonadBaseControl
- MonadTransControl
- RandT
- Rand
- Cont
- ContT
- Logic
- LogicT
- Select
- SelectT
- Writer
- WriterT
- State
- StateT
- RWS
- RWST
- Identity
- IdentityT
- Maybe
- MaybeT
- ListT
- Either
- EitherT
- Except
- ExceptT
- Reader
- ReaderT
- WriterT
- MonadTrans
- MonadIO
- MonadReader
- MonadWriter
- MonadLogger
- MonadState
- MonadError
- MonadExcept
- MonadCatch
- MonadMask
- MonadThrow




## General type classes

- Monad
- MonadPlus
- MonadFail

* MonadFix
  - package `base` 4.8.0.0, in the module `Control.Monad.Fix`
  - class for monads that have fixed points with a "knot-tying" semantics
  - method
    - `mfix :: (a -> m a) -> m a`
    - the fixed point of a monadic computation
    - `mfix f` executes the action `f` only once,
    - with the eventual output fed back as the input.
  - class is used in translation of recursive do notation
  - Laws:
    1. purity: `mfix (return . h) = return (fix h)`
    2. left shrinking (tightening): 
       `mfix (\x -> a >>= \y -> f x y) = a >>= \y -> mfix (\x -> f x y)`
    3. sliding: `mfix (liftM h . f) = liftM h (mfix (f . h))` for strict `h`
    4. nesting: `mfix (\x -> mfix (\y -> f x y)) = mfix (\x -> f x x)`
  - Value Recursion in Monadic Computations, by Levent Erkok, 2002
    https://leventerkok.github.io/papers/erkok-thesis.pdf


* MonadZip
  - Monadic zipping; used for monad comprehensions


- MonadBase (liftBase)
  - type class from the `transformers-base` package
  - generalized version of the MonadIO class
  - provides more general `liftBase` which allows lifting to any base monad
- MonadBaseControl
- MonadTransControl


## Monad Transformers types and classes

A monad transformer is a data type, usually a newtype, like `StateT`. Each MT has a convenince variant, defined as a type alias (without the suffix `T`) and using the `Identity` monad to stand in for the type param `m` that represents a monad. The third wheel around monad transformers is a type class, in this case, `MonadState` that groups together all monady-staty-likey datatypes under a common interface. The usual lascious mix of words and tricks.

```hs
-- monad transformer data type
newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

-- monad transformer type alias
type State s a = StateT s Identity a

-- type class for monad-staty-like data types
class Monad m => MonadState s m | m -> s where
-- MINIMAL state | get, put
```


- `IdentityT`
  - the trivial MT that maps a monad to an equivalent monad
  - useful for functions parameterized by a MT

- `MaybeT`
  - parameterizable maybe monad
  - obtained by composing an arbitrary monad with the Maybe monad
  - computations are actions that may produce a value or exit
  - `return` yields a computation that produces that value
  - `>>=` sequences two subcomputations, exiting if either one exits


* WriterT
  - `newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }`
  - writer monad withac cumulating output `w` and the inner monad `m`
  - `return` produces `mempty`
  - `>>=` combines outputs of subcomputations using `mappend`
  - has strict and lazy versions with the same interface

* StateT
  - `newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }`

* RWST
  - MT that combines ReaderT, WriterT and StateT
  - has strict and lazy versions with the same interface
  - MT with env `r`, output `w`, state `s`, to an inner monad `m`
  - `newtype RWST r w s m a = RWST { runRWST :: r -> s -> m (a, s, w) }`


## Monad Transformers classes

* MonadTrans (lift)
  - The class of monad transformers; single method
    - `lift :: Monad m => m a -> t m a`
  - lift a computation from the argument monad to the constructed monad   
  - Instances should satisfy the two laws that say `lift` is a MT:
    - `lift . return  = return`                   [MT.1]
    - `lift (m >>= f) = lift m >>= (lift . f)`    [MT.2]

* MonadIO (liftIO)
  - type class from the `base` package
  - lifting IO actions through MT stacks with `IO` at the base



## Packages

`transformers` 0.4.2.0
- Concrete functor and monad transformers
- https://hackage.haskell.org/package/transformers-0.4.2.0/docs/doc-index-All.html

`mtl` 2.2.2
- https://hackage.haskell.org/package/mtl-2.2.2/docs/doc-index-All.html
- Monad classes, using functional dependencies


## Refs

https://wiki.haskell.org/Category:Monad
https://wiki.haskell.org/Category:Monad
https://wiki.haskell.org/Monad
https://wiki.haskell.org/Merely_monadic
https://wiki.haskell.org/Monad_tutorials_timeline
https://wiki.haskell.org/Monads_as_computation#Do_notation
https://wiki.haskell.org/Research_papers/Monads_and_arrows
https://okmij.org/ftp/Computation/monads.html
https://okmij.org/ftp/Computation/monadic-shell.html
