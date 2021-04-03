# Monads

Monads make many concepts possible in a purely FPL that is Haskell. Monads are ideal for dealing with side-effects and modelling things like
- logging with `Writer` monad
- shared, read-only, environment with `Reader` monad
- stateful computations: intervleaving state with `State` monad
- exceptions
- continuations
- partial functions
- indeterminism

All these things are achieved by embellishing impure procedures, from the impure type signature schema `a -> b`, into the pure type signatures of the form `a -> m b`, where `m` stands for a monad specialized to each particular use case. These procedures, now pure functions, still take a type `a`, only now, instead of producing an impure type `b`, they produce an embellished type `m b`, resulting in "embellished" computations.

General signature of
* impure computations: `a -💩-> b`
* pure   computations: `a --> m b`

Use cases
* Writer monad
  - used for: logging
  - signature: `Writer w a`
* Reader monad
  - used for: shared read-only env
  - signature: `Reader r a`
* State monad
  - used for: modelling state
  - signature: `State s a`
* partial functions
  - signature: `a -> Maybe b`
* indeterminism
  - signature: `a -> [b]`


```hs
newtype Reader r a = Reader { runReader :: (r -> a) }
newtype Writer w a = Writer { runWriter :: (a, w) }
newtype State  s a = State  { runState  :: s -> (a, s) }
```

## Monad transformers

In fact, these newtypes are not defines as above, rather they are defined in terms of their corresponding monad's transformer.

```hs
newtype WriterT w m a = WriterT { runWriterT :: m (a, w)      }
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a      }
newtype StateT  s m a = StateT  { runStateT  :: s -> m (a, s) }
```

Then, to retrive the bare type (as above), Data.Functor.Identity is used for the type param `m` that represents a particular monad.

```hs
type State s = StateT s Data.Functor.Identity.Identity :: * -> *
-- i.e.
import Data.Functor.Identity (Identity)
type State  s a = StateT  s Identity a
type Reader r a = ReaderT r Identity a
type Writer w a = WriterT w Identity a
```
