# Concrete monad transformers

- https://hackage.haskell.org/package/transformers
- https://hackage.haskell.org/package/mtl

Concrete monad transformers were first defined in the `transformers` package. The `mtl` package depends on the `transformers` package, defines these classes using fundeps.

## `transformers` package

The `transformers` package contains:
- the monad transformer class (in `Control.Monad.Trans.Class`)
- concrete functor and monad transformers, each with associated operations and functions to lift operations associated with other transformers.

The `transformers` package can be used on its own in portable Haskell code, in which case operations need to be *manually lifted* through transformer stacks (see `Control.Monad.Trans.Class` for some examples).

Alternatively, it can be used with the non-portable monad classes in the `mtl` or `monads-tf` packages, which *automatically lift* operations introduced by monad transformers through other transformers.

### Concrete monads

- Reader,                     `r ->   a`
- ReaderT,                    `r -> m a`
- Writer (Lazy, Strict, CPS),   `(a, w)`
- WriterT                     `m (a, w)`
- State  (Lazy, Strict),      `s ->   (a, s)`
- StateT                      `s -> m (a, s)`
- RWS    (Lazy, Strict, CPS)  `r -> s ->   (a, s, w)`
- RWST                        `r -> s -> m (a, s, w)`
- Identity
- Maybe
- Except
- Select
- Accum
- Cont


Kleisli arrows, `a -> m b`
- Reader    `r ->   a`                   ((->) r)
- ReaderT   `r -> m a`
- Writer          `(a, w)`               ((,) a)
- WriterT         `m (a, w)`
- State     `s ->   (a, s)`
- StateT    `s -> m (a, s)`
- RWS       `r -> s ->   (a, s, w)`
- RWST      `r -> s -> m (a, s, w)`

```hs
newtype ReaderT r m a = ReaderT { runReaderT :: r ->      m  a) }
newtype WriterT w m a = WriterT { runWriterT ::           m (a, w) }
newtype StateT  s m a = StateT  { runStateT  ::      s -> m (a, s) }
newtype RWST  r s m a = RWST    { runRWST    :: r -> s -> m (a, s, w) }
```



### Modules

- *Control*
  - *Applicative*
    - Control.Applicative.Backwards
    - Control.Applicative.Lift
  - *Monad*
    - *IO*
      - Control.Monad.IO.Class
    - Control.Monad.Signatures
    - *Trans*
      - Control.Monad.Trans.Accum
      - Control.Monad.Trans.Class
      - Control.Monad.Trans.Cont
      - Control.Monad.Trans.Except
      - Control.Monad.Trans.Identity
      - Control.Monad.Trans.Maybe
      - Control.Monad.Trans.RWS
        - Control.Monad.Trans.RWS.CPS
        - Control.Monad.Trans.RWS.Lazy
        - Control.Monad.Trans.RWS.Strict
      - Control.Monad.Trans.Reader
      - Control.Monad.Trans.Select
      - Control.Monad.Trans.State
        - Control.Monad.Trans.State.Lazy
        - Control.Monad.Trans.State.Strict
      - Control.Monad.Trans.Writer
        - Control.Monad.Trans.Writer.CPS
        - Control.Monad.Trans.Writer.Lazy
        - Control.Monad.Trans.Writer.Strict
  - *Data*
    - *Functor*
      - Data.Functor.Classes
      - Data.Functor.Identity
      - Data.Functor.Constant
      - Data.Functor.Compose
      - Data.Functor.Reverse
      - Data.Functor.Product
      - Data.Functor.Sum
