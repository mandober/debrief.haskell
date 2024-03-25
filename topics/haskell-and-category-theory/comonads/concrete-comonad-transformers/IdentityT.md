# IdentityT comonad transformer

`IdentityT` is a trivial monad transformer, which maps a monad to an equivalent monad. `IdentityT` is both a trivial monad and comonad transformer.

The comonad `package` just delegates this type and the containing module, `Control.Comonad.Identity`, i.e. `Control.Comonad.Trans.Identity`, to the same type defined in the `Control.Monad.Identity` i.e. `Control.Monad.Trans.Identity` module, that just re-exports the `Identity` type from the `Data.Functor.Identity` module where it is actually defined.


The `Identity` is a trivial functor/monad that just holds a single value `a`. The `IdentityT` is a trivial monad transformer in that it just holds a single value `a` in some monadic context `m`.

`Identity` is the epitome of the Kleisli arrow, `a -> m a`, where the monad `m` is the `Identity`, `a -> Identity a`.

`Identity` is also the epitome of the co-Kleisli arrow, `w a -> a`, where the comonad `w` is the `Identity`, `Identity a -> a`.

Despite being rather trivial, the `IdentityT` monad transformer and the `Identity` monad is useful as a "unit" monad in a monadic transformer stack.

Many monad precursors are defined in terms of their monad transformers. For example, the `Reader` monad is just a type alias for `ReaderT r Identity a`, where the `Identity` fills the role of monad `m` in `ReaderT r m a`.

```hs
-- the Reader monad (precursor to monad transformer)
newtype Reader r a = Reader { runReader :: r -> a }
-- the ReaderT monad transformer
newtype Reader r m a = Reader { runReader :: r -> m a }
-- the Reader monad again as a type alias of ReaderT
type Reader r a = ReaderT r Identity a
-- often eta-reduced as
type Reader r = ReaderT r Identity
```
