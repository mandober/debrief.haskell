# Monad

In Haskell, monad is the name of a typeclass. To be a member of the Monad class, a type must have methods `return` and `bind` defined.

```hs
class Applicative m => Monad (m :: * -> *) where
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a       -> m b  -> m b
    return ::   a               -> m a
    fail   :: String            -> m a
-- MINIMAL: (>>=)
```
