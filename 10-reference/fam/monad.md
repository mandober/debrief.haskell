# Monad

In Haskell, monad is the name of a typeclass. To be a member of the Monad class, a type must have methods `return` and `bind` defined.

```hs
class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    fail   :: String -> m a
```
