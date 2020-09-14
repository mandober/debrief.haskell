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

# Monad

In Haskell, monad is the name of a typeclass. To be a member of the Monad class, a type must have methods `return` and `bind` defined.

```hs
class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
    fail   :: String -> m a
```



```hs
class Functor (f :: * -> *) where
    fmap  :: (a -> b) -> f a -> f b
    (<$)  ::  a       -> f b -> f a
-- MINIMAL: fmap

class Functor f => Applicative (f :: * -> *) where
    pure  ::    a              -> f a
    (<*>) :: f (a -> b) -> f a -> f b
    (*>)  :: f  a       -> f b -> f b
    (<*)  :: f  a       -> f b -> f a
-- MINIMAL: pure, (<*>)

class Applicative m => Monad (m :: * -> *) where
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a       -> m b  -> m b
    return ::   a               -> m a
    fail   :: String            -> m a
-- MINIMAL: (>>=)
```
