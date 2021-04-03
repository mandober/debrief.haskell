# Functor-based classes


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
