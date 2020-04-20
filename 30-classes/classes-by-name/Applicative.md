# Applicative

Applicatives have the `pure` function that takes a pure value and regards it as a computation returning that value, and the effectual application which takes a computation of a function and a computation of an argument and returns a computation of a result.

The key difference is that there are more Applicatives then Monads because for an Applicative the effects can influence a value but not a structure of the computation.


```hs
class Functor f => Applicative (f :: * -> *) where
    (<*>) :: f (a ->   b) -> f a -> f b
-- (=<<)  ::   (a -> f b) -> f a -> f b

-- (>>=)  :: f a -> (a -> f b) -> f b
```

## The laws

```hs
pure = return

f <*> a = f >>= \f' -> a >>= \a' -> return (f' a')
```
