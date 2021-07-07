# Applicative class

https://en.wikibooks.org/wiki/Haskell/Prologue:_IO,_an_applicative_functor
https://wiki.haskell.org/Typeclassopedia#Applicative

**Applicative class** was added to Haskell after the Monad class, which accounts for some overlaps in their functionality (e.g. `Alternative` and `MonadPlus`), but even then they at least have a different constraint, that is their methods and related functions are constrained by Applicative vs Monad context, respectivelly.

The Applicative class is functionally located between the Functor and Monad classes: Applicative is a subclass of Functor, but a superclass of Monad. Therefore, all applicatives are **applicative functors**, and all monads are functors and applicatives. The applicatives and monads have many methods in common and defined in terms of each other, e.g. `return = pure`. However, monads are generaly more expressive then applicatives.

There are more Applicatives then Monads since the key difference between them is that applicative effects can influence the value but not the structure of a computation.

This hierarchy Functor => Applicative => Monad allows for some methods to be defined within each other. Functor's `fmap` method can be defined in terms of Applicative's `<*>` and `pure` methods: `fmap f x = pure f <*> x`.

The applicative functors were first described by McBride and Paterson in their classic paper, [Applicative Programming with Effects](http://www.soi.city.ac.uk/~ross/papers/Applicative.html) which hints at the intended intuition behind the Applicative type class. It is intended to encapsulate certain "effectful" computations in a functionally pure way, encouraging the *applicative programming style*.

Applicatives have the `pure` method that takes a pure value and regards it as a computation returning that value. Their other method is the **effectual application**, performed with the `<*>` operator, that takes a computation of a function and a computation of an argument, returning a computation of a result.


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
