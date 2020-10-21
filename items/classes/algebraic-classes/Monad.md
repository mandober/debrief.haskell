# Monad class

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


# Monad

A Monad wraps a computation (value, function) in a certain context (structure, contaner, box). A monad must define both, a means of wrapping a computation in a context, and a way of combining computations that are inside a context. As a difference from functors and applicatives, monads also automatically join (flatten) nested contexts.

In Haskell, monad is a typeclass, hierarchically above the Functor and Applicative classes and it requires that a type is an instance of both before becoming a monadic instance: Functor ⊆ Applicative ⊆ Monad.

The monadic context may be a data structure (tuple, list) or ADTs (like `Maybe`, `Either`), functions (functions are instances of the Monad class), a computation such as effectful `IO` type.


# Monads

A monadic type must have a way to lift (plain) values into the monad (box). This may be achieved with a value/data constructor or by a function. Also, to secure themembership in the Monad typeclass, a type must have a binding function that, by combining values of type `m` with computations that output values of type `m`, are used for producing new computations for `m` values.

The definition of the Monad class:

```hs
class Monad m where
    return :: a -> m a
    (>>=)  :: m a -> (a -> m b) -> m b
    (>>)   :: m a -> m b -> m b
```

For example, `Maybe` type is a member of the Monad class. If we are to rewrite the Monad class from the perspective of the Maybe type (by replacing every `m` with `Maybe`), it'd look like:

```hs
class Monad Maybe where
    return :: a -> Maybe a
    (>>=)  :: Maybe a -> (a -> Maybe b) -> Maybe b
    (>>)   :: Maybe a -> Maybe b -> Maybe b
```

The `return` function is taken care by the Maybe's own `Just` type ctor:    
`Just :: a -> Maybe a`   
It has the appropriate signature and it certainly does lift a value into Maybe.

The bind function, usually in the form of the `>>=` operator, must be defined, with this sig: `Maybe a -> (a -> Maybe b) -> Maybe b`.


```hs
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
m a >>= m b = 
```


Also the "then" function (`>>` operator) must be defined. It is similar to the bind function only it doesn't use the value `a` (from the `Maybe a` input) as the input for the function `(a -> Maybe b)`; that's why there is no such function - it just collapses to `Maybe b` (i.e. function's return value).

```hs
(>>)  :: Maybe a -> Maybe b -> Maybe b
m a >> m b = 
```
