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
