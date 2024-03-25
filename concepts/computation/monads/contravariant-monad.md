# Contravariant monads

```hs
return :: Monad m => a -> m a
(>>=)  :: Monad m => m a -> (a -> m b) -> m b
contrabind :: ContraMonad m => m a -> (b -> m a) -> m b

(=<<)      :: Monad       m => (a -> m b) -> (m a -> m b)
contrabind :: ContraMonad m => (b -> m a) -> (m a -> m b)
```

Contravariance doesn't apply to monads, but there is a contravariant analogue of Applicative: https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant-Divisible.html



## Ref

https://stackoverflow.com/questions/30641500/are-there-contravariant-monads
