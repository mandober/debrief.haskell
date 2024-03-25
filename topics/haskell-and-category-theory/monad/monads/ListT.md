# ListT

```hs
newtype List    a = List     (Maybe (a, List    a))
newtype ListT m a = ListT (m (Maybe (a, ListT m a)))
```


## `ListT` done right

- `list-t-1.0.5.3`: ListT done right
https://hackage.haskell.org/package/list-t-1.0.5.3/docs/ListT.html


```hs
{-|
A proper implementation of the list monad-transformer.

Useful for streaming of monadic data structures.

Since it has instances of 'MonadPlus' and 'Alternative', you can use general utilities packages like monadplus with it.

  * monadplus: Haskell98 partial maps and filters over MonadPlus
    http://hackage.haskell.org/package/monadplus
    Filtering and folding over arbitrary MonadPlus instances. This package generalizes many common stream operations such as filter, catMaybes etc.

-}
newtype ListT m a = ListT (m (Maybe (a, ListT m a)))
  deriving (Foldable, Traversable, Generic)
```


## Refs

- ListT done right
https://hackage.haskell.org/package/list-t-1.0.5.3/docs/ListT.html

- https://hackage.haskell.org/package/directory-1.3.8.0/docs/System-Directory-Internal.html#t:ListT

- https://hackage.haskell.org/package/optparse-applicative-0.17.0.0/docs/Options-Applicative-Internal.html#t:ListT

- https://hackage.haskell.org/package/pipes-4.3.16/docs/Pipes.html#t:ListT
