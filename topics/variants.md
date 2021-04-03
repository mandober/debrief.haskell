# Variants

- Functor (Covariant)      fmap
- Contravariant:      contramap
- Bifunctor:              bimap, first, second
- Profunctor:             dimap, lmap,  rmap

```hs
fumap  :: (a -> a')                -> f a       -> f a'
comap  :: (a -> a')                -> f a'      -> f a

bimap  :: (a -> a') -> (b -> b')   -> f a  b    -> f a' b'
second ::              (b -> b')   -> f a  b    -> f a  b'
first  :: (a -> a')                -> f a  b    -> f a' b

dimap  :: (a -> a') -> (b -> b')   -> f a' b    -> f a  b'
lmap   :: (a -> a')                -> f a' b    -> f a  b
rmap   ::              (b -> b')   -> f a' b    -> f a' b'
```


```
a        F a b       b
. -----------------> .
|                    |
|                    |
|                    |
| f   Bifunctor    g |
|       bimap        |
|                    |
↓                    ↓
. =================> .
a'      F a' b'      b'

```
