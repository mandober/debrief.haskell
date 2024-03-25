# Type constructors

## Higher-order Type-level Programming in Haskell
Csongor Kisss at ICFP'19

https://www.youtube.com/watch?v=ZiGIBU0haOk&list=PLMTONe7-tohkJ-PnXozQSGX0Y1s_VSB68&index=3



- `f` is injective           iff `f a = f b => a = b`
- `f` and `g` are generative iff `f a = g b => f = g`
- `f` is matchable           iff `f` is injective and generative


A part of GHC's type inference is application decomposition.

```hs
fmap :: Functor f => (a -> b) -> f a -> f b

fmap @Maybe :: (a -> b) -> Maybe a -> Maybe b

x = fmap show (Just True)

f a ~ Maybe Bool
f ~ Maybe
a ~ Bool
```

All type ctors are injective:

```hs
injective :: forall f a b. (f a ~ f b) => a -> b
injective = id
```

All type ctors are generative:

```hs
generative :: forall f g a b. (f a ~ g b) => f a -> g b
generative = id
```
