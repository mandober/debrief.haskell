# SPECIALIZE pragma

https://downloads.haskell.org/~ghc/9.2.1-alpha2/docs/html/users_guide/exts/pragmas.html#specialize-pragma

Ask that GHC specialize a polymorphic value to a particular type.

For key overloaded functions, you can create extra versions, *at the cost of larger code*, specialised to particular types. Thus, if you have an overloaded function:

```hs
hammeredLookup :: Ord k => [(k, v)] -> k -> v
```

If it is heavily used on lists with `Widget` keys, you could specialise it as follows:

```hs
{-# SPECIALIZE hammeredLookup :: [(Widget, v)] -> Widget -> v #-}
```
