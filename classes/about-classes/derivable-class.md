# Deriving

Derivable classes:
- Show, Read
- Eq, Ord
- Bounded, Enum
- Num


## Standalone Deriving Declaration

```hs
{-# LANGUAGE StandaloneDeriving #-}

-- inline-deriving
data List a = Empty | Cons a (List a) deriving (Eq, Ord)

-- standalone deriving
deriving instance Read a => Read (List a)
```


## Deriving-related extensions

- DeriveAnyClass
- DeriveTypeable
- DeriveFunctor

Extensions to the "deriving" mechanism
https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/deriving.html


```hs
{-# LANGUAGE DeriveAnyClass #-}
```


(↑) :: Integral a => a -> a -> a; _ ↑ 0 = 1; x ↑ (n + 1) = x * (x ↑ n)
