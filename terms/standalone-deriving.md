# Standalone Deriving Declaration

Standalone Deriving Instance Declaration
https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/deriving.html#stand-alone-deriving

derived instances of eq and ord
derived instances of enum
derived instances of bounded
derived instances of read and show

```hs
{-# LANGUAGE StandaloneDeriving #-}

-- datatype with auto-derived classed
data List a = Empty | Cons a (List a) deriving (Eq, Ord)

-- standalone Read class auto-derivation
deriving instance Read a => Read (List a)
```


## Derivable classes

Derivable classes: Show, Read, Ord, Eq, Num, Bounded, Enum
