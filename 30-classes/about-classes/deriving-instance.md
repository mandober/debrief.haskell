# Standalone Deriving Declaration

Standalone Deriving Instance Declaration
https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/deriving.html#stand-alone-deriving

derived instances of eq and ord
derived instances of enum
derived instances of bounded
derived instances of read and show

```hs
deriving instance Bounded ()
```


# Derivable classes

Derivable classes: Show, Read, Ord, Eq, Num, Bounded, Enum

```hs
-- deriving one class
data <...> deriving Show

-- deriving more then one class - parens required
data <...> deriving (Show, Read, Ord, Eq, Num, Bounded, Enum)
```
