# Derivable classes

Derivable classes: Show, Read, Ord, Eq, Num, Bounded, Enum

```hs
-- deriving one class
data ... deriving Show

-- deriving more then one class - parens required
data ... deriving (Show, Read, Ord, Eq, Num, Bounded, Enum)
```
