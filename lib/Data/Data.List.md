# Data.List



```hs
import Data.List

-- bultin list
data [] a = [] | (:) a ([] a)
data [a]  = [] | a : [a]

-- ≅ user list
data List a = Nil | Cons a (List a)
-- ≅ Cons tags a pair
data List a = Nil | Cons (a, List a)
type La = 1 + μaLa
```
