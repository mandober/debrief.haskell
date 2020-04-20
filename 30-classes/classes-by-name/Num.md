

```hs
class Num a where
    (+)    :: a -> a -> a
    (-)    :: a -> a -> a
    (*)    :: a -> a -> a
    abs    :: a -> a
    negate :: a -> a
    signum :: a -> a
    fromInteger :: Integer -> a
-- MINIMAL: (+), (*), abs, signum, fromInteger, negate|(-)
```


**Num**
- Int, Integer, Double, Float
- Its members have the property of being able to act like numbers
- To be in `Num`, a type must already be in `Show` and `Eq`
