

```hs
class Enum a where
    succ            :: a -> a
    pred            :: a -> a
    toEnum          :: Int -> a
    fromEnum        :: a -> Int
    enumFrom        :: a -> [a]
    enumFromThen    :: a -> a -> [a]
    enumFromTo      :: a -> a -> [a]
    enumFromThenTo  :: a -> a -> a -> [a]
-- MINIMAL: toEnum, fromEnum
```


**Enum**
- Enum members are sequentially ordered types, they can be enumerated
- tuple, list,
- we can use its types in list ranges
- They have defined successors and predecesors, `succ` and `pred` functions
- Member types: (), Bool, Char, Ordering, Int, Integer, Float, Double
