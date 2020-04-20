# Eq

**Eq**
- used for types that support equality testing
- functions its members implement are == and /=
- Member types: (), Bool, Char, Ordering, Int, Integer, Float, Double




* `class Eq a where`
* functions:
  - (==) :: a -> a -> Bool
  - (/=) :: a -> a -> Bool
  - min impl: either (==) ot (/=)
* members:
  - instance Eq ()
  - instance Eq Word
  - instance Eq Ordering
  - instance Eq Int
  - instance Eq Float
  - instance Eq Double
  - instance Eq Char
  - instance Eq Bool
  - instance Eq a => Eq [a]
  - instance (Eq b, Eq a) => Eq (Either a b)
  - instance Eq Integer
  - instance Eq a => Eq (Maybe a)
  - tuples
    - instance (Eq a, Eq b) => Eq (a, b)
    - other tuples



```hs
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool
-- MINIMAL: (==) | (/=)
```
