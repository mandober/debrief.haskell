**Ord**
- is for types that have an ordering, e.g. `(>) :: (Ord a) => a -> a -> Bool`
- All the types we covered so far except for functions are part of Ord
- Ord covers all the standard comparing functions such as >, <, >= and <=
- `compare` func takes two same type Ord members and returns an `Ordering`
- `Ordering` is a type that can be GT, LT or EQ
- To be a member of Ord, a type must first be in `Eq`




* Ord
  * functions:
    - compare
    - (<), (<=), (>=), (>)
    - max, min

```hs
class Eq a => Ord a where
    compare :: a -> a -> Ordering
    (<)     :: a -> a -> Bool
    (<=)    :: a -> a -> Bool
    (>)     :: a -> a -> Bool
    (>=)    :: a -> a -> Bool
    max     :: a -> a -> a
    min     :: a -> a -> a
-- MINIMAL: compare | (<=)
```
