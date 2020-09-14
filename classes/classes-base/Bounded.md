

```hs
class Bounded a where
    minBound :: a
    maxBound :: a
-- MINIMAL: minBound, maxBound
```

**Bounded**
- members have an upper and a lower bound, `minBound` and `maxBound` funcs
- minBound and maxBound have a type of `(Bounded a) => a`
- In a sense they are polymorphic constants
- members: Bool, Char, Int
- tuples are part of Bounded if the components are also in it
