

```hs
class Num a => Fractional a where
    (/) :: a -> a -> a
    recip :: a -> a
    fromRational :: Rational -> a
-- MINIMAL: fromRational, (recip | (/))
```
