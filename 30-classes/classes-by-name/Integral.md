


```hs
class (Real a, Enum a) => Integral a where
    quot        :: a -> a -> a
    rem         :: a -> a -> a
    div         :: a -> a -> a
    mod         :: a -> a -> a
    quotRem     :: a -> a -> (a, a)
    divMod      :: a -> a -> (a, a)
    toInteger   :: a -> Integer
-- MINIMAL: quotRem, toInteger
```


**Integral**
- includes Int and Integer
- useful function for dealing with numbers is fromIntegral:
- `fromIntegral :: (Num b, Integral a) => a -> b`
