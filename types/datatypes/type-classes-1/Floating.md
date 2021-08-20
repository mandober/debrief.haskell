

```hs
class Fractional a => Floating a where
    pi      :: a
    exp     :: a -> a
    log     :: a -> a
    sqrt    :: a -> a
    (**)    :: a -> a -> a
    logBase :: a -> a -> a
    sin     :: a -> a
    cos     :: a -> a
    tan     :: a -> a
    asin    :: a -> a
    acos    :: a -> a
    atan    :: a -> a
    sinh    :: a -> a
    cosh    :: a -> a
    tanh    :: a -> a
    asinh   :: a -> a
    acosh   :: a -> a
    atanh   :: a -> a
    GHC.Float.log1p    :: a -> a
    GHC.Float.expm1    :: a -> a
    GHC.Float.log1pexp :: a -> a
    GHC.Float.log1mexp :: a -> a
-- pi, exp, log, sin , cos , asin , acos , atan,sinh, cosh, asinh, acosh, atanh
```



**Floating**
- includes Float and Double
