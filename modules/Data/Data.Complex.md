# Data.Complex

```hs
import Data.Complex

(:+) :: a -> a -> Complex a

data Complex a = ...E

cis       :: Floating a => a -> Complex a
conjugate :: Num a => Complex a -> Complex a
imagPart  :: Complex a -> a
magnitude :: RealFloat a => Complex a -> a
mkPolar   :: Floating a => a -> a -> Complex a
phase     :: RealFloat a => Complex a -> a
polar     :: RealFloat a => Complex a -> (a, a)
realPart  :: Complex a -> a
```
