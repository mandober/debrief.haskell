# GHC.Num

```hs
import GHC.Num

NatJ# :: integer-gmp-1.0.2.0:GHC.Integer.Type.BigNat -> Natural
NatS# :: integer-gmp-1.0.2.0:GHC.Integer.Type.GmpLimb# -> Natural

data Natural -- = ...

absInteger          :: Integer -> Integer
andInteger          :: Integer -> Integer -> Integer
andNatural          :: Natural -> Natural -> Natural
bitInteger          :: GHC.Prim.Int# -> Integer
bitNatural          :: GHC.Prim.Int# -> Natural
compareInteger      :: Integer -> Integer -> Ordering
complementInteger   :: Integer -> Integer
decodeDoubleInteger :: GHC.Prim.Double# -> (# Integer, GHC.Prim.Int# #)
divInteger          :: Integer -> Integer -> Integer
divModInteger       :: Integer -> Integer -> (# Integer, Integer #)
doubleFromInteger   :: Integer -> GHC.Prim.Double#
encodeDoubleInteger :: Integer -> GHC.Prim.Int# -> GHC.Prim.Double#
encodeFloatInteger  :: Integer -> GHC.Prim.Int# -> GHC.Prim.Float#
eqInteger           :: Integer -> Integer -> Bool
eqInteger#          :: Integer -> Integer -> GHC.Prim.Int#
floatFromInteger    :: Integer -> GHC.Prim.Float#
gcdNatural          :: Natural -> Natural -> Natural
geInteger           :: Integer -> Integer -> Bool
geInteger#          :: Integer -> Integer -> GHC.Prim.Int#
gtInteger           :: Integer -> Integer -> Bool
gtInteger#          :: Integer -> Integer -> GHC.Prim.Int#
hashInteger         :: Integer -> GHC.Prim.Int#
intToNatural        :: Int -> Natural
integerToInt        :: Integer -> GHC.Prim.Int#
integerToWord       :: Integer -> GHC.Prim.Word#
isValidNatural      :: Natural -> Bool
lcmNatural          :: Natural -> Natural -> Natural
leInteger           :: Integer -> Integer -> Bool
leInteger#          :: Integer -> Integer -> GHC.Prim.Int#
ltInteger           :: Integer -> Integer -> Bool
ltInteger#          :: Integer -> Integer -> GHC.Prim.Int#
minusInteger        :: Integer -> Integer -> Integer
minusNatural        :: Natural -> Natural -> Natural
minusNaturalMaybe   :: Natural -> Natural -> Maybe Natural
mkInteger           :: Bool -> [Int] -> Integer
mkNatural           :: [Word] -> Natural
modInteger          :: Integer -> Integer -> Integer
naturalFromInteger  :: Integer -> Natural
naturalToInt        :: Natural -> Int
naturalToInteger    :: Natural -> Integer
naturalToWord       :: Natural -> Word
naturalToWordMaybe  :: Natural -> Maybe Word
negateInteger       :: Integer -> Integer
negateNatural       :: Natural -> Natural
neqInteger          :: Integer -> Integer -> Bool
neqInteger#         :: Integer -> Integer -> GHC.Prim.Int#
orInteger           :: Integer -> Integer -> Integer
orNatural           :: Natural -> Natural -> Natural
plusInteger         :: Integer -> Integer -> Integer
plusNatural         :: Natural -> Natural -> Natural
popCountInteger     :: Integer -> GHC.Prim.Int#
popCountNatural     :: Natural -> Int
powModNatural       :: Natural -> Natural -> Natural -> Natural
quotInteger         :: Integer -> Integer -> Integer
quotNatural         :: Natural -> Natural -> Natural
quotRemInteger      :: Integer -> Integer -> (# Integer, Integer #)
quotRemNatural      :: Natural -> Natural -> (Natural, Natural)
remInteger          :: Integer -> Integer -> Integer
remNatural          :: Natural -> Natural -> Natural
shiftLInteger       :: Integer -> GHC.Prim.Int# -> Integer
shiftLNatural       :: Natural -> Int -> Natural
shiftRInteger       :: Integer -> GHC.Prim.Int# -> Integer
shiftRNatural       :: Natural -> Int -> Natural
signumInteger       :: Integer -> Integer
signumNatural       :: Natural -> Natural
smallInteger        :: GHC.Prim.Int# -> Integer
testBitInteger      :: Integer -> GHC.Prim.Int# -> Bool
testBitNatural      :: Natural -> Int -> Bool
timesInteger        :: Integer -> Integer -> Integer
timesNatural        :: Natural -> Natural -> Natural
wordToInteger       :: GHC.Prim.Word# -> Integer
wordToNatural       :: Word -> Natural
wordToNatural#      :: GHC.Prim.Word# -> Natural
wordToNaturalBase   :: GHC.Prim.Word# -> Natural
xorInteger          :: Integer -> Integer -> Integer
xorNatural          :: Natural -> Natural -> Natural


-- imported via Prelude, GHC.Num
(*) :: Num a => a -> a -> a
(+) :: Num a => a -> a -> a
(-) :: Num a => a -> a -> a

class Num a ...

abs         :: Num a => a -> a
fromInteger :: Num a => Integer -> a
negate      :: Num a => a -> a
signum      :: Num a => a -> a
subtract    :: Num a => a -> a -> a

data Integer -- = ...
```
