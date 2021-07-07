# GHC.Natural

```hs
data Natural -- = ...

NatJ# :: integer-gmp-1.0.2.0:GHC.Integer.Type.BigNat -> Natural
NatS# :: integer-gmp-1.0.2.0:GHC.Integer.Type.GmpLimb# -> Natural

andNatural         :: Natural -> Natural -> Natural
orNatural          :: Natural -> Natural -> Natural
xorNatural         :: Natural -> Natural -> Natural
timesNatural       :: Natural -> Natural -> Natural
plusNatural        :: Natural -> Natural -> Natural
minusNatural       :: Natural -> Natural -> Natural
minusNaturalMaybe  :: Natural -> Natural -> Maybe Natural
gcdNatural         :: Natural -> Natural -> Natural
lcmNatural         :: Natural -> Natural -> Natural
quotNatural        :: Natural -> Natural -> Natural
remNatural         :: Natural -> Natural -> Natural
powModNatural      :: Natural -> Natural -> Natural -> Natural
quotRemNatural     :: Natural -> Natural -> (Natural, Natural)

isValidNatural     :: Natural -> Bool
popCountNatural    :: Natural -> Int

negateNatural      :: Natural -> Natural
signumNatural      :: Natural -> Natural

testBitNatural     :: Natural -> Int -> Bool
shiftLNatural      :: Natural -> Int -> Natural
shiftRNatural      :: Natural -> Int -> Natural

naturalToInt       :: Natural -> Int
naturalToInteger   :: Natural -> Integer
naturalToWord      :: Natural -> Word
naturalToWordMaybe :: Natural -> Maybe Word

intToNatural       :: Int     -> Natural
naturalFromInteger :: Integer -> Natural
wordToNatural      :: Word    -> Natural
mkNatural          :: [Word]  -> Natural

bitNatural         :: GHC.Prim.Int# -> Natural
wordToNatural#     :: GHC.Prim.Word# -> Natural
wordToNaturalBase  :: GHC.Prim.Word# -> Natural
```
