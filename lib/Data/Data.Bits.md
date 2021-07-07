# Data.Bits

```hs
import Data.Bits


(.&.) :: Bits a => a -> a -> a
(.|.) :: Bits a => a -> a -> a
xor   :: Bits a => a -> a -> a

isSigned            :: Bits a          => a        -> Bool
testBit             :: Bits a          => a -> Int -> Bool
testBitDefault      :: (Bits a, Num a) => a -> Int -> Bool

zeroBits            :: Bits a => a
complement          :: Bits a => a -> a

popCount            :: Bits a => a -> Int
bitSize             :: Bits a => a -> Int

bit                 ::  Bits a         => Int -> a
bitDefault          :: (Bits a, Num a) => Int -> a
bitSizeMaybe        ::  Bits a         => a -> Maybe Int

countLeadingZeros   :: FiniteBits b => b -> Int
countTrailingZeros  :: FiniteBits b => b -> Int
finiteBitSize       :: FiniteBits b => b -> Int

popCountDefault     :: (Bits a, Num a) => a -> Int
toIntegralSized     :: (Integral a, Integral b, Bits a, Bits b) => a -> Maybe b

clearBit        :: Bits a => a -> Int -> a
setBit          :: Bits a => a -> Int -> a
complementBit   :: Bits a => a -> Int -> a
rotate          :: Bits a => a -> Int -> a
rotateL         :: Bits a => a -> Int -> a
rotateR         :: Bits a => a -> Int -> a
shift           :: Bits a => a -> Int -> a
shiftL          :: Bits a => a -> Int -> a
shiftR          :: Bits a => a -> Int -> a
unsafeShiftL    :: Bits a => a -> Int -> a
unsafeShiftR    :: Bits a => a -> Int -> a

class Eq a => Bits a
  ...
class Bits b => FiniteBits b
  ...

```
