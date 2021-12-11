# Data.Bits

```hs
import Data.Bits

-- METHODS of the `Bits` class
toIntegralSized :: (Integral a, Integral b, Bits a, Bits b) => a -> Maybe b
testBitDefault  :: (Bits a, Num a) => a -> Int -> Bool
popCountDefault :: (Bits a, Num a) => a -> Int
bitDefault      :: (Bits a, Num a) => Int -> a
bit             :: Bits a => Int -> a
zeroBits        :: Bits a => a
complement      :: Bits a => a -> a
xor             :: Bits a => a -> a -> a
(.&.)           :: Bits a => a -> a -> a
(.|.)           :: Bits a => a -> a -> a
bitSizeMaybe    :: Bits a => a -> Maybe Int
isSigned        :: Bits a => a -> Bool
testBit         :: Bits a => a -> Int -> Bool
popCount        :: Bits a => a -> Int
bitSize         :: Bits a => a -> Int
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

-- METHODS of the `FiniteBits` class
countLeadingZeros   :: FiniteBits b => b -> Int
countTrailingZeros  :: FiniteBits b => b -> Int
finiteBitSize       :: FiniteBits b => b -> Int

type Bits :: Type -> Constraint
class Eq a => Bits a where
  (.&.)         :: a -> a -> a
  (.|.)         :: a -> a -> a
  xor           :: a -> a -> a
  zeroBits      :: a
  complement    :: a -> a
  bit           :: Int -> a
  isSigned      :: a -> Bool
  bitSizeMaybe  :: a -> Maybe Int
  bitSize       :: a -> Int
  popCount      :: a -> Int
  testBit       :: a -> Int -> Bool
  setBit        :: a -> Int -> a
  clearBit      :: a -> Int -> a
  complementBit :: a -> Int -> a
  unsafeShiftL  :: a -> Int -> a
  unsafeShiftR  :: a -> Int -> a
  shift         :: a -> Int -> a
  shiftL        :: a -> Int -> a
  shiftR        :: a -> Int -> a
  rotate        :: a -> Int -> a
  rotateL       :: a -> Int -> a
  rotateR       :: a -> Int -> a
{-# MINIMAL
    (.&.), (.|.), xor, complement,
    bitSize, bitSizeMaybe,
    isSigned, testBit, bit, popCount
    (shift | shiftL, shiftR),
    (rotate | rotateL, rotateR),
  #-}
instance Bits Word    -- Defined in 'Data.Bits'
instance Bits Integer -- Defined in 'Data.Bits'
instance Bits Int     -- Defined in 'Data.Bits'
instance Bits Bool    -- Defined in 'Data.Bits'



type FiniteBits :: Type -> Constraint
class Bits b => FiniteBits b where
    finiteBitSize      :: b -> Int
    countLeadingZeros  :: b -> Int
    countTrailingZeros :: b -> Int
{-# MINIMAL finiteBitSize #-}

instance FiniteBits Word -- Defined in 'Data.Bits'
instance FiniteBits Int  -- Defined in 'Data.Bits'
instance FiniteBits Bool -- Defined in 'Data.Bits'
```
