# Enum

- members are sequentially ordered enumerable types
- use member types in range expressions
- members: (), Bool, Char, Ordering

## Enum class definition

```hs
class Enum a where
    succ            :: a -> a
    pred            :: a -> a
    toEnum          :: Int -> a
    fromEnum        :: a -> Int
    enumFrom        :: a -> [a]
    enumFromThen    :: a -> a -> [a]
    enumFromTo      :: a -> a -> [a]
    enumFromThenTo  :: a -> a -> a -> [a]
-- MINIMAL: toEnum, fromEnum
```



## Ranges

Generate range from `m` to `n` (inclusive)
- enumFrom       :: Enum a => a -> [a]                [m..∞]
- enumFromThen   :: Enum a => a -> a -> [a]           [i_1,i_2,...,∞]
- enumFromTo     :: Enum a => a -> a -> [a]           [m..n]
- enumFromThenTo :: Enum a => a -> a -> a -> [a]      [i_1,i_2,...,i_n]



```hs
-- infinite range from 1 (partially apply 1 to enumFromTo)
nat1 = enumFromTo 1     -- = [1..n]

enumFrom :: Enum a => a -> [a]
enumFrom 1   -- ℕ

enumFromThen   :: Enum a => a -> a -> [a]
enumFromThen 1 3 -- [1,3,5,...,∞]


enumFromTo :: Enum a => a -> a -> [a]
enumFromTo 2 5   -- [2,3,4,5]

enumFromThenTo :: Enum a => a -> a -> a -> [a]
enumFromThenTo 1 3 11  -- [1,3,5,7,9,11]

-- there's no version with lower bound defaulting to 1
-- make it by partially appling m as 1, m=1
enumTo :: Enum a => a -> [a]
enumTo n = enumFromTo 1

-- ranges
[1..5] :: (Enum t, Num t) => [t]
[1..5]   -- [1,2,3,4,5]

-- ops
map (+1) [1..4]
map (\x -> x + 5) [5..15]
sum [1..10]

-- Haskell knows Greek
putStrLn ['α'..'ω']
```

## Enum info


```hs
:info! Enum

instance Enum Foreign.C.Types.CWchar

instance Enum Foreign.C.Types.CUShort

instance Enum Foreign.C.Types.CUSeconds

instance Enum Foreign.C.Types.CULong

instance Enum Foreign.C.Types.CULLong

instance Enum Foreign.C.Types.CUIntPtr

instance Enum Foreign.C.Types.CUIntMax

instance Enum Foreign.C.Types.CUInt
instance Enum Foreign.C.Types.CUChar

instance Enum Foreign.C.Types.CTime
instance Enum Foreign.C.Types.CSize
instance Enum Foreign.C.Types.CSigAtomic

instance Enum Foreign.C.Types.CShort

instance Enum Foreign.C.Types.CSUSeconds

instance Enum Foreign.C.Types.CSChar

instance Enum Foreign.C.Types.CPtrdiff

instance Enum Foreign.C.Types.CLong
instance Enum Foreign.C.Types.CLLong

instance Enum Foreign.C.Types.CIntPtr

instance Enum Foreign.C.Types.CIntMax

instance Enum Foreign.C.Types.CInt
instance Enum Foreign.C.Types.CFloat

instance Enum Foreign.C.Types.CDouble

instance Enum Foreign.C.Types.CClock

instance Enum Foreign.C.Types.CChar
instance Enum Foreign.C.Types.CBool
instance Enum (Data.Fixed.Fixed a) -- Defined in 'Data.Fixed'
instance Enum a => Enum (Data.Semigroup.WrappedMonoid a)

instance Enum a => Enum (Data.Semigroup.Min a)

instance Enum a => Enum (Data.Semigroup.Max a)

instance Enum a => Enum (Data.Semigroup.Last a)

instance Enum a => Enum (Data.Semigroup.First a)

instance Enum Foreign.Ptr.WordPtr
instance Enum Foreign.Ptr.IntPtr
instance forall k1 k2 (a :: k1) (b :: k2).
         (a ~~ b) =>
         Enum (a Data.Type.Equality.:~~: b)
  -- Defined in 'Data.Type.Equality'
instance forall k (a :: k) (b :: k).
         (a ~ b) =>
         Enum (a Data.Type.Equality.:~: b)
  -- Defined in 'Data.Type.Equality'
instance forall k (a :: k) (b :: k).
         Coercible a b =>
         Enum (Data.Type.Coercion.Coercion a b)
  -- Defined in 'Data.Type.Coercion'
instance Enum GHC.Int.Int8
instance Enum GHC.Int.Int64
instance Enum GHC.Int.Int32
instance Enum GHC.Int.Int16
instance Enum GHC.Unicode.GeneralCategory

instance Enum GHC.Word.Word8
instance Enum GHC.Word.Word64
instance Enum GHC.Word.Word32
instance Enum GHC.Word.Word16
instance forall a k (b :: k).
         Enum a =>
         Enum (Data.Functor.Const.Const a b)
  -- Defined in 'Data.Functor.Const'
instance Enum a => Enum (Data.Functor.Identity.Identity a)
  -- Defined in 'Data.Functor.Identity'
instance forall k (f :: k -> *) (a :: k).
         Enum (f a) =>
         Enum (Data.Monoid.Ap f a)
  -- Defined in 'Data.Monoid'
instance forall k (s :: k). Enum (Data.Proxy.Proxy s)
  -- Defined in 'Data.Proxy'
instance forall k (f :: k -> *) (a :: k).
         Enum (f a) =>
         Enum (base-4.13.0.0:Data.Semigroup.Internal.Alt f a)
  -- Defined in 'base-4.13.0.0:Data.Semigroup.Internal'
instance Enum GHC.Generics.SourceUnpackedness

instance Enum GHC.Generics.SourceStrictness

instance Enum GHC.Generics.DecidedStrictness

instance Enum GHC.Generics.Associativity

instance Enum Word
instance Enum GHC.Types.VecElem
instance Enum GHC.Types.VecCount
instance Enum Ordering
instance Enum GHC.Natural.Natural
instance Enum Integer
instance Enum Int
instance Enum Char
instance Enum Bool
instance Enum ()
instance Integral a => Enum (Ratio a) -- Defined in 'GHC.Real'
instance Enum Float -- Defined in 'GHC.Float'
instance Enum Double -- Defined in 'GHC.Float'
```
