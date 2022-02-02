# Test.QuickCheck.Arbitrary

```hs
type GSubterms :: (* -> *) -> * -> Constraint
class GSubterms f a where {- ... -}

type RecursivelyShrink :: forall {k}. (k -> *) -> Constraint
class RecursivelyShrink f where {- ... -}

-- imported via Test.QuickCheck
(><) :: (Gen a -> Gen a) -> (Gen a -> Gen a) -> Gen a -> Gen a

type Arbitrary :: * -> Constraint
class Arbitrary a where {- ... -}

type Arbitrary1 :: (* -> *) -> Constraint
class Arbitrary1 f where {- ... -}

type Arbitrary2 :: (* -> * -> *) -> Constraint
class Arbitrary2 f where {- ... -}

type CoArbitrary :: * -> Constraint
class CoArbitrary a where {- ... -}

applyArbitrary2 :: (Arbitrary a, Arbitrary b) => (a -> b -> r) -> Gen r

applyArbitrary3 :: (Arbitrary a, Arbitrary b, Arbitrary c)
                => (a -> b -> c -> r) -> Gen r

applyArbitrary4 :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
                => (a -> b -> c -> d -> r) -> Gen r

arbitrary :: Arbitrary a => Gen a
arbitrary1 :: (Arbitrary1 f, Arbitrary a) => Gen (f a)
arbitrary2 :: (Arbitrary2 f, Arbitrary a, Arbitrary b) => Gen (f a b)

arbitraryASCIIChar :: Gen Char
arbitraryBoundedEnum :: (Bounded a, Enum a) => Gen a
arbitraryBoundedIntegral :: (Bounded a, Integral a) => Gen a
arbitraryBoundedRandom :: (Bounded a, System.Random.Random a) => Gen a
arbitraryPrintableChar :: Gen Char
arbitrarySizedBoundedIntegral :: (Bounded a, Integral a) => Gen a
arbitrarySizedFractional :: Fractional a => Gen a
arbitrarySizedIntegral :: Integral a => Gen a
arbitrarySizedNatural :: Integral a => Gen a
arbitraryUnicodeChar :: Gen Char

coarbitrary :: CoArbitrary a => a -> Gen b -> Gen b
coarbitraryEnum :: Enum a => a -> Gen b -> Gen b
coarbitraryIntegral :: Integral a => a -> Gen b -> Gen b
coarbitraryReal :: Real a => a -> Gen b -> Gen b
coarbitraryShow :: Show a => a -> Gen b -> Gen b

genericCoarbitrary ::
  (GHC.Generics.Generic a,
   GCoArbitrary (GHC.Generics.Rep a)) =>
  a -> Gen b -> Gen b

genericShrink ::
  (GHC.Generics.Generic a,
   RecursivelyShrink (GHC.Generics.Rep a),
   GSubterms (GHC.Generics.Rep a) a) =>
  a -> [a]

infiniteList :: Arbitrary a => Gen [a]

liftArbitrary :: Arbitrary1 f => Gen a -> Gen (f a)
liftArbitrary2 :: Arbitrary2 f => Gen a -> Gen b -> Gen (f a b)
liftShrink :: Arbitrary1 f => (a -> [a]) -> f a -> [f a]
liftShrink2 :: Arbitrary2 f => (a -> [a]) -> (b -> [b]) -> f a b -> [f a b]
orderedList :: (Ord a, Arbitrary a) => Gen [a]

recursivelyShrink ::
  (GHC.Generics.Generic a,
   RecursivelyShrink
     (GHC.Generics.Rep a)) =>
  a -> [a]

shrink :: Arbitrary a => a -> [a]
shrink1 :: (Arbitrary1 f, Arbitrary a) => f a -> [f a]
shrink2 :: (Arbitrary2 f, Arbitrary a, Arbitrary b) => f a b -> [f a b]
shrinkDecimal :: RealFrac a => a -> [a]
shrinkIntegral :: Integral a => a -> [a]
shrinkList :: (a -> [a]) -> [a] -> [[a]]
shrinkMap :: Arbitrary a => (a -> b) -> (b -> a) -> b -> [b]
shrinkMapBy :: (a -> b) -> (b -> a) -> (a -> [a]) -> b -> [b]
shrinkNothing :: a -> [a]
shrinkRealFrac :: RealFrac a => a -> [a]

subterms ::
  (GHC.Generics.Generic a,
   GSubterms (GHC.Generics.Rep a) a) =>
  a -> [a]

vector :: Arbitrary a => Int -> Gen [a]
```

Test.QuickCheck.All
Test.QuickCheck.Arbitrary
Test.QuickCheck.Exception
Test.QuickCheck.Features
Test.QuickCheck.Function
Test.QuickCheck.Gen
Test.QuickCheck.Gen.Unsafe
Test.QuickCheck.Modifiers
Test.QuickCheck.Monadic
Test.QuickCheck.Poly
Test.QuickCheck.Property
Test.QuickCheck.Random
Test.QuickCheck.State
Test.QuickCheck.Test
Test.QuickCheck.Text
