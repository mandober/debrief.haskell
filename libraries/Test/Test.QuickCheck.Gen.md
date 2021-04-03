# Test.QuickCheck.Gen

```hs
Test.QuickCheck.Gen.MkGen ::
  (Test.QuickCheck.Random.QCGen -> Int -> a) -> Gen a

Test.QuickCheck.Gen.chooseInt64 ::
  (GHC.Int.Int64, GHC.Int.Int64) -> Gen GHC.Int.Int64

Test.QuickCheck.Gen.chooseUpTo ::
  GHC.Word.Word64 -> Gen GHC.Word.Word64

Test.QuickCheck.Gen.chooseWord64 ::
  (GHC.Word.Word64, GHC.Word.Word64) -> Gen GHC.Word.Word64

Test.QuickCheck.Gen.unGen ::
  Gen a -> Test.QuickCheck.Random.QCGen -> Int -> a

-- imported via Test.QuickCheck
type Gen :: * -> *
newtype Gen a = {- ... -} ETC

choose        :: System.Random.Random a => (a, a) -> Gen a
chooseAny     :: System.Random.Random a => Gen a

chooseBoundedIntegral :: (Bounded a, Integral a) => (a, a) -> Gen a

chooseEnum    :: Enum a => (a, a) -> Gen a
chooseInt     :: (Int, Int) -> Gen Int
chooseInteger :: (Integer, Integer) -> Gen Integer

elements        :: [a] -> Gen a
growingElements :: [a] -> Gen a

frequency :: [(Int, Gen a)] -> Gen a
generate  :: Gen a -> IO a
getSize   :: Gen Int

infiniteListOf  :: Gen a -> Gen [a]
listOf          :: Gen a -> Gen [a]
listOf1         :: Gen a -> Gen [a]

oneof   :: [Gen a] -> Gen a
resize  :: Int -> Gen a -> Gen a

sample  :: Show a => Gen a -> IO ()
sample' :: Gen a -> IO [a]

scale     :: (Int -> Int) -> Gen a -> Gen a
shuffle   :: [a] -> Gen [a]
sized     :: (Int -> Gen a) -> Gen a
sublistOf :: [a] -> Gen [a]

suchThat      :: Gen a -> (a -> Bool) -> Gen a
suchThatMap   :: Gen a -> (a -> Maybe b) -> Gen b
suchThatMaybe :: Gen a -> (a -> Bool) -> Gen (Maybe a)

variant :: Integral n => n -> Gen a -> Gen a
vectorOf :: Int -> Gen a -> Gen [a]
```
