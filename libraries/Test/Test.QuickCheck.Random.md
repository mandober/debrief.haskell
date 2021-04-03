# Test.QuickCheck.Random

```hs
Test.QuickCheck.Random.QCGen ::
  System.Random.SplitMix.SMGen -> Test.QuickCheck.Random.QCGen
type Test.QuickCheck.Random.QCGen :: *
newtype Test.QuickCheck.Random.QCGen = ...
type Test.QuickCheck.Random.Splittable :: * -> Constraint
class Test.QuickCheck.Random.Splittable a
  ...
Test.QuickCheck.Random.integerVariant ::
  Test.QuickCheck.Random.Splittable a => Integer -> a -> a
Test.QuickCheck.Random.left ::
  Test.QuickCheck.Random.Splittable a => a -> a
Test.QuickCheck.Random.mkQCGen ::
  Int -> Test.QuickCheck.Random.QCGen
Test.QuickCheck.Random.newQCGen :: IO Test.QuickCheck.Random.QCGen
Test.QuickCheck.Random.right ::
  Test.QuickCheck.Random.Splittable a => a -> a
Test.QuickCheck.Random.wrapQCGen ::
  (System.Random.SplitMix.SMGen -> (a, System.Random.SplitMix.SMGen))
  -> Test.QuickCheck.Random.QCGen
  -> (a, Test.QuickCheck.Random.QCGen)
```
