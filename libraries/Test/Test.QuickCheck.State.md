# Test.QuickCheck.State

```hs
Test.QuickCheck.State.MkState ::
  Test.QuickCheck.Text.Terminal
  -> Int
  -> Int
  -> Maybe Confidence
  -> (Int -> Int -> Int)
  -> Int
  -> Int
  -> Int
  -> Int
  -> Data.Map.Internal.Map [String] Int
  -> Data.Map.Internal.Map String Int
  -> Data.Map.Internal.Map String (Data.Map.Internal.Map String Int)
  -> Data.Map.Internal.Map (Maybe String, String) Double
  -> Bool
  -> Test.QuickCheck.Random.QCGen
  -> Int
  -> Int
  -> Int
  -> Test.QuickCheck.State.State


type Test.QuickCheck.State.State :: *
data Test.QuickCheck.State.State = ...

Test.QuickCheck.State.classes ::
  Test.QuickCheck.State.State -> Data.Map.Internal.Map String Int
Test.QuickCheck.State.computeSize ::
  Test.QuickCheck.State.State -> Int -> Int -> Int
Test.QuickCheck.State.coverageConfidence ::
  Test.QuickCheck.State.State -> Maybe Confidence
Test.QuickCheck.State.expected ::
  Test.QuickCheck.State.State -> Bool
Test.QuickCheck.State.labels ::
  Test.QuickCheck.State.State -> Data.Map.Internal.Map [String] Int
Test.QuickCheck.State.maxDiscardedRatio ::
  Test.QuickCheck.State.State -> Int
Test.QuickCheck.State.maxSuccessTests ::
  Test.QuickCheck.State.State -> Int
Test.QuickCheck.State.numDiscardedTests ::
  Test.QuickCheck.State.State -> Int
Test.QuickCheck.State.numRecentlyDiscardedTests ::
  Test.QuickCheck.State.State -> Int
Test.QuickCheck.State.numSuccessShrinks ::
  Test.QuickCheck.State.State -> Int
Test.QuickCheck.State.numSuccessTests ::
  Test.QuickCheck.State.State -> Int
Test.QuickCheck.State.numTotMaxShrinks ::
  Test.QuickCheck.State.State -> Int
Test.QuickCheck.State.numTotTryShrinks ::
  Test.QuickCheck.State.State -> Int
Test.QuickCheck.State.numTryShrinks ::
  Test.QuickCheck.State.State -> Int
Test.QuickCheck.State.randomSeed ::
  Test.QuickCheck.State.State -> Test.QuickCheck.Random.QCGen
Test.QuickCheck.State.requiredCoverage ::
  Test.QuickCheck.State.State
  -> Data.Map.Internal.Map (Maybe String, String) Double
Test.QuickCheck.State.tables ::
  Test.QuickCheck.State.State
  -> Data.Map.Internal.Map String (Data.Map.Internal.Map String Int)
Test.QuickCheck.State.terminal ::
  Test.QuickCheck.State.State -> Test.QuickCheck.Text.Terminal


-- imported via Test.QuickCheck
Confidence :: Integer -> Double -> Confidence

type Confidence :: *
data Confidence = ...Etc

certainty :: Confidence -> Integer
tolerance :: Confidence -> Double
```
