# Test.QuickCheck.Test

```hs
Test.QuickCheck.Test.addCoverageCheck ::
  Confidence -> Test.QuickCheck.State.State -> Property -> Property
Test.QuickCheck.Test.allCoverage ::
  Test.QuickCheck.State.State
  -> [(Maybe String, String, Int, Int, Double)]
Test.QuickCheck.Test.callbackPostFinalFailure ::
  Test.QuickCheck.State.State
  -> Test.QuickCheck.Property.Result -> IO ()
Test.QuickCheck.Test.callbackPostTest ::
  Test.QuickCheck.State.State
  -> Test.QuickCheck.Property.Result
  -> IO Test.QuickCheck.Property.Result
Test.QuickCheck.Test.doneTesting ::
  Test.QuickCheck.State.State -> Property -> IO Result
Test.QuickCheck.Test.failureReason ::
  Test.QuickCheck.State.State
  -> Test.QuickCheck.Property.Result -> [String]
Test.QuickCheck.Test.failureSummary ::
  Test.QuickCheck.State.State
  -> Test.QuickCheck.Property.Result -> String
Test.QuickCheck.Test.failureSummaryAndReason ::
  Test.QuickCheck.State.State
  -> Test.QuickCheck.Property.Result -> (String, [String])
Test.QuickCheck.Test.foundFailure ::
  Test.QuickCheck.State.State
  -> Test.QuickCheck.Property.Result
  -> [Test.QuickCheck.Property.Rose Test.QuickCheck.Property.Result]
  -> IO (Int, Int, Int, Test.QuickCheck.Property.Result)
Test.QuickCheck.Test.giveUp ::
  Test.QuickCheck.State.State -> Property -> IO Result
Test.QuickCheck.Test.insufficientlyCovered ::
  Maybe Integer -> Int -> Int -> Double -> Bool
Test.QuickCheck.Test.invnormcdf :: Double -> Double
Test.QuickCheck.Test.labelsAndTables ::
  Test.QuickCheck.State.State -> ([String], [String])
Test.QuickCheck.Test.localMin ::
  Test.QuickCheck.State.State
  -> Test.QuickCheck.Property.Result
  -> [Test.QuickCheck.Property.Rose Test.QuickCheck.Property.Result]
  -> IO (Int, Int, Int, Test.QuickCheck.Property.Result)
Test.QuickCheck.Test.localMin' ::
  Test.QuickCheck.State.State
  -> Test.QuickCheck.Property.Result
  -> [Test.QuickCheck.Property.Rose Test.QuickCheck.Property.Result]
  -> IO (Int, Int, Int, Test.QuickCheck.Property.Result)
Test.QuickCheck.Test.localMinFound ::
  Test.QuickCheck.State.State
  -> Test.QuickCheck.Property.Result
  -> IO (Int, Int, Int, Test.QuickCheck.Property.Result)
Test.QuickCheck.Test.runATest ::
  Test.QuickCheck.State.State -> Property -> IO Result
Test.QuickCheck.Test.showTable ::
  Int -> Maybe String -> Data.Map.Internal.Map String Int -> [String]
Test.QuickCheck.Test.showTestCount ::
  Test.QuickCheck.State.State -> String
Test.QuickCheck.Test.success ::
  Test.QuickCheck.State.State -> IO ()
Test.QuickCheck.Test.sufficientlyCovered ::
  Confidence -> Int -> Int -> Double -> Bool
Test.QuickCheck.Test.test ::
  Test.QuickCheck.State.State -> Property -> IO Result
Test.QuickCheck.Test.wilson ::
  Integer -> Integer -> Double -> Double
Test.QuickCheck.Test.wilsonHigh ::
  Integer -> Integer -> Double -> Double
Test.QuickCheck.Test.wilsonLow ::
  Integer -> Integer -> Double -> Double
Test.QuickCheck.Test.withState ::
  Args -> (Test.QuickCheck.State.State -> IO a) -> IO a
-- imported via Test.QuickCheck
Args ::
  Maybe (Test.QuickCheck.Random.QCGen, Int)
  -> Int -> Int -> Int -> Bool -> Int -> Args
type Args :: *
data Args = ...
Failure ::
  Int
  -> Int
  -> Int
  -> Int
  -> Int
  -> Test.QuickCheck.Random.QCGen
  -> Int
  -> String
  -> Maybe Test.QuickCheck.Exception.AnException
  -> String
  -> [String]
  -> [String]
  -> Data.Set.Internal.Set String
  -> Result
GaveUp ::
  Int
  -> Int
  -> Data.Map.Internal.Map [String] Int
  -> Data.Map.Internal.Map String Int
  -> Data.Map.Internal.Map String (Data.Map.Internal.Map String Int)
  -> String
  -> Result
NoExpectedFailure ::
  Int
  -> Int
  -> Data.Map.Internal.Map [String] Int
  -> Data.Map.Internal.Map String Int
  -> Data.Map.Internal.Map String (Data.Map.Internal.Map String Int)
  -> String
  -> Result
type Result :: *
data Result = ...
Success ::
  Int
  -> Int
  -> Data.Map.Internal.Map [String] Int
  -> Data.Map.Internal.Map String Int
  -> Data.Map.Internal.Map String (Data.Map.Internal.Map String Int)
  -> String
  -> Result
chatty :: Args -> Bool
classes :: Result -> Data.Map.Internal.Map String Int
failingClasses :: Result -> Data.Set.Internal.Set String
failingLabels :: Result -> [String]
failingTestCase :: Result -> [String]
isSuccess :: Result -> Bool
labels :: Result -> Data.Map.Internal.Map [String] Int
maxDiscardRatio :: Args -> Int
maxShrinks :: Args -> Int
maxSize :: Args -> Int
maxSuccess :: Args -> Int
numDiscarded :: Result -> Int
numShrinkFinal :: Result -> Int
numShrinkTries :: Result -> Int
numShrinks :: Result -> Int
numTests :: Result -> Int
output :: Result -> String
quickCheck :: Testable prop => prop -> IO ()
quickCheckResult :: Testable prop => prop -> IO Result
quickCheckWith :: Testable prop => Args -> prop -> IO ()
quickCheckWithResult :: Testable prop => Args -> prop -> IO Result
reason :: Result -> String
replay :: Args -> Maybe (Test.QuickCheck.Random.QCGen, Int)
stdArgs :: Args
tables ::
  Result
  -> Data.Map.Internal.Map String (Data.Map.Internal.Map String Int)
theException ::
  Result -> Maybe Test.QuickCheck.Exception.AnException
usedSeed :: Result -> Test.QuickCheck.Random.QCGen
usedSize :: Result -> Int
verboseCheck :: Testable prop => prop -> IO ()
verboseCheckResult :: Testable prop => prop -> IO Result
verboseCheckWith :: Testable prop => Args -> prop -> IO ()
verboseCheckWithResult ::
  Testable prop => Args -> prop -> IO Result
```
