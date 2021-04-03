# Test.QuickCheck.Property

```hs
type Test.QuickCheck.Property.Callback :: *
data Test.QuickCheck.Property.Callback = ...
type Test.QuickCheck.Property.CallbackKind :: *
data Test.QuickCheck.Property.CallbackKind = ...
Test.QuickCheck.Property.Counterexample ::
  Test.QuickCheck.Property.CallbackKind
Test.QuickCheck.Property.IORose ::
  IO (Test.QuickCheck.Property.Rose a)
  -> Test.QuickCheck.Property.Rose a
Test.QuickCheck.Property.MkProp ::
  Test.QuickCheck.Property.Rose Test.QuickCheck.Property.Result
  -> Test.QuickCheck.Property.Prop
Test.QuickCheck.Property.MkProperty ::
  Gen Test.QuickCheck.Property.Prop -> Property
Test.QuickCheck.Property.MkResult ::
  Maybe Bool
  -> Bool
  -> String
  -> Maybe Test.QuickCheck.Exception.AnException
  -> Bool
  -> Maybe Int
  -> Maybe Confidence
  -> [String]
  -> [String]
  -> [(String, String)]
  -> [(Maybe String, String, Double)]
  -> [Test.QuickCheck.Property.Callback]
  -> [String]
  -> Test.QuickCheck.Property.Result
Test.QuickCheck.Property.MkRose ::
  a
  -> [Test.QuickCheck.Property.Rose a]
  -> Test.QuickCheck.Property.Rose a
Test.QuickCheck.Property.NotCounterexample ::
  Test.QuickCheck.Property.CallbackKind
Test.QuickCheck.Property.PostFinalFailure ::
  Test.QuickCheck.Property.CallbackKind
  -> (Test.QuickCheck.State.State
      -> Test.QuickCheck.Property.Result -> IO ())
  -> Test.QuickCheck.Property.Callback
Test.QuickCheck.Property.PostTest ::
  Test.QuickCheck.Property.CallbackKind
  -> (Test.QuickCheck.State.State
      -> Test.QuickCheck.Property.Result -> IO ())
  -> Test.QuickCheck.Property.Callback
type Test.QuickCheck.Property.Prop :: *
newtype Test.QuickCheck.Property.Prop = ...
type Test.QuickCheck.Property.Result :: *
data Test.QuickCheck.Property.Result = ...
type Test.QuickCheck.Property.Rose :: * -> *
data Test.QuickCheck.Property.Rose a = ...
Test.QuickCheck.Property.abort ::
  Test.QuickCheck.Property.Result -> Bool
Test.QuickCheck.Property.callback ::
  Testable prop =>
  Test.QuickCheck.Property.Callback -> prop -> Property
Test.QuickCheck.Property.callbacks ::
  Test.QuickCheck.Property.Result
  -> [Test.QuickCheck.Property.Callback]
Test.QuickCheck.Property.classes ::
  Test.QuickCheck.Property.Result -> [String]
Test.QuickCheck.Property.exception ::
  String
  -> Test.QuickCheck.Exception.AnException
  -> Test.QuickCheck.Property.Result
Test.QuickCheck.Property.expect ::
  Test.QuickCheck.Property.Result -> Bool

Test.QuickCheck.Property.failed    :: Test.QuickCheck.Property.Result
Test.QuickCheck.Property.succeeded :: Test.QuickCheck.Property.Result

Test.QuickCheck.Property.formatException ::
  String -> Test.QuickCheck.Exception.AnException -> String
Test.QuickCheck.Property.ioRose ::
  IO (Test.QuickCheck.Property.Rose Test.QuickCheck.Property.Result)
  -> Test.QuickCheck.Property.Rose Test.QuickCheck.Property.Result
Test.QuickCheck.Property.joinRose ::
  Test.QuickCheck.Property.Rose (Test.QuickCheck.Property.Rose a)
  -> Test.QuickCheck.Property.Rose a
Test.QuickCheck.Property.labels ::
  Test.QuickCheck.Property.Result -> [String]
Test.QuickCheck.Property.liftBool ::
  Bool -> Test.QuickCheck.Property.Result
Test.QuickCheck.Property.mapProp ::
  Testable prop =>
  (Test.QuickCheck.Property.Prop -> Test.QuickCheck.Property.Prop)
  -> prop -> Property
Test.QuickCheck.Property.mapResult ::
  Testable prop =>
  (Test.QuickCheck.Property.Result
   -> Test.QuickCheck.Property.Result)
  -> prop -> Property
Test.QuickCheck.Property.mapRoseResult ::
  Testable prop =>
  (Test.QuickCheck.Property.Rose Test.QuickCheck.Property.Result
   -> Test.QuickCheck.Property.Rose Test.QuickCheck.Property.Result)
  -> prop -> Property
Test.QuickCheck.Property.mapTotalResult ::
  Testable prop =>
  (Test.QuickCheck.Property.Result
   -> Test.QuickCheck.Property.Result)
  -> prop -> Property
Test.QuickCheck.Property.maybeCheckCoverage ::
  Test.QuickCheck.Property.Result -> Maybe Confidence
Test.QuickCheck.Property.maybeNumTests ::
  Test.QuickCheck.Property.Result -> Maybe Int
Test.QuickCheck.Property.morallyDubiousIOProperty ::
  Testable prop => IO prop -> Property
Test.QuickCheck.Property.ok ::
  Test.QuickCheck.Property.Result -> Maybe Bool

Test.QuickCheck.Property.onRose ::
  (a
   -> [Test.QuickCheck.Property.Rose a]
   -> Test.QuickCheck.Property.Rose a)
  -> Test.QuickCheck.Property.Rose a
  -> Test.QuickCheck.Property.Rose a
Test.QuickCheck.Property.protect ::
  (Test.QuickCheck.Exception.AnException -> a) -> IO a -> IO a
Test.QuickCheck.Property.protectProp ::
  Test.QuickCheck.Property.Prop -> Test.QuickCheck.Property.Prop
Test.QuickCheck.Property.protectResult ::
  IO Test.QuickCheck.Property.Result
  -> IO Test.QuickCheck.Property.Result
Test.QuickCheck.Property.protectResults ::
  Test.QuickCheck.Property.Rose Test.QuickCheck.Property.Result
  -> Test.QuickCheck.Property.Rose Test.QuickCheck.Property.Result
Test.QuickCheck.Property.protectRose ::
  IO (Test.QuickCheck.Property.Rose Test.QuickCheck.Property.Result)
  -> IO
       (Test.QuickCheck.Property.Rose Test.QuickCheck.Property.Result)
Test.QuickCheck.Property.reason ::
  Test.QuickCheck.Property.Result -> String
Test.QuickCheck.Property.reduceRose ::
  Test.QuickCheck.Property.Rose Test.QuickCheck.Property.Result
  -> IO
       (Test.QuickCheck.Property.Rose Test.QuickCheck.Property.Result)
Test.QuickCheck.Property.rejected ::
  Test.QuickCheck.Property.Result
Test.QuickCheck.Property.requiredCoverage ::
  Test.QuickCheck.Property.Result -> [(Maybe String, String, Double)]
Test.QuickCheck.Property.showCounterexample :: String -> IO String


Test.QuickCheck.Property.tables ::
  Test.QuickCheck.Property.Result -> [(String, String)]
Test.QuickCheck.Property.testCase ::
  Test.QuickCheck.Property.Result -> [String]
Test.QuickCheck.Property.theException ::
  Test.QuickCheck.Property.Result
  -> Maybe Test.QuickCheck.Exception.AnException
Test.QuickCheck.Property.unProp ::
  Test.QuickCheck.Property.Prop
  -> Test.QuickCheck.Property.Rose Test.QuickCheck.Property.Result
Test.QuickCheck.Property.unProperty ::
  Property -> Gen Test.QuickCheck.Property.Prop
-- imported via Test.QuickCheck
(.&&.) ::
  (Testable prop1, Testable prop2) => prop1 -> prop2 -> Property
(.&.) ::
  (Testable prop1, Testable prop2) => prop1 -> prop2 -> Property
(.||.) ::
  (Testable prop1, Testable prop2) => prop1 -> prop2 -> Property
(=/=) :: (Eq a, Show a) => a -> a -> Property
(===) :: (Eq a, Show a) => a -> a -> Property
(==>) :: Testable prop => Bool -> prop -> Property
Discard :: Discard
type Discard :: *
data Discard = ...
type Property :: *
newtype Property = ...
type Testable :: * -> Constraint
class Testable prop
  ...
again :: Testable prop => prop -> Property
checkCoverage :: Testable prop => prop -> Property
checkCoverageWith ::
  Testable prop => Confidence -> prop -> Property
classify :: Testable prop => Bool -> String -> prop -> Property
collect :: (Show a, Testable prop) => a -> prop -> Property
conjoin :: Testable prop => [prop] -> Property
counterexample :: Testable prop => String -> prop -> Property
cover ::
  Testable prop => Double -> Bool -> String -> prop -> Property
coverTable ::
  Testable prop => String -> [(String, Double)] -> prop -> Property
disjoin :: Testable prop => [prop] -> Property
expectFailure :: Testable prop => prop -> Property
forAll ::
  (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
forAllBlind :: Testable prop => Gen a -> (a -> prop) -> Property
forAllShow ::
  Testable prop => Gen a -> (a -> String) -> (a -> prop) -> Property
forAllShrink ::
  (Show a, Testable prop) =>
  Gen a -> (a -> [a]) -> (a -> prop) -> Property
forAllShrinkBlind ::
  Testable prop => Gen a -> (a -> [a]) -> (a -> prop) -> Property
forAllShrinkShow ::
  Testable prop =>
  Gen a -> (a -> [a]) -> (a -> String) -> (a -> prop) -> Property
idempotentIOProperty :: Testable prop => IO prop -> Property
ioProperty :: Testable prop => IO prop -> Property
label :: Testable prop => String -> prop -> Property
mapSize :: Testable prop => (Int -> Int) -> prop -> Property
noShrinking :: Testable prop => prop -> Property
once :: Testable prop => prop -> Property
printTestCase :: Testable prop => String -> prop -> Property
property :: Testable prop => prop -> Property
propertyForAllShrinkShow ::
  Testable prop =>
  Gen a -> (a -> [a]) -> (a -> [String]) -> (a -> prop) -> Property
shrinking ::
  Testable prop => (a -> [a]) -> a -> (a -> prop) -> Property
stdConfidence :: Confidence
tabulate :: Testable prop => String -> [String] -> prop -> Property
total :: Control.DeepSeq.NFData a => a -> Property
verbose :: Testable prop => prop -> Property
verboseShrinking :: Testable prop => prop -> Property
whenFail :: Testable prop => IO () -> prop -> Property
whenFail' :: Testable prop => IO () -> prop -> Property
withMaxSuccess :: Testable prop => Int -> prop -> Property
within :: Testable prop => Int -> prop -> Property
```
