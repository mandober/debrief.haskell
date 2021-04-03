# Test.QuickCheck.Features

```hs
Test.QuickCheck.Features.features ::
  [String]
  -> Data.Set.Internal.Set String -> Data.Set.Internal.Set String

Test.QuickCheck.Features.prop_noNewFeatures ::
  Testable prop => Data.Set.Internal.Set String -> prop -> Property

-- imported via Test.QuickCheck
labelledExamples           :: Testable prop => prop -> IO ()
labelledExamplesResult     :: Testable prop => prop -> IO Result

labelledExamplesWith       :: Testable prop => Args -> prop -> IO ()
labelledExamplesWithResult :: Testable prop => Args -> prop -> IO Result
```
