# Test.QuickCheck.Exception

```hs
type AnException :: *
type AnException = GHC.Exception.Type.SomeException

isDiscard     :: AnException -> Bool
isInterrupt   :: AnException -> Bool

evaluate      :: a -> IO a
tryEvaluate   :: a -> IO (Either AnException a)

finally       :: IO a -> IO b -> IO a
tryEvaluateIO :: IO a -> IO (Either AnException a)

-- imported via Test.QuickCheck
discard :: a
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
