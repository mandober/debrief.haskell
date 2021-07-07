# Test.QuickCheck.Gen.Unsafe

```hs
Test.QuickCheck.Gen.Unsafe.Capture ::
  (forall a. Gen a -> a) -> Test.QuickCheck.Gen.Unsafe.Capture

type Test.QuickCheck.Gen.Unsafe.Capture :: *
newtype Test.QuickCheck.Gen.Unsafe.Capture = ...

Test.QuickCheck.Gen.Unsafe.capture ::
  Gen Test.QuickCheck.Gen.Unsafe.Capture

Test.QuickCheck.Gen.Unsafe.delay :: Gen (Gen a -> a)

Test.QuickCheck.Gen.Unsafe.promote ::
  Monad m => m (Gen a) -> Gen (m a)
```
