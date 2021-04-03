# Test.QuickCheck.Monadic

```hs
Test.QuickCheck.Monadic.MkPropertyM ::
  ((a -> Gen (m Property)) -> Gen (m Property))
  -> Test.QuickCheck.Monadic.PropertyM m a
type Test.QuickCheck.Monadic.PropertyM :: (* -> *) -> * -> *
newtype Test.QuickCheck.Monadic.PropertyM m a = ...
Test.QuickCheck.Monadic.assert ::
  Monad m => Bool -> Test.QuickCheck.Monadic.PropertyM m ()
Test.QuickCheck.Monadic.forAllM ::
  (Monad m, Show a) =>
  Gen a
  -> (a -> Test.QuickCheck.Monadic.PropertyM m b)
  -> Test.QuickCheck.Monadic.PropertyM m b
Test.QuickCheck.Monadic.monadic ::
  (Testable a, Monad m) =>
  (m Property -> Property)
  -> Test.QuickCheck.Monadic.PropertyM m a -> Property
Test.QuickCheck.Monadic.monadic' ::
  (Testable a, Monad m) =>
  Test.QuickCheck.Monadic.PropertyM m a -> Gen (m Property)
Test.QuickCheck.Monadic.monadicIO ::
  Testable a => Test.QuickCheck.Monadic.PropertyM IO a -> Property
Test.QuickCheck.Monadic.monadicST ::
  Testable a =>
  (forall s. Test.QuickCheck.Monadic.PropertyM (GHC.ST.ST s) a)
  -> Property
Test.QuickCheck.Monadic.monitor ::
  Monad m =>
  (Property -> Property) -> Test.QuickCheck.Monadic.PropertyM m ()
Test.QuickCheck.Monadic.pick ::
  (Monad m, Show a) => Gen a -> Test.QuickCheck.Monadic.PropertyM m a
Test.QuickCheck.Monadic.pre ::
  Monad m => Bool -> Test.QuickCheck.Monadic.PropertyM m ()
Test.QuickCheck.Monadic.run ::
  Monad m => m a -> Test.QuickCheck.Monadic.PropertyM m a
Test.QuickCheck.Monadic.runSTGen ::
  (forall s. Gen (GHC.ST.ST s a)) -> Gen a
Test.QuickCheck.Monadic.stop ::
  (Testable prop, Monad m) =>
  prop -> Test.QuickCheck.Monadic.PropertyM m a
Test.QuickCheck.Monadic.unPropertyM ::
  Test.QuickCheck.Monadic.PropertyM m a
  -> (a -> Gen (m Property)) -> Gen (m Property)
Test.QuickCheck.Monadic.wp ::
  Monad m =>
  m a
  -> (a -> Test.QuickCheck.Monadic.PropertyM m b)
  -> Test.QuickCheck.Monadic.PropertyM m b
```
