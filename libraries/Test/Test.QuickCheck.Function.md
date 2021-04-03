# Test.QuickCheck.Function

```hs
type role (Test.QuickCheck.Function.:->) nominal representational
type (Test.QuickCheck.Function.:->) :: * -> * -> *

data (Test.QuickCheck.Function.:->) a c where {- ... -}

Test.QuickCheck.Function.apply :: Fun a b -> a -> b

Test.QuickCheck.Function.functionEitherWith ::
  ((a -> c) -> a Test.QuickCheck.Function.:-> c)
  -> ((b -> c) -> b Test.QuickCheck.Function.:-> c)
  -> (Either a b -> c)
  -> Either a b Test.QuickCheck.Function.:-> c

Test.QuickCheck.Function.functionMapWith ::
  ((b -> c) -> b Test.QuickCheck.Function.:-> c)
  -> (a -> b)
  -> (b -> a)
  -> (a -> c)
  -> a Test.QuickCheck.Function.:-> c

Test.QuickCheck.Function.functionPairWith ::
  ((a -> b -> c) -> a Test.QuickCheck.Function.:-> (b -> c))
  -> ((b -> c) -> b Test.QuickCheck.Function.:-> c)
  -> ((a, b) -> c)
  -> (a, b) Test.QuickCheck.Function.:-> c

-- imported via Test.QuickCheck
pattern Fn :: (a -> b) -> Fun a b
pattern Fn2 :: (a -> b -> c) -> Fun (a, b) c
pattern Fn3 :: (a -> b -> c -> d) -> Fun (a, b, c) d

Fun ::
  (a Test.QuickCheck.Function.:-> b, b,
   Test.QuickCheck.Function.Shrunk)
  -> (a -> b) -> Fun a b

type role Fun nominal representational
type Fun :: * -> * -> *
data Fun a b = ...
type Function :: * -> Constraint
class Function a where {- ... -}

applyFun :: Fun a b -> a -> b
applyFun2 :: Fun (a, b) c -> a -> b -> c
applyFun3 :: Fun (a, b, c) d -> a -> b -> c -> d

function :: Function a => (a -> b) -> a Test.QuickCheck.Function.:-> b

functionBoundedEnum ::
  (Eq a, Bounded a, Enum a) =>
  (a -> b) -> a Test.QuickCheck.Function.:-> b

functionIntegral ::
  Integral a => (a -> b) -> a Test.QuickCheck.Function.:-> b

functionMap ::
  Function b =>
  (a -> b)
  -> (b -> a) -> (a -> c) -> a Test.QuickCheck.Function.:-> c

functionRealFrac ::
  RealFrac a => (a -> b) -> a Test.QuickCheck.Function.:-> b

functionShow ::
  (Show a, Read a) => (a -> c) -> a Test.QuickCheck.Function.:-> c

functionVoid ::
  (forall b. void -> b) -> void Test.QuickCheck.Function.:-> c
```
