# The Arrow class

A computation takes inputs of some type and produces outputs of another type, so Hughes defined the class `Arrow` for binary type constructors.

```hs
class Arrow a where
  -- Each function may be treated as a computation (effectful!)
  arr :: (b -> c) -> a b c

  -- Computations are composable, by connecting
  -- the output of the first to the input of the second.
  (>>>) :: a b c -> a c d -> a b d

  -- A computation may be applied to just a part of the input,
  -- with the rest of the input passed through to the output.
  first :: a b c -> a (b,d) (c,d)
```

Again, using different names

```hs
class Arrow z where
  arr   :: (b -> c) -> z b c
  arr   :: (a -> b) -> z a b
  arr   :: (a -> b) -> z ta tb

  (>>>) :: z b c -> z c d -> z b d

  first :: z b c -> z (b, d) (c, d)
```
