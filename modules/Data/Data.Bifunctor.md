# Data.Bifunctor

```hs
type Bifunctor :: (* -> * -> *) -> Constraint
class Bifunctor p where
  bimap :: forall a b c d. (a -> b) -> (c -> d) -> p a c -> p b d
  
  first  :: forall a b c. (a -> b) -> p a c -> p b c
  second :: forall b c a. (b -> c) -> p a b -> p a c
{-# MINIMAL bimap | first, second #-}


bimap  :: Bifunctor p => (a -> b) -> (c -> d) -> p a c -> p b d
first  :: Bifunctor p => (a -> b) -> p a c -> p b c
second :: Bifunctor p => (b -> c) -> p a b -> p a c
```
