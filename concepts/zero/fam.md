# Functor, Applicative and Monad

Functors are structure-preserving mappings (arrows) between types (categories). Besides the specific set of laws that almost every Haskell class abides to (except `Show`, perhaps also `Read`), the Functor class has only one obligatory method, `fmap`. USing fmap we can apply a function to a structure, possibly changing the type of the structure but never the structure itself.

While Functors allows us to apply a function to some value(s) in a context (structure), Applicatives let us lift a function - put it into the context akin to the context of values.

Unlike Functors and Applicatives, Monads are the only ones that can express branching; the `>>=` operator can make choices based on some conditions.




## F-A-M hierarchy

```haskell
Functor (fmap)
  ↓
Applicative (pure, (<*>) | liftA2 :: (a -> b -> c) -> f a -> f b -> f c)
  ↓
Monad ((>>=))

import Data.Functor        ((<$>), (<$), ($>))
import Control.Applicative ((<*>), (*>), (<*))
import Control.Monad       ((>>=), (>>), (<<), (=<<))
```


## Quickly implement Functor and Applicative given a Monad instance

When you have already defined the Monad insance for your type, you still cannot use it until you also supply definitions for Monad's superclasses, Functor and Applicative.

```hs
import Control.Monad

-- if you already have monad instance
instance Monad Ty where
    return :: a -> Ty a
    return x = Done x
    (>>=) :: Ty a -> (a -> Ty b) -> Ty b
    mx >>= mf = mf $ runTy mx

-- Functor: fmap = liftM
instance Functor Ty where
    fmap = liftM

-- Applicative: <*> = ap
instance Applicative Ty where
    pure = return
    (<*>) = ap
```

Also, you can have the Functor class automatically derived by turning on the *DeriveFunctor* pragma.
