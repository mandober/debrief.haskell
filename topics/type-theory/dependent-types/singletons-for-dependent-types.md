# Singletons

We use the standard method of singletons to simulate dependent types in Haskell.

A singleton type `Sing a` of a type-level value `a` is the unique type that has the same structure as `a`, on which we can patternmatch to retrieve its exact shape. `Sing a` can be identified with a type `a` but demoted to the term level.

In particular, we assume the following API:

```hs
type family Sing :: k -> Type

class Known a where -- SingI in singletons
  sing :: Sing a

withKnown :: Sing a -> (Known a => r) -> r

data SomeSing k where -- SingKind in singletons
  MkSomeSing :: Sing (a :: k) -> SomeSing k

class HasSing k where
  type Demoted k
  demote :: Sing (a :: k) -> Demoted k
  promote :: Demoted k -> SomeSing k

withPromoted :: HasSing k
             => Demoted k
             -> (forall x. Sing (x :: k) -> r) -> r

type FromJust :: ErrorMessage -> Maybe a -> a

type family FromJust err may where
  FromJust err 'Nothing = TypeError err
  FromJust _ ('Just a) = a

type instance Sing = (SNat :: Nat -> Type)

sNat :: KnownNat n => SNat n

withKnownNat :: SNat n -> (KnownNat n => r) -> r
(%+) :: SNat n -> SNat m -> SNat (n + m)
sMod :: SNat n -> SNat m -> SNat (n `Mod` m)
```
