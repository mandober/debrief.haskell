# Levity Polymorphism

Polymorphism over "liftedness" of types. Allows defining functions which work for lifted and unlifted types. Unlifted types are machine integers and floats; these types have `#` suffix and their kind is `#`. And the kind of lifted (boxed) types that was `*` previously is renamed to `Type`. The new kind `TYPE` is the most general kind including other kinds.

```hs
TYPE :: Rep -> Type

data Rep = LiftedRep  -- pointer to heap obj
         | IntRep     -- no ptr, machine int
         | DoubleRep
         | ...

type Type = TYPE LiftedRep

Int  :: Type
Int  :: TYPE LiftedRep
Int# :: TYPE IntRep
Dbl# :: TYPE DblRep
Maybe:: Type -> Type

(+) :: ∀(r :: Rep). ∀(a :: TYPE r). (Num a) => a -> a -> a
-- so now (+) works for both boxed and unboxed sorts
3 + 4
3# + 4#

```

sort > kind > type > values
