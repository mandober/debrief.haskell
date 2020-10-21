# Levity Polymorphism

In some cases, it may be useful to abstract over lifted and unlifted types. 

Polymorphism over "liftedness" of types - allows defining functions which work for both lifted and unlifted types.

Unlifted types are machine integers and floats
- these types are hash-suffixed: `Int#`, `Word#`
- their kind is `#`
Lifted (boxed) types
- the kind of lifted types that was previously `*` is now renamed to `Type`.
- the new kind `TYPE` is the most general kind that includes other kinds.

TYPE = {#, Type/*}

`# ⊂ */Type ⊂ TYPE`
`values ⊂ types ⊂ kinds ⊂ sorts`

`sort > kind > type > lifted-values > unlifted-values `
`sort > kind > TYPE > Type          > #`


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


-- ---------------------------------------------
-- Levity-polymorphic Num typeclass

{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeInType #-}

module LevNum where

import GHC.Prim
import GHC.Exts (TYPE)

class LevNum (a :: TYPE r) where
  levAdd :: a -> a -> a

instance LevNum Int  where levAdd = (+)
instance LevNum Int# where levAdd = (+#)
```
