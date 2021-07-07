# Levity Polymorphism

* **Levity polymorphism** is an abstraction over "liftedness" of types that allows defining functions which work for both lifted and unlifted types.

* *Unlifted types* are machine integers and floats; suffixed with a hash symbol: `Int#`, `Word#` (called the magic hash); their kind is `#`.

* *Lifted types* are types extended with an additional, *bottom*, value. Bottom value (also `⊥`) represents undefined, infinite loops, errors. The kind of lifted types is `Type` or `*` (read "star" or "type"; deprecated).

* The new kind `TYPE` is now the most general kind that includes other kinds.

```hs
kinds = # | * | TYPE

TYPE = {#, *}

LangItems = UnliftedValue | LiftedValue | Type | Kind | Constraint | Class
```

value level:
- unlifted values
- lifted values
type level:
- types
- kinds
- constraints
- type classes
- type equalities



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
