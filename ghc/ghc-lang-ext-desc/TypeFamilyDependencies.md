# TypeFamilyDependencies

Lang Pragma: `TypeFamilyDependencies`, GHC 8.0

Historically, type families have been non-injective - they are not guaranteed to map a distinct element of its argument to the same element of its result.

The syntax the extension brings is similar to functional dependencies, where the resulting type is uniquely determined by the type parameters of a type family.

```hs
:set -XPolyKinds
:set -XTypeFamilyDependencies

type family TyFam a b c = (result :: k) | result -> a b c

type instance TyFam Int  Char Bool = Bool
type instance TyFam Char Bool Int  = Int
type instance TyFam Bool Int  Char = Char

-- then why is this allowed?!
type instance TyFam Float Char Bool = ()
```
