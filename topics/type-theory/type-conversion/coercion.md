# Coercion

Coercion is the name of a particular type of conversion: exclusively those between a type `T` and its newtype `NT`. The types `T` and `NT` are representationally the same - they have the same run-time repr - so coercing between them is a zero-cost operation, and can be done anytime.

GHC automatically implements the `Coercible` class for any type `T` and its corresponding newtype `NT`.

```hs
-- Defined in GHC.Types
type role Coercible nominal representational representational
type Coercible :: forall k. k -> k -> Constraint
class Coercible k a b => Coercible @k a b
```

Coercible is not a class, but a **special constraint with custom solving rules**.

### The Coercible constraint
The constraint `Coercible t1 t2` is similar to `t1 ~ t2`, but denotes representational equality between `t1` and `t2` in the sense of Roles. It is exported by `Data.Coerce`, which also contains the documentation. More details and discussion can be found in the paper "Safe Coercions".

* The Coercible constraint - GHC manual
https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/equality_constraints.html

* Safe Coercions, 2018
https://www.microsoft.com/en-us/research/uploads/prod/2018/05/coercible-JFP.pdf

* Safe coercions between data types
https://ghc.gitlab.haskell.org/ghc/doc/libraries/base/Data-Coerce.html

* Roles - GHC manual
https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/roles.html

* More in-depth information can be found on the *Roles wiki page*
https://gitlab.haskell.org/ghc/ghc/-/wikis/roles


## Overloaded Lists
