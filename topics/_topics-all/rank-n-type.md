# Rank-N type

https://wiki.haskell.org/Rank-N_types

Arbitrary Rank Polymorphism (GHC User Manual)
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#arbitrary-rank-polymorphism


- `RankNTypes`


Normal Haskell'98 types are considered *Rank-1 types*. A Haskell '98 type signature such as `a -> b -> a` implies universal quantification of type vars, so it is really `∀a b. a -> b -> a`. The forall quantifier can be floated out of the RHS of (->) such that `∀a. a -> (∀b. b -> a)` is also a Rank-1 type, being equivalent to the previous signature.

`∀a b. a -> b -> a` ≅ `∀a. a -> (∀b. b -> a)`

However, the forall quantifier on the LHS of (->) cannot be moved up, and therefore forms another (nested) level or *rank*.

* Rank-N where `N` is the number of foralls which are nested and cannot be floated (and possibly merged with the previously declared forall binder).

For example, `(∀a. a -> a) -> (∀b. b -> b)` is a Rank-2 type because the second ∀ can be floated but the first one cannot. Thus, there are two levels of universal quantification.

Rank-N type reconstruction (type inference) is undecidable in general, and some explicit type annotations are required.
