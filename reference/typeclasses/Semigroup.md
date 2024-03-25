# Semigroup

The `Semigroup` Haskell class represents *Semigroup* algebraic structures from abstract algebra.



a *Magma* is an algebra (algebraic structure) with a single binary


## Semigroup in Haskell

### Semigroup info

- class definition in: `Prelude`
- other definitions in: Data.Semigroup
  - module: `Data.Semigroup`
    - submodule(s): `Data.Semigroup.Internal`
- package: all definitions in `base-4.19` (on 2024-01-26) package

The `Semigroup` class itself is defined in the `Prelude` module, which is a part of the `Base` standard package. Other functions pertaining to `Semigroup` are defined in the `Data.Semigroup` module.

Links:
- base-4.19.0.0, Prelude, Semigroup
https://hackage.haskell.org/package/base-4.19.0.0/docs/Prelude.html#t:Semigroup
- base-4.19.0.0, Prelude, Semigroup (source code)
https://hackage.haskell.org/package/base-4.19.0.0/docs/src/GHC.Base.html#Semigroup
- `Data.Semigroup` module, `Internal` submodule
https://hackage.haskell.org/package/base-4.19.0.0/docs/src/Data.Semigroup.Internal.html


## Semigroup as algebraic structure

*Semigroup* is an algebraic structure consisting of:
- 1 carrier set of elements
- 1 binary operation
- 2 axioms:
  - Closure
  - Associativity

Both axioms (more) pertain to the properties of the binary operation (than to the carrier set, although the two always work in concord): the operation must be a binary associative operation closed over the carrier set.


For example, `(ℕ, ÷)` is not a semigroup since division is not closed over `ℕ` (nor is division associative in the first place).

Semigroups that lack associtivity (i.e. without the associtive operation) are called Magmas, but `Magma` makes no interesting type class. Magma is the simples group-like algebraic structure - all it respects is the closure axiom.

Semigroups with identity element are called Monoids, and `Monoid` does make for an interesting type class.

Higher group-like algebras, although certainly interesting, are not represented as Haskell type classes.

Index of group-like algebraic structures
- Magma:                     closure
- Semigroup:     Magma     + associativity
- Monoid:        Semigroup + identity
- Group:         Monoid    + inverse
- Abelian group: Group     + commutativity

There are other, less frequently occurring, group-like algebras with the combination of axioms missing in the list above.






## Semigroup as Haskell class

```hs
class Semigroup a where
  -- 
  (<>) :: a -> a -> a
  sconcat :: NonEmpty a -> a
  stimes  :: forall b. Integral b => b -> a -> a
```


### Semigroup laws

Semigroup instances should satisfy the following law:

[Associativity] 

x <> (y <> z) = (x <> y) <> z

You can alternatively define `sconcat` instead of (`<>`), in which case the laws are:

- [Unit]: @'sconcat' ('pure' x) = x@

- [Multiplication]: @'sconcat' ('join' xss) = 'sconcat' ('fmap' 'sconcat' xss)@
class Semigroup a where

An associative operation, (<>) :: a -> a -> a

>>> [1,2,3] <> [4,5,6] -- [1,2,3,4,5,6]


sconcat :: NonEmpty a -> a

stimes  :: forall b. Integral b => b -> a -> a
