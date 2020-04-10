# Exporting

The module statement can be used not only for explicitly naming the module that groups a set of related definitions, but also to limit the visibility of these definitions outside the module.

```hs
module Frac (Fraction(..), mk, add, mul, lt) where

data Fraction
  = F Int Int
  | Groo Float Int
  | HReal Float Double
  | Complex Real Imag
  | Fairy Dust Angel
  deriving (Eq, Ord, Show)
```

The module definition at the beginning of the script simultaneously specifies the name of the module (Frac) and the list of definitions that should be exported from the module.

In case no list of exported definitions is given, Haskell assumes that all definitions are exported.

The syntax `T(..)` means that **the type and all its data ctors are exported**.

This makes it possible to construct and deconstruct values of type Fraction also outside of module Frac:

```hs
Frac.F 1 2 == Frac.mk 1 2  -- True
```

In other words, the full representation of the type (fraction) is exposed. This may be undesirable in general for at least two reasons:
* users of the module may depend on a particular representation of fractions, hence future changes to the Frac module may break existing code
* users may maliciously create ill-formed data or access to information they are not supposed to see; e.g. it is possible to create a fraction with a null denominator, if one does not use the provided `mk` function:

```hs
Frac.mk 1 0  -- *** Exception: Null denominator
Frac.F 1 0   -- F 1 0
```

> By omitting the `(..)`, a type is exported, but its representation is not.

In other words, no constructor of Fraction can be directly accessed from outside the module Frac: *this prevents users from creating values of type Fraction if not by means of the provided functions, and doing pattern matching of values of type Fraction*. In this way, Fraction becomes an **abstract data type**.

Haskell also suppors a **partial form of abstraction** if you specify, instead of `(..)`, the list of some data ctors (but not all) that should be exported (generally, you'd specify a subset of the data ctors).


```hs
module Frac (
  Fraction(Groo, Fairy)
) where

data Fraction
  = F Int Int
  | Groo Float Int
  | HReal Float Double
  | Complex Real Imag
  | Fairy Dust Angel
  deriving (Eq, Ord, Show)
```



---

limiting visibility of module entities
data ctors hiding
abstract data type
partial abstract data type
