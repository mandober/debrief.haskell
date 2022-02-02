# Canonical type representations

A direct corollary that any two types with the same cardinality are isomorphic, is that there are multiple ways to represent any given type. And any time multiple forms are available it becomes useful to determine one that is most representative.

> The **canonical representation** of ADT is a, possibly recursive, *sum of product* types.

* The canonical form of a sum type is a `Either a b`, tha tis the type ctor `Either` with two type vars `a` and `b`.
* The canonical form of a product type is a pair, `(a,b)`, or more formally, the type ctor `(,)` with two type vars `a` and `b`.
* Function types have the arrow familiar form, `a -> b` or more formally, the type ctor `(->)` with two type vars `a` and `b`.

Basically, asll sum types are repr by `Either` and all product types by a pair, with the arroew forms also accepted.

Each of following types is in its canonical representation:
- ()
- Either a b
- Either (a, b) (c, d)
- Either a (Either b (c, d))
- a -> b
- (a, b)
- (a, Int)
- `Either () a` is the canonical repr of `Maybe a`
- We make an exception to the rule for numeric types (like `Int` in the last case) as it would be too much work to express them as sums.
