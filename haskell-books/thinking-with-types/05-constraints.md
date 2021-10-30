# Constraints

The `Constraint` **kind** is reserved for things that appear in the context:
- fully-saturated typeclasses, e.g. `Show a`
- tuple of Constraints, e.g. `(Read a, Int ∼ b)`
- type equalities, `Int ∼ a` (need `GADTs` pragma)

Type equalities are enabled along with `GADTs`.

```hs
{-# LANGUAGE GADTs #-}

five' :: Int
five' = 5

five :: (a ~ Int) => a
five = 5
```

The `five` has type `a` along with a constraint saying that `a` is equivalent to an `Int`.

Type equalities form an equivalence relation, meaning they respect the axioms of reflexivity, symmetry and transitivity.
* reflexivity  : `a ∼ a` (a type is always equal to itself)
* symmetry     : `a ~ b` <=> `b ~ a`
* transitivity : `a ~ b` ∧ `b ~ c` -> `a ~ c`
