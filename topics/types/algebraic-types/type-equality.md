---
title        : Type equality
slug         : type-equality.md
path         : debrief.haskell/terms/
keywords     : GADTs, type equality, type equality constraint
---

# Type equality

Type equalities, like `Int ∼ a`, are a kind of type constraints enabled by the *GADTs* language pragma.

Type equalities form an equivalence relation, obeying the axioms of:
* reflexivity:  `a ∼ a`
* symmetry:     `a ∼ b <-> b ∼ a`
* transitivity: `a ∼ b ∧ b ∼ c -> a ∼ c`


```hs
{-# LANGUAGE GADTs #-}

five :: Int
five = 5

five' :: (a ∼ Int) => a
five' = 5
```




The canonical example of a GADT is a type safe syntax tree. Besides using GADTs, we can use type equalities instead and define it like this:

```hs
data Exp a
    = (a ~ Int)  => LInt Int
    | (a ~ Bool) => LBool Bool
    | (a ~ Int)  => LAdd (Exp Int) (Exp Int)
    | (a ~ Bool) => LNot (Exp Bool)
    | Cond (Exp Bool) (Exp a) (Exp a)
```

Viewed like this, it's easier to see what is happening behind the scenes: each data constructor of `Expr` carries with itself a type equality constraint. Like any constraint inside a data ctor, Haskell will require the constraint to be proven when the data ctor is called.

As such, when we pattern match on a data ctor which contains a constraint, this satisfied constraint comes back into scope. That is, a function of type 
`Expr a -> a` can return an `Int` when pattern matching on `LitInt`, but return a `Bool` when matching on `LitBool`.

The type equality constraining `a` *only comes back into scope after pattern matching on the data ctor that contains it*. This technique enables us to pack constraints inside data ctors.
