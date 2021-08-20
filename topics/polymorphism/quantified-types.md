# Quantified types

In a data or newtype declaration one can quantify the types of the constructor arguments.

Consider that (1) is an example of an existential type that defines a type wrapper for things that we can convert to a String. The wiki mentions that what we really want to define is a type like (2).

```hs
data S = forall a. (Show a) => S a      -- (1)

data S = S (exists a. (Show a) => a)    -- (2)


data S where                          -- (3)
  S :: Show a => a -> S
```

However, it's completely not obvious to me why (1) is equivalent to (2). Why does moving the data constructor to the outside turn the forall into an exists?



## Connection to logic

These logic formulas are equvalent:

`(∃x. Px) -> Q`   ≡  `∀x. (Px -> Q)`    
`(∃x. p x) -> q`  ≡  `∀x. (p x -> q)`

(note that `q` does not depend on `x`).

This can be used to express existential quantification in terms of universal quantification, provided the existentially quantified variable is on the LHS of implication arrow, i.e. LHS of function arrow in Haskell:

```hs
data S = forall a. Show a => S a      -- universal quantification

data S = S (exists a. Show a => a)    -- existential quantification

data S = S (forall a. Show a => a)    -- existential quantification
```




  ∀x. Px
¬(∀x. Px)
  ∀x.¬Px
 ¬∀x. Px
 ¬∀x.¬Px
------------
  ∃x. Px
¬(∃x. Px)
  ∃x.¬Px
 ¬∃x. Px
 ¬∃x.¬Px
------------
¬(∀x. Px)  ≡
¬∀x. ¬Px   ≡
 ∃x. ¬Px



The Curry-Howard correspondence states that types in a PL correspond to formulas of an intuitionistic logic.

∀x.Px = ¬∃x.¬Px
∃x.Px = ¬∀x.¬Px

We can derive that `∀x. P -> Qx` is equivalent to `P -> (∀x. Qx)`

∀x. P -> Qx
∀x. ¬P ∨ Qx
¬P ∨ ∀x. Qx
P -> ∀x. Qx

and `∀x. Qx -> P` ≡ `(∃x. Qx) -> P` (this one is used below):

(∀x. Qx -> P)
(∀x. ¬Qx ∨ P)
(¬¬∀x. ¬Qx) ∨ P
(¬∃x. Qx) ∨ P
(∃x. Qx) -> P

Note that these laws hold in intuitionistic logic as well.

The two laws we derived are cited in the paper below.
