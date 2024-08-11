# Using forall to mean exists

Haskell has both universal quantification, expressed with the `forall` keyword that acts as the `∀` quantifier, and existential quantification, also expressed with the `forall` keyword, which, depending on location, can act as the `∃` quantifier.

This usage implies some kind of equivalence, at least in some circumstances, between the ∀ and ∃ quantifed formulas - what exactly are these circumstances?

## Take 1

In logic (classical or intuitionistic), these two formulas, expressed in pseudo Haskell, are *equivalent*

`(exists x. p x) -> q` ≡ `forall x. (p x -> q)`

Note that `q` does not depend on `x`.

This equivalence can be used to express existential quantification in terms of the universal quantification, provided the existentially quantified variable occurs on the left side of implication.

Here is a classical proof:
https://stackoverflow.com/a/10753957/3234959


So, instead of writing the following in pseudo Haskell:

```hs
rapply :: (exists a. (a, a -> Int)) -> Int
rapply (x, h) = h x

-- How would we express this in logic?
-- f : ∀y(∃x(x ∧ x -> y)) |- y
-- this is actually the same as
-- f : ∀y∀x(x ∧ x -> y) |- y
-- i.e. Modus Ponens
```

we can write this in Haskell:

```hs
rapply :: (a, a -> Int) -> Int
-- forall is auto-added anyway:
rapply :: forall a. (a, a -> Int) -> Int
rapply (x, h) = h x
```

We can make do without the existential quantifier, at least in cases such as the above.

Existential quantification is still needed when it occurs not on the left of an arrow. For instance,

```hs
g :: exists a. (a, a->Int)
g = (2 :: Int, \x -> x + 3)
```

Alas, Haskell chose not to include these types. Likely, this choice was made to keep the already sophisticated type system from becoming too complex.

Still, Haskell got existential data types, which just require to wrap/unwrap one constructor around the existential. For example, using GADT syntax, we can write:

```hs
data Ex where
  E :: forall a. (a, a->Int) -> Ex
g :: Ex
g = E (2 :: Int, \x -> x+3)
```

Finally, let me add that existentials can also be simulated by rank-2 types and continuation-passing:

```hs
g :: forall r. (forall a. (a, a->Int) -> r) -> r
g k = k (2 :: Int, \x -> x+3)
```
