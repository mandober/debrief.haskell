# Universal and existential quantification in Haskell

https://serokell.io/blog/universal-and-existential-quantification

In logic, there are two common quantifiers:
- the universal quantifier, ∀
- the existential quantifier, ∃

Haskell uses quantification to
- make universal quantification explicit with `ExplicitForAll`
- Create a heterogeneous list with existential data types
- existentially quantify type vars to move instantiation at the definition site

## Universal quantification

In Haskell, all type variables are implicitly universally quantified by default.

```hs
{-# LANGUAGE ExplicitForAll #-}

-- this is actually
id :: a -> a
id x = x

-- universally quantified
id :: forall a. (a -> a)
id x = x

-- and the type var `a` has kind Type
-- the most explicit expression:
id :: forall (a :: Type) . a -> a
id x = x
```

At the beginning of a function, data type, or instance declaration, before any constraints or arguments are listed, we use the `forall` quantifier to introduce all type variables that we'll refer to in the definition.

After the `forall` we can add constraints.

```hs
-- all type vars are universally quantifyied
func :: forall a b c m. Monad m => a -> b -> m c

-- universally quantifyied `a`
-- existentially quantified `b`
data Stream a = forall b. Stream { mkStream :: b -> (b -> a) -> (b -> b) }
```

Practical use cases of universal quantification
- Reordering type variables
- Visible type application
- ScopedTypeVariables

## Existential quantification

Haskell also supports existential quantification which is also done using the `forall` keyword. This is possible thanks to this logical equivalence:

`∀x.(Px -> Q)` ≡ `∀x.Px -> Q`  where `x ∉ FV(Q)`

`∀x.(Px -> Q)` ≡ `∃x.Px -> Q`  where `x ∉ FV(Q)`




```
∀x.(P -> Q)
∀x.(P(x) -> Q(x))   but
∀x.(P(x) -> Q)      if x ∉ FV(Q)
∀x.P(x) -> Q        simplified
```


These are equivalent in terms of first-order predicate logic.


The formula `∀x.(P -> Q)` is `∀x.(P(x) -> Q(x))`, but if the predicate `Q` doesn't have free variable `x`, i.e. `x ∉ FV(Q)`, then the formula becomes `∀x.(P(x) -> Q)`, which is simplifed to `∀x.P(x) -> Q`, since `x` only occurs free in `P`; so `x` is universally quantified only over the predicate `P`.

For a theoretical proof of this statement, see this thread:
https://stackoverflow.com/questions/10753073/whats-the-theoretical-basis-for-existential-types?rq=1


The Curry-Howard correspondence states that the types in a PLs correspond to formulas in Intuitionistic logic (IL). IL does not admit the Law of the Excluded Middle (LEM) and the Law of Double Negation Elimination (DNE):
- LEM, `P ∨ ¬P`   is not an axiom in IL
- DNE, `P ⟺ ¬¬P` is not an axiom in IL, but `P -> ¬¬P` is admitted

The DeMorgan's laws state:
- `∀x.P(x)` ≡ `¬∃x.¬P(x)`
- `∃x.P(x)` ≡ `¬∀x.¬P(x)`

Consider the formula 
`∀x(P(x) -> Q(x))`
but if `x` is not free in `P` then 
`∀x(P -> Q(x))` ≡ `P -> ∀x.Q(x)`

which can also be derived by using the correspondence of implication and disjunction, `P -> Q` ≡ `¬P ⋁ Q`

```
∀x(P -> Q(x))
∀x(¬P ∨ Q(x))
¬P ∨ ∀x.Q(x)
P -> ∀x.Q(x)
```

`∀x.(Q(x) -> P)` ≡ `(∃x.Q(x)) -> P` this one is what we need

```
  ∀x.( Qx  → P)          where x ∉ FV(P)
  ∀x.(¬Qx  ∨ P)          p → q ≡ ¬p ∨ q
  ∀x. ¬Qx  ∨ P           since x ∉ FV(P)
¬¬∀x. ¬Qx  ∨ P           ∀x = ¬¬∀x
¬(¬∀x.¬Qx) ∨ P           ¬∀x.¬Px = ∃x.Px
¬(∃x. Qx) ∨ P
  ∃x. Qx  → P
```

`∃x.Qx -> P`





 ∀x.¬P(x)


- `∃x.P(x)` ⟺ `¬∀x.¬P(x)`
  If P holds for at least one x, 
  then it is not the case that P does not hold for no x.
  But P may still turn out to hold for all x.

- `∀x.P(x)` --> `∃x.P(x)`
  if P holds for all x, then P holds for at least one x


¬( ∀x. Qx) =  ∃x. ¬Qx =  ∃x.¬Qx
¬(¬∀x. Qx) = ¬∃x. ¬Qx = ¬∃x.¬Qx
¬( ∀x.¬Qx) =  ∃x.¬¬Qx =  ∃x. Qx
¬(¬∀x.¬Qx) = ¬∃x.¬¬Qx = ¬∃x. Qx


¬( ∃x. Qx) =  ∀x. ¬Qx =  ∀x.¬Qx
¬(¬∃x. Qx) = ¬∀x. ¬Qx = ¬∀x.¬Qx
¬( ∃x.¬Qx) =  ∀x.¬¬Qx =  ∀x. Qx
¬(¬∃x.¬Qx) = ¬∀x.¬¬Qx = ¬∀x. Qx


   ∃x.¬Q(x)  = ¬(∀x.  Q(x))


¬(∀x.¬Q(x)) = ∃x.¬¬Q(x) = ∃x.Q(x)


 ¬¬∀x.¬Q(x)  = 
¬(¬∀x. Q(x)) = 
¬(¬∀x. Q(x)) = 
