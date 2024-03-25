# Predicates in Haskell

A value-level predicate in Haskell is generally a function of type `a -> Bool`. Given a value of type `a`, if `True` is returned, then the predicate holds.

A type-level predicate is generally a type ctor of kind `k -> Type`. Given a type ctor of kind `k`, if the type is inhabited (if a value of that type exists or can be constructed), then the predicate is satisfied. That value, if it exists, is called a *witness* or a *proof*. Obviously, there can be more than one proof, and proof equality is an issue still under active investigation.

Only types classified by the `Type` kind are inhabited.

* decidable: Combinators for manipulating dependently-typed predicates.
https://hackage.haskell.org/package/decidable

* type-combinators: A collection of data types for type-level programming
https://hackage.haskell.org/package/type-combinators


We say that a predicate is **decidable** if, for any input type, we can say whether or not the predicate is satisfiable. In Haskell, we say that a predicate `P` is decidable if we can always prove, for any input, if the predicate holds or does not hold. Concretely, it means that we can write a total function:

```hs
-- | given a type via the singleton, return a decision.
decidePred :: Sing x -> Decision (P x)

data Decision a
  = Proved a               -- value of 'a' exists
  | Disproved (Refuted a)  -- value of 'a' cannot exist

-- | Empty data type
type Falsum = Void

-- | 'a' cannot exist. 'NOT' in IL: ¬p ≡ p -> ⟘
type Refuted a = a -> Falsum
```
