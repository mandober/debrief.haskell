# Natural Numbers in Haskell

We use natural numbers to illustrate several ideas that apply to Haskell data types in general (ignoring the fact that each type in Haskell includes `⟘` value). Note that it is only possible to define the natural numbers up to an isomorphism.

Historically, the natural numbers were first consistently defined by the italian mathematician Giuseppe Peano in the XIX century. Originally, Peano came up with the 9 axioms for natural numbers in the second-order logic.

Consider this modern treatment of Peano axioms:
1. 0 ∈ N                                 (at least one element exists)
2. ∀n(n ∈ N -> S(n) ∈ N)                    (closure of S over N)
3. ∀n(n ∈ N -> S(n) ≠ 0)                       (exclude negative nums)
4. ∀n,m ∈ N. (n = m ⇔ S n = S m)                     (S is injective)

5. φ(0) ⋀ [φ(n) -> φ(S(n))] -> ∀n ∈ N. φ(n)   (induction as any predicate)
5. X ⊆ ℕ ⋀ 0 ∈ X ⋀ (n ∈ X -> S n ∈ X) -> X = ℕ   (induction in term of sets)


The nonnegative real numbers, the rational numbers (excluding negative integers) and some other sets all satisfy the axioms 1-4. That is why we need the axiom of induction as the 5th and final axiom.

For any predicate `P` in the first order predicate calculus,    
`P(0) ⋀ [P(n) -> P(S(n))] -> ∀n ∈ N. P(n)`

```js
φ(0) ⋀
  (φ(n) ->
    φ(S(n))) ->
      ∀n ∈ N. φ(n)
```

The induction axiom insures that `N` is the smallest set that satisfies the axioms.

Since we have no way of expressing more than a countable subset of the
predicates on ℕ, perhaps it is better to restate the final axiom in terms of sets.

`X ⊆ ℕ ⋀ 0 ∈ X ⋀ (n ∈ X -> S n ∈ X) -> X = ℕ`

```js
(X ⊆ ℕ ⋀
  (0 ∈ X ⋀
    (n ∈ X -> S n ∈ X))) ->
      X = N
```

## Natural Numbers defined using Category Theory
