# Type inference

- algorithm 𝓦
  - de facto standard
  - bottom-up
  - context-insensitive
  - sound, complete
- algorithm 𝓜 ("folklore" algorithm)
  - top-down
  - context-sensitive
  - sound, complete

## Comparison

Algorithms for Hindley-Milner let-polymorphic type inference
- `Algorithm 𝓦`, de facto standard, bottom-up (context-insensitive)
- `Algorithm 𝓜`, aka "folklore" algorithm, top-down (context-sensitive)

The distinguishing property of the algorithm 𝓜 is that it stops earlier than algorithm 𝓦 if input program is ill-typed, and gives a more specific type-error message.

*Algorithm W*, which is the standard presentation of the Hindley-Milner let-polymorphic type inference system, fails late if the input program has a type error. Because the algorithm fails only at an *application* expression where its two subexpressions (function and argument) have conflicting types, an erroneous expression is often successfully type-checked long before its consequence collides at an application expression. This "bottom-up" Algorithm W thus reports the whole application expression as the problem area, implying some of its subexpressions are ill typed. Such a large type-error message does not help the programmer to find the cause of the type problem.


*Algorithm M* cures this problem. This "folklore" algorithm carries a type constraint (or an expected type) implied by the context of an expression down to its sub-or-sibling expressions. For example, for an application expression `e₁ e₂` with a type constraint, say of `int`, the type constraint for `e₁` is `α → int` and the constraint for `e₂` is the type that the `α` becomes after the type inference of `e₁`. For a constant or a variable expression, its type must satisfy the type constraint that the algorithm has carried to that point. Because of this top-down nature we name the algorithm M.

```ml hs
-- there is a typo in the last exp where `=` is used instead of `-`

-- M underlines                                 ∨∨∨
let rec fac n = if n = 0 then 1 else n * (fac (n = 1));;
-- W underlines ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Error report from algorithm W (in CamlLight 0.71):
This expression has type   int -> int
   but is used with type  bool -> int

-- Error report from algorithm M (in CamlLight 0.61):
      Expression of type  'a -> 'a -> bool
cannot be used with type  'a -> 'a -> int
```

W points at the whole definition (entire `if` exp) as the problem as it fails to unify the arg type `bool`, inferred from the recursive call `fac (n - 1)`, with the type `int`, inferred from the arg use `if n = 0 …`.

M, on the other hand, pinpoints the operator `=` as the problem. This exact error message is possible because the type constraint of the function's arg is `int` when the argument `n = 1` of the recursive call is type-checked.

In the paper (`Proofs about a folklore let-polymorphic type inference algorithm`, PictureOukseh Lee, PictureKwangkeun Yi, 1998) they formally define algorithm M, prove its soundness and completeness, and show that it finds type errors earlier than W. This property implies that algorithm M in combination with W can generate strictly more informative type-error messages than either of the two algorithms alone.

## Algorithm M

Algorithm M carries a *type constraint* from the context of an expression and stops when the expression cannot satisfy the current type constraint.
