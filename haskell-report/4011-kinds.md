# 4.1.1 Kinds

To ensure they are valid, *type expressions* are classified into different kinds, which take one of two possible forms:
- the `*` represents the kind of all saturated type ctors
- if `κ₁` and `κ₂` are kinds, then `κ₁ → κ₂` is the kind of types that take a type of kind `κ₁` and return a type of kind `κ₂`.


```
C :: κ₁ → κ₂    t :: κ₁
----------------------- [kind-app]
      C t :: κ₂
```

Kind inference checks the validity of type expressions in a similar way that type inference checks the validity of value expressions.
