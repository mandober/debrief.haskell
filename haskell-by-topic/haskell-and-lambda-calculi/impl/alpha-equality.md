# Alpha equality

The syntetic equality, `≡` compares two lambda exp for the exact equality string-wise, i.e. it compares if two exps-as-strings are equal.

Syntetic equality (`=`)

```
x    ≡ y      = true      if x ≡ y
M N  ≡ Mʹ Nʹ  = true      if M ≡ Mʹ ∧ N ≡ Nʹ
λx.B ≡ λy.Bʹ  = true      if x ≡ y  ∧ B ≡ Bʹ
```

α-equality (α-equivalence), `=α=`, compares two exp for equality modulo renaming:

```
x    =α= y     = true
M N  =α= Mʹ Nʹ = true      if M =α= Mʹ ∧ N =α= Nʹ
λx.B =α= λy.Bʹ = true      if x ≡ y    ∧ B =α= Bʹ
```

β-equivalence, `=ᵦ=` compares two exp for compatibility. The two exp are a redex and a contractum

redex ~> contractum <~ redexʹ

M N → (λx.B)N ← Mʹ Nʹ


β-equivalence compares two exp as "compatible" if one can be reduced to the other, or if the one is the redex of the other which is a contractum.

           (λf.λx.f x) K I
          /               \
         /                 \
(λf.f I) K                 (λx.K x) I
          \                /
           \              /
                  K I
