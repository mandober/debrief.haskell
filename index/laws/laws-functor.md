# Functor laws

1. fmap id          =  id                        [Functor-1-ID]
2. fmap g . fmap f  =  fmap (g . f)              [Functor-2-COMPOSE]


(the 2.law can be derived from the 1.law, they say?)


```hs
a          Fᵃ         F a
○ ──────────────────────→ ●
│                         │
│f                        │ Fᶠ
│                         │
↓                         ↓
○ ──────────────────────→ ●
b          Fᵇ         F b





                    F : C -> D
a, b ∈ Ob(C)                       F a, F b ∈ Ob(C)
f : a -> b ∈ Ar(C)                 F f : F a -> F b ∈ Ar(C)

F(a) = F a
F(b) = F b
F(f) = F f
F(f : a -> b) = F f : F a -> F b


fmap :: Functor f => (a -> b) -> f a -> f b


```
