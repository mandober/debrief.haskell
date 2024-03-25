# Index of monad transformers

Monad transformer
  - name: `ListT`
  - purpose: non-determinism
  - precursor type:   `newtype List    a = List     (Maybe (a, List    a))`
  - transformer type: `newtype ListT m a = ListT (m (Maybe (a, ListT m a)))`


The list precursor type says that a list is maybe a pair of list's head and tail.

- A regular list is a sum type, `Lᵃ  = 1 + a ⨯ Lᵃ`.
- Maybe type,                   `Mᵃ  = 1 + a`.
- Product type,                 `Pᵃᵇ = a ⨯ b`.
- Coproduct type,               `Eᵃᵇ = a + b`.


```hs
-- regular list is a sum type
data List a = Nil | Cons a (List a)

-- list precursor
newtype List a = List (Maybe (a, List a))
```

La = L (M (a * La))
   = L (1 + (a * La))
   = L (1 + (a * (1 + a * La)))
   = L (1 + (a + a² * La)))
   = 1 + [1 + (a + a² * La)] * L[1 + (a + a² * La)]

algebra of algebraic data types
