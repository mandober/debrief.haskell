# Non-linear patterns

In term-level functions, a variable can't be bound more than once:

```hs
dedup (x : x : xs) = dedup (x : xs)   -- Rejected!
dedup (y : xs) = y : dedup xs
dedup [] = []
```

If we want to check that two inputs are equal, we must do so explicitly with the (==) operator:

```hs
dedup (x1 : x2 : xs) | x1 == x2 = dedup (x1 : xs)
dedup (y : xs) = y : dedup xs
dedup [] = []
```

On the other hand, in type family instances the former style is also allowed:

```hs
type family Dedup xs where
  Dedup (x : x : xs) = Dedup (x : xs)
  Dedup (y : xs) = y : Dedup xs
  Dedup '[] = '[]
```

The feature is called **non-linear patterns** (but do not confuse it with linear types, which are not related).
