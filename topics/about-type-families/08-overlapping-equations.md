# Overlapping equations

> The clauses of a closed type family are ordered and matched from top to bottom.

This allows us to define logical conjunction as follows:

```hs
type And :: Bool -> Bool -> Bool
type family And a b where
  And True True = True
  And _    _    = False
```

If we were to reorder them, the `And _ _` equation would match all inputs; but since it comes last, the `And True True` clause is matched first.

> This is the key property of closed type families as opposed to open type families: the equations may be overlapping.

An open type family would need to enumerate all possibilities, leading to a combinatorial explosion

```hs
type And' :: Bool -> Bool -> Bool
type family And' a b

type instance And' True  True  = True
type instance And' True  False = False
type instance And' False True  = False
type instance And' False False = False
```
