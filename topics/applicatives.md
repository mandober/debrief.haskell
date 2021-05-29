# Applicatives

- Applicatives are applicative functors (AF), grouped by `Applicative` class

- AFs generelize the structural mapping: while fmap can only map a unary function over a structure, but AFs extend this to binary functions.

- `pure` just lifts a value into applicative context, while `<*>` is for sequential application.

```hs
-- fmap lifts a unary function (maps a structure with a unary fn)
fmap succ [3,5,8] -- [4,6,9]

-- ⊛ lifts a binary function
pure (+) <*> (Just 3) <*> (Just 5) -- Just 8
pure (+) <*> [3,5,8]  <*> [1,4,7]  -- [4,7,10,6,9,12,9,12,15]
```



## Using `ApplicativeDo`

`fs <*> as` can be understood as the do-expression:

```hs
fs <*> as = do
  f <- fs
  a <- as
  pure (f a)
```
