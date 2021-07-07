# Type family

https://serokell.io/blog/type-families-haskell

## Closed type families

At the term level, when we need to perform a computation, we define functions. Here is, for example, list concatenation:

```hs
append :: forall a. [a] -> [a] -> [a]    -- type signature
append []     ys = ys                    -- equation 1
append (x:xs) ys = x : append xs ys      -- equation 2
```

At the type level, we define such computations using closed type families:

```hs
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}

type Append :: forall a. [a] -> [a] -> [a]  -- kind signature
type family Append xs ys where              -- header
  Append '[]    ys = ys                     -- clause 1
  Append (x:xs) ys = x : Append xs ys       -- clause 2
```

Heres a GHCi session to demonstrate how we can use both of these:

```hs
ghci> append [1, 2, 3] [4, 5, 6]
 [1,2,3,4,5,6]

ghci> :kind! Append [1, 2, 3] [4, 5, 6]
 Append [1, 2, 3] [4, 5, 6] :: [Nat]
 = '[1, 2, 3, 4, 5, 6]
```

There are many similarities but also some differences:
- the name of a type family is uppercased, so `Append`
- instead of a type signature, there is a *standalone kind signature*, introduced with the `type` keyword, after enabling `StandaloneKindSignatures`
- the nil data ctor `[]` is promoted and thus written as `'[]` to distinguish it from the term-level list type ctor, `[]`.
- clauses of a type family are grouped under the *type family header*
