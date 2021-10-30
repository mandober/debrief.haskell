# Closed type families

## Toplevel Closed Type Family

Closed type families are always top-level and they define type-level functions

```hs
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}

-- term-level function:
append :: forall a. [a] -> [a] -> [a]       -- type signature
append []     ys = ys                       -- equation 1
append (x:xs) ys = x : append xs ys         -- equation 2

-- type-level function:
type Append :: forall a. [a] -> [a] -> [a]  -- kind signature
type family Append xs ys where              -- header
  Append '[]    ys = ys                     -- clause 1
  Append (x:xs) ys = x : Append xs ys       -- clause 2
```

Heres a GHCi session to demonstrate how we can use it:

```hs
ghci> :kind! Append [1, 2, 3] [4, 5, 6]
Append [1, 2, 3] [4, 5, 6] :: [Nat]
= '[1, 2, 3, 4, 5, 6]
```

There are many similarities but also some differences:
- Instead of a type signature, there is a *standalone kind signature* 
  introduced with the `type` keyword, and `StandaloneKindSignatures` pragma
- Nil data ctor, `[]`, is promoted and thus written as `'[]`
  in order to distinguish it from its term-level counterpart
- Clauses of type family are grouped under the *type family header*

The *type family header* is probably the most notable difference here, and to understand its importance we must first discuss the notion of arity.

Just before we do that, here are a few more examples of closed type families to get accustomed to the syntax:

```hs
type Not :: Bool -> Bool
type family Not a where
  Not True  = False
  Not False = True

type FromMaybe :: a -> Maybe a -> a
type family FromMaybe d x where
  FromMaybe d Nothing  = d
  FromMaybe _ (Just x) = x

type Fst :: (a, b) -> a
type family Fst t where
  Fst '(x, _) = x
```
