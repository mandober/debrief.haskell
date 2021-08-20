# StandaloneDeriving

- `StandaloneDeriving`
- https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/deriving.html#stand-alone-deriving

```hs
{-# LANGUAGE StandaloneDeriving #-}

-- datatype with auto-derived classed
data List a = Empty | Cons a (List a)
  deriving (Eq, Ord)

-- standalone Read class auto-derivation for List
deriving instance Read a => Read (List a)

-- standalone Show auto-derivation
deriving instance Show (NewType a)

-- data type declaration
data Foo a = Bar a | Baz String
-- standalone derivation
deriving instance Eq a => Eq (Foo a)
```


The syntax is identical to that of an ordinary instance declaration apart from
- `deriving instance`
- presence of the `deriving` keyword
- absence of the `where` clause

Standalone deriving differs from a deriving clause in important ways:
- can be in *different module* wrt the data type declaration
- in most cases, you must supply an *explicit context*, exactly as you would in an ordinary instance declaration (the context is inferred in a deriving clause that is attached to a data type declaration)
