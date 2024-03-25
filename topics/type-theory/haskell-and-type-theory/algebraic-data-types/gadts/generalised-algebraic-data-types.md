# Generalised Algebraic Data Types

* One of the primary motivations for supporting GADTs is because they provide a way to build inductive type-level structures out of term-level data.


Algebraic Data Types (ADTs) are the usual Haskell sum and product types created with the scheme: `data` keyword + TypeName + type vars + `=`, on the LHS.

```hs
data TypeName a
  = DataCtor1 a
  | DataCtor2 a Int     -- ≅ DataCtor2 (a,Int)
  | DataCtor3 a Int Int -- ≅ DataCtor2 (a,Int,Int) ≅ DataCtor2 (a,(Int,Int))

-- the types of data ctors:
DataCtor1 :: Bool       -> TypeName a
DataCtor2 :: Int        -> TypeName a
DataCtor3 :: Int -> Int -> TypeName a
```

Most importantly, the return type of each data ctor is the type being defined; it is always the same, e.g. `TypeName a`.

```hs
data TypeName a where
  DataCtor1 :: a ->               TypeName Bool
  DataCtor2 :: a -> Int ->        TypeName Int
  DataCtor3 :: a -> Int -> Int -> TypeName a
```
