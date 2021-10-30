# Deriving instances for empty data types

6. Language extensions »   
6.6. Deriving mechanism »   
6.6.1. Deriving instances for empty data types

https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/empty_data_deriving.html

`EmptyDataDeriving`
- since 8.4.1
- Allow deriving instances of standard type classes for empty data types

One can write data types with no constructors using the `EmptyDataDecls` pragma, which is on by default in Haskell2010. However, Haskell2010 offers no way to derive class instances for these types, the ability which can be enabled in GHC witht the `EmptyDataDeriving` pragma.

```hs
data Empty
  deriving (Eq, Ord, Read, Show)

-- this generates these instances:

instance Eq   Empty where _ == _        = True
instance Ord  Empty where compare _ _   = EQ
instance Read Empty where readPrec      = pfail
instance Show Empty where showsPrec _ x = case x of {}
```

The EmptyDataDeriving is only required to enable deriving of these 4 "standard" classes (which are mentioned in the Haskell Report). Other extensions to the deriving mechanism don't require the `EmptyDataDeriving` flag in order to be used in conjunction with empty data types. These include:
- `StandaloneDeriving`
- `DeriveAnyClass`
- classes that need own pragma
  - `DeriveFunctor`
  - `DeriveFoldable`
  - `Derive`
