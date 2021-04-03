# DerivingStrategies

- Pragma: `DerivingStrategies`
- Since: 8.2.1
- https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/deriving_strategies.html#extension-DerivingStrategies

Allow multiple deriving, each optionally qualified with a strategy.

In most scenarios, every deriving statement generates a typeclass instance in an unambiguous fashion. There is a corner case, however, where simultaneously enabling both the `GeneralizedNewtypeDeriving` and `DeriveAnyClass` extensions can make deriving become ambiguous.

Deriving strategies:
- `anyclass`: Use DeriveAnyClass
- `newtype` : Use GeneralizedNewtypeDeriving
- `via`     : Use DerivingVia
- `stock`   : Have GHC implement a "standard" instance for a data type,
              if possible (e.g., Eq, Ord, Generic, Data, Functor, etc.)


```hs
newtype Baz = Baz Quux
  deriving          (Eq, Ord)
  deriving stock    (Read, Show)
  deriving newtype  (Num, Floating)
  deriving anyclass C
```


- https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/derive_any_class.html#derive-any-class
- https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/newtype_deriving.html#newtype-deriving
- https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/deriving_via.html#deriving-via
