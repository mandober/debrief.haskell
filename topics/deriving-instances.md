# Deriving class instances

https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/deriving.html

Haskell 98 allows the programmer to add a `deriving` clause to a data type declaration, to generate a standard instance declaration for specified class. GHC extends this mechanism:
- The derivation mechanism can be used separately from the data type declaration, using the *standalone deriving* mechanism.
- In Haskell 98, the only derivable classes are `Eq, Ord, Enum, Ix, Bounded, Read, Show`. Various language extensions extend this list.
- Besides the stock approach to deriving instances by generating all method definitions, GHC supports two additional deriving strategies, which can derive arbitrary classes:
  - *generalised newtype deriving* for newtypes
  - *deriving any class* using an empty instance declaration
- The user can optionally declare the desired *deriving strategy*, especially if the compiler chooses the wrong one by default.
