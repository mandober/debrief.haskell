# Type-Level Literals

https://gitlab.haskell.org/ghc/ghc/-/wikis/type-nats/basics

GHC 9.x supports two forms of type-level literals: natural numbers and symbols.
- Natural number literals are a family of types, all of kind `Nat`
- Symbol literals are types of kind `Symbol`

The type classified by `Nat` and `Symbol` kinds are uninhabited (only the types of the kind `Type` have values), but they may be used to index other type ctors.
