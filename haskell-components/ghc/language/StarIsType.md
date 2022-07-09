# StarIsType

- `StarIsType`
- Since: 8.6.1

When enabled, treats the unqualified uses of the `*` type operator as nullary and desugar to `Type` from `Data.Kind.Type`.

The kind `Type`, imported from the `Data.Kind` module, classifies ordinary types. With StarIsType (currently enabled by default), `*` is desugared to `Type`, but using this legacy syntax is not recommended due to conflicts with `TypeOperators` (particularly, multiplication). This also applies to `★`, the Unicode variant of `*`.

[Note by me] To make GHCI print `Type` instead of `*` right from the repl's startup and without further persuasion, it seems the only reliable option is to put `NoStarIsType` in the project's cabal file, in the section "extensions". GHCi didn't react to this pragma (also to `DataKinds`) when they were placed in `.ghci.conf` file, or when a Haskell source file containing it was loaded.
