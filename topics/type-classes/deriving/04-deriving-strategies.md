# Deriving strategies

https://kowainik.github.io/posts/deriving#strategies

In addition to regular deriving clauses, you can opt-in to use a specific deriving strategy with `DerivingStrategies` pragma.

**Deriving strategies** grant users a fine-grained control over how the instances are derived, and thus how the code is generated.

Noramlly, when you ask the compiler to derive an instances for a class, it needs to figure out an exact plan of how to generate the appropriate code, especially because sometimes the same class may be derived in multiple ways.

A deriving strategy is a (implicit or explicit) way to tell the compiler what sort of deriving you want for a particular datatype and class pair.

When GHC encounters a deriving clause, it associates a particulr strategy for each class named in the deriving clause. This process is called **strategy resolution**.

If you don't specify the strategy explicitly, GHC uses an algorithm to find out a proper strategy. Otherwise, it jsut checks whether the explicitly specified strategy is applicable in that situation.

Currently (GHC 9.0), there are 4 kinds of deriving strategies:
- stock
- newtype
- anyclass
- via


Strategy `stock`:
- DeriveFunctor
- DeriveFoldable
- DeriveTraversable
- Generic,  (DeriveGeneric), `import GHC.Generics (Generic)`
- Generic1, (DeriveGeneric), `import GHC.Generics (Generic1)`
- Data (DeriveDataTypeable), `import Data.Data (Data)`
- Lift (DeriveLift),         `import Language.Haskell.TH.Syntax (Lift)`

## Newtypes

- `GeneralizedNewtypeDeriving`
- strategy: `newtype`
- classes: any class implemented by the wrapped type

When you don't specify a strategy explicitly for newtypes, GHC uses the *stock* strategy for standard typeclasses, and the *newtype* strategy for other typeclasses, e.g. `deriving (Eq, Ord, Show, Num)`.

The standar classes can also be derived using the *newtype strategy*. For example, `deriving Show` defaults to *stock* if the strategy is unspecified, which produces the output like `"Sum 42"`. Specifing the newtype strategy, e.g. `deriving newtype Show` produces the output `"42"` (without the ctor).
