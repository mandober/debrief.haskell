# A Guide to GHC's Extensions

https://limperg.de/ghc-extensions/

1. Language Standards
2. Enabling GHC Extensions
3. Basic Track
  * Syntactic Niceties
    - TupleSections
    - LambdaCase
    - MultiWayIf
    - BlockArguments
    - `TypeOperators`
    - `NumericUnderscores`
  * Overloaded Literals
    - OverloadedStrings
    - OverloadedLists
  * Patterns
    - ViewPatterns
    - PatternSynonyms
  * Type System
    - ExplicitForAll
    - TypeApplications
    - ScopedTypeVariables
    - RankNTypes
    - LiberalTypeSynonyms
  * Records
    - NamedFieldPuns
    - RecordWildCards
  * Classes
    - `FlexibleInstances`
    - `FlexibleContexts`
    - DeriveFunctor
    - DeriveFoldable
    - DeriveTraversable
    - GeneralizedNewtypeDeriving
    - InstanceSigs
    - ConstrainedClassMethods
    - `MultiParamTypeClasses`
    - FunctionalDependencies
    - DeriveGeneric
  * Typed Holes
    - Typed Holes and Type Wildcards
    - NamedWildCards
    - PartialTypeSignatures
4. Advanced Track
  * Type System
    - ExistentialQuantification
    - GADTSyntax
    - GADTs
    - TypeFamilies
    - TypeFamilyDependencies
    - AllowAmbiguousTypes
  * Kinds
    - KindSignatures
    - ConstraintKinds
    - DataKinds
    - PolyKinds
  * Empty Types
    - EmptyCase
    - EmptyDataDeriving
  * Classes
    - StandaloneDeriving
    - DefaultSignatures
    - DeriveAnyClass
    - DerivingStrategies
    - DerivingVia
    - QuantifiedConstraints
    - UndecidableInstances
    - UndecidableSuperClasses
    - RoleAnnotations
  * Records
    - DisambiguateRecordFields
    - DuplicateRecordFields
    - OverloadedLabels
  * Strictness
    - BangPatterns
    - Strict
    - StrictData
  * Do Notation
    - ApplicativeDo
    - RecursiveDo
  * Literals
    - NegativeLiterals
    - NumDecimals
    - BinaryLiterals
    - HexFloatLiterals
  * Template Haskell
    - TemplateHaskell, TemplateHaskellQuotes
    - QuasiQuotes
    - DeriveLift
  * Low-Level Hacking
    - MagicHash
    - UnboxedTuples
    - UnboxedSums
  * Safe Haskell
    - Safe, Trustworthy, Unsafe
  * Miscellaneous
    - CPP
    - NoImplicitPrelude
    - RebindableSyntax
    - UnicodeSyntax
    - NoMonomorphismRestriction
    - PostfixOperators
    - PackageImports
5. Questionable Track
  5.1 Comprehensions
  5.1.1 ParallelListComp
  5.1.2 TransformListComp
  5.1.3 MonadComprehensions
  5.2 Disabling Standard Features
  5.2.1 NoTraditionalRecordSyntax
  5.2.2 NoPatternGuards
  5.3 Miscellaneous
  5.3.1 Arrows
  5.3.2 StaticPointers
  5.3.3 ImplicitParams
  5.3.4 ExtendedDefaultRules
  5.3.5 DeriveDataTypeable
  5.3.6 NPlusKPatterns
  5.3.7 ImpredicativeTypes
  5.4 Deprecated Extensions
  5.4.1 DatatypeContexts
  5.4.2 NullaryTypeClasses
  5.4.3 OverlappingInstances, IncoherentInstances
  5.4.4 TypeInType
  5.4.5 Rank2Types
6. Miscellaneous Track
  * Dependent Extensions
    - ExplicitNamespaces
    - MonoLocalBinds
  * Temporary Extensions
    - MonadFailDesugaring
    - StarIsType



## Syntactic Niceties
<!-- #region Syntactic Niceties -->

### TupleSections
```hs
{-# LANGUAGE TupleSections #-}
> :set -XTupleSections
```
Write `(x,,)` instead of `\y z -> (x, y, z)`


### LambdaCase

```hs
-- before
f = \x -> case x of
  1 -> True
  _ -> False


{-# LANGUAGE LambdaCase #-}

-- after
f x = \case
  1 -> True
  _ -> False
```

### MultiWayIf
```hs
{-# LANGUAGE MultiWayIf #-}
> :set -XMultiWayIf
```

```hs
-- Instead of
if n == m
  then x
  else if n == k
    then y
    else z

-- write
if | n == m    -> x
   | n == k    -> y
   | otherwise -> z
```

### BlockArguments
```hs
{-# LANGUAGE BlockArguments #-}
> :set -XBlockArguments
```

If you want to apply a function to a do block, you usually need to use the $ operator:

```hs
main = f $ do
  ...
```

This extension allows you to omit the $ and directly give the do block as an argument.

This also works with lambdas, case statements and other blocks:

```hs
(f \x -> y)
-- is now equivalent to 
(f (\x -> y))
```

### TypeOperators
```hs
{-# LANGUAGE TypeOperators #-}
> :set -XTypeOperators
```

- In Haskell2010, expressions like (+) in a type are parsed as *type variables*, which is not very useful.
- This extension allows you to use operators as names for *type constructors*, *type synonyms*, etc.
- Do not use `*` ("type", "star") for this, it's reserved for type kinds

```hs
type a + b = Either a b
```

### NumericUnderscores
```hs
{-# LANGUAGE NumericUnderscores #-}
> :set -XNumericUnderscores
```

Write `100_000_001` instead of `100000001`

<!-- #endregion -->


## Overloaded Literals
<!-- #region Overloaded Literals -->

### OverloadedStrings

- packed strings, less waste then noraml linked-list strings

```hs
{-# LANGUAGE OverloadedStrings #-}
> :set -XOverloadedStrings
```

Overload string literals, similar to numeric literals.
This means that `"a string"` has type `IsString a => a`, and you can define your own instances of `IsString`.

For example, a library for regular expressions might define an instance `IsString Regex` that parses regular expressions, enabling you to write:

```hs
r :: Regex
r = "^x$"

-- it means that a string has type constraint
"string" :: IsString a => a
```

### OverloadedLists

```hs
{-# LANGUAGE OverloadedLists #-}
> :set -XOverloadedLists
```

Overload list literals, similar to numeric literals and string literals.

This means that `[1, 2]` has type `(IsList l, Num (Item l)) => l`
(`Item` is an associated type of the `IsList` class; see `TypeFamilies`)

You can then define instances of `IsList` for list-like types like vectors and sets and write:

```hs
s :: Set
s = [1, 2, 3]
```

<!-- #endregion -->
