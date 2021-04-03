# GHC 9.0.1 Supported Extensions


```hs
{-# OPTIONS_HADDOCK prune #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE RoleAnnotations #-}
```

Haskell98
Haskell2010
Trustworthy
Safe | Unsafe

## Negatable Extensions
(126 extensions that can be negated with `No` prefix)

AllowAmbiguousTypes
AlternativeLayoutRule
AlternativeLayoutRuleTransitional
Arrows
AutoDeriveTypeable
BangPatterns
BinaryLiterals
CApiFFI
CPP
CUSKs
ConstrainedClassMethods
ConstraintKinds
DataKinds
DatatypeContexts
DefaultSignatures
DeriveAnyClass
DeriveDataTypeable
DeriveFoldable
DeriveFunctor
DeriveGeneric
DeriveLift
DeriveTraversable
DerivingStrategies
DerivingVia
DisambiguateRecordFields
DoAndIfThenElse
BlockArguments
DoRec
DuplicateRecordFields
EmptyCase
EmptyDataDecls
EmptyDataDeriving
ExistentialQuantification
ExplicitForAll
ExplicitNamespaces
ExtendedDefaultRules
FlexibleContexts
FlexibleInstances
ForeignFunctionInterface
FunctionalDependencies
GADTSyntax
GADTs
GHCForeignImportPrim
GeneralizedNewtypeDeriving
GeneralisedNewtypeDeriving
ImplicitParams
ImplicitPrelude
ImportQualifiedPost
ImpredicativeTypes
IncoherentInstances
TypeFamilyDependencies
InstanceSigs
ApplicativeDo
InterruptibleFFI
JavaScriptFFI
KindSignatures
LambdaCase
LexicalNegation
LiberalTypeSynonyms
LinearTypes
MagicHash
MonadComprehensions
MonadFailDesugaring
MonoLocalBinds
MonoPatBinds
MonomorphismRestriction
MultiParamTypeClasses
MultiWayIf
NumericUnderscores
NPlusKPatterns
NamedFieldPuns
NamedWildCards
NegativeLiterals
HexFloatLiteralsion
NullaryTypeClasses
NumDecimals
OverlappingInstances
OverloadedLabels
OverloadedLists
OverloadedStrings
PackageImports
ParallelArrays
ParallelListComp
PartialTypeSignatures
PatternGuards
PatternSignatures
PatternSynonyms
PolyKinds
PolymorphicComponents
QuantifiedConstraints
PostfixOperators
QuasiQuotes
QualifiedDo
Rank2Types
RankNTypes
RebindableSyntax
RecordPuns
RecordWildCards
RecursiveDo
RelaxedLayout
RelaxedPolyRec
RoleAnnotations
ScopedTypeVariables
StandaloneDeriving
StarIsType
StaticPointers
Strict
StrictData
TemplateHaskell
TemplateHaskellQuotes
StandaloneKindSignatures
TraditionalRecordSyntax
TransformListComp
TupleSections
TypeApplications
TypeInType
TypeFamilies
TypeOperators
TypeSynonymInstances
UnboxedTuples
UnboxedSums
UndecidableInstances
UndecidableSuperClasses
UnicodeSyntax
UnliftedFFITypes
UnliftedNewtypes
ViewPatterns


> `:show language`

The base language is Haskell2010 with the following modifiers:
- BinaryLiterals
- NoCUSKs
- ConstrainedClassMethods
- DeriveDataTypeable
- DeriveFoldable
- DeriveFunctor
- DeriveGeneric
- DeriveTraversable
- BlockArguments
- ExplicitForAll
- ExplicitNamespaces
- FlexibleContexts
- FlexibleInstances
- GeneralizedNewtypeDeriving
- GeneralisedNewtypeDeriving
- InstanceSigs
- KindSignatures
- LiberalTypeSynonyms
- MultiParamTypeClasses
- NumericUnderscores
- NegativeLiterals
- HexFloatLiterals
- PartialTypeSignatures
- PatternSignatures
- ScopedTypeVariables
- StandaloneDeriving
- StandaloneKindSignatures
- TupleSections
- TypeApplications
- TypeOperators
- TypeSynonymInstances
- UnicodeSyntax


> `:showi language`

The base language is Haskell2010 with the following modifiers:
- BinaryLiterals
- NoCUSKs
- ConstrainedClassMethods
- DeriveDataTypeable
- DeriveFoldable
- DeriveFunctor
- DeriveGeneric
- DeriveTraversable
- BlockArguments
- ExplicitForAll
- ExplicitNamespaces
- ExtendedDefaultRules          (ghci only)
- FlexibleContexts
- FlexibleInstances
- GeneralizedNewtypeDeriving
- GeneralisedNewtypeDeriving
- InstanceSigs
- KindSignatures
- LiberalTypeSynonyms
- NoMonomorphismRestriction     (ghci only)
- MultiParamTypeClasses
- NumericUnderscores
- NegativeLiterals
- HexFloatLiterals
- PartialTypeSignatures
- PatternSignatures
- ScopedTypeVariables
- StandaloneDeriving
- StandaloneKindSignatures
- TupleSections
- TypeApplications
- TypeOperators
- TypeSynonymInstances
- UnicodeSyntax
