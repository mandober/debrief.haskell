# GHC Extensions Index

https://damianfral.github.io/ghcaniuse/


> :! ghc --supported-extensions

The only extensions for which there are no `No{EXT}` versions.

The first two of those are sort of like extension bundles whose only purpose is to set a whole bunch of other extension flags with the goal of being compliant with a particular Haskell standard.
- Haskell98
- Haskell2010

The last three act together as a three-way switch, in a similar way to how all other extensions act as a two-way switch with their
- Unsafe
- Trustworthy
- Safe


These extensions have the opposite version with `No{EXT_NAME}`
(e.g. `AllowAmbiguousTypes` has `NoAllowAmbiguousTypes`)


AllowAmbiguousTypes
AlternativeLayoutRule
AlternativeLayoutRuleTransitional
Arrows
AutoDeriveTypeable
BangPatterns
BinaryLiterals
CApiFFI
CPP
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
DisambiguateRecordFieldsNoDisambiguateRecordFields
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
ImpredicativeTypes
IncoherentInstances
TypeFamilyDependencies
InstanceSigs
ApplicativeDo
InterruptibleFFI
JavaScriptFFI
KindSignatures
LambdaCase
LiberalTypeSynonyms
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
HexFloatLiterals
NondecreasingIndentation
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
ViewPatterns
