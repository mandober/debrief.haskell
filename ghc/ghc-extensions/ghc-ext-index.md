# GHC Extensions Index

https://damianfral.github.io/ghcaniuse/


> :! ghc --supported-extensions

The only extensions for which there are no `No{EXT}` versions are the following five:

The first two of those are extension bundles whose purpose is to set a bunch of other flags but kepping compliancy with a particular Haskell standard.
- *Haskell98*
- *Haskell2010*

The last three act together as a three-way switch (similarly how all other extensions act as a two-way switch with their negations):
- Unsafe
- Trustworthy
- Safe


## Extensions proper

Each of the following extensions has its opposite version obtained by the `No` prefix (e.g. `AllowAmbiguousTypes` has `NoAllowAmbiguousTypes`):


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
