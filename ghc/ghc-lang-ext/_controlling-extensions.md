# Controlling extensions

Language extensions can be controlled in two ways:
* Command-line flag: enable `-X…`, disable `-XNo…`
* Using the `LANGUAGE` pragma, e.g. `{-# LANGUAGE {No}StarIsType #-}`

## Extensions sets

Currently (GHC 9.2.0.2021-08-21), there are 3+1 extensions sets: 3 sets that have progressed with the language and one extra set for "fuck-it" approach.
- langext `GHC2021`
- langext `Haskell2010`
- langext `Haskell98`
- flag `-fglasgow-exts` (the only set that can be negated: `-fno-glasgow-exts`)


## GHC Haskell 2021

The `GHC2021` enables the following extensions:

BangPatterns
BinaryLiterals
ConstrainedClassMethods
ConstraintKinds
DeriveDataTypeable
DeriveFoldable
DeriveFunctor
DeriveGeneric
DeriveLift
DeriveTraversable
DoAndIfThenElse
EmptyCase
EmptyDataDecls
EmptyDataDeriving
ExistentialQuantification
ExplicitForAll
FieldSelectors
FlexibleContexts
FlexibleInstances
ForeignFunctionInterface
GADTSyntax
GeneralisedNewtypeDeriving
HexFloatLiterals
ImplicitPrelude
ImportQualifiedPost
InstanceSigs
KindSignatures
MonomorphismRestriction
MultiParamTypeClasses
NamedFieldPuns
NamedWildCards
NumericUnderscores
PatternGuards
PolyKinds
PostfixOperators
RankNTypes
RelaxedPolyRec
ScopedTypeVariables
StandaloneDeriving
StandaloneKindSignatures
StarIsType
TraditionalRecordSyntax
TupleSections
TypeApplications
TypeOperators
TypeSynonymInstances


## Haskell2010

The `Haskell2010` enables the following extensions:

CUSKs
DatatypeContexts
DoAndIfThenElse
EmptyDataDecls
FieldSelectors
ForeignFunctionInterface
ImplicitPrelude
MonomorphismRestriction
PatternGuards
RelaxedPolyRec
StarIsType
TraditionalRecordSyntax


## Haskell 1998

The `Haskell98` enables the following extensions:

CUSKs
DatatypeContexts
FieldSelectors
ImplicitPrelude
MonomorphismRestriction
NondecreasingIndentation
NPlusKPatterns
StarIsType
TraditionalRecordSyntax



## Glasgow extensions

The deprecated `-fglasgow-exts` flag enables a large swath of extensions:

ConstrainedClassMethods
DeriveDataTypeable
DeriveFoldable
DeriveFunctor
DeriveGeneric
DeriveTraversable
EmptyDataDecls
ExistentialQuantification
ExplicitNamespaces
FlexibleContexts
FlexibleInstances
ForeignFunctionInterface
FunctionalDependencies
GeneralizedNewtypeDeriving
ImplicitParams
InterruptibleFFI
KindSignatures
LiberalTypeSynonyms
MagicHash
MultiParamTypeClasses
ParallelListComp
PatternGuards
PostfixOperators
RankNTypes
RecursiveDo
ScopedTypeVariables
StandaloneDeriving
TypeOperators
TypeSynonymInstances
UnboxedTuples
UnicodeSyntax
UnliftedFFITypes
