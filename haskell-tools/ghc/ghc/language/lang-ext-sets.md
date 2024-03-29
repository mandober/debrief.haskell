# GHC Language Extensions Sets

# GHC2021

https://downloads.haskell.org/~ghc/9.2.1-alpha2/docs/html/users_guide/exts/control.html

GHC blesses a number of extensions, beyond Haskell 2010, to be suitable to turned on by default. These extensions are considered to be stable and conservative.

GHC2021 is used by GHC if neither Haskell98 nor Haskell2010 is turned on explicitly. Since later versions of GHC may use a later GHC20xx by default, users are advised to declare the language set explicitly with -XGHC2021.

The GHC2021 language set comprises the following extensions:

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
