# Language extensions

All extensions in 9.4.2
- 1:     GHC2021
- 0: Haskell2010
- 8:   Haskell98
- w:   -fglasgow-exts
- y:          my set

- k: kind of ext
  - A: Haskell modes
  - T: Template Haskell
  - D: deprecated
  - φ: FFI
  - O: obsolete
  - σ: syntax sugar
  - δ: deriving
  - ρ: records
  - τ: types

                              G     g
                              H     l
                              C     a
                              2 2   s
                              0 0   g
                              2 1 9 o m

ext                        |k|1|0|8|w|y|α|β|γ|ω| desc
---------------------------|-|-|-|-|-|-|-|-|-|-|-----------------------------------------------
BangPatterns               |σ|1| | | |✓| | | | | Enable bang patterns.
BinaryLiterals             |σ|1| | | |✓| | | | | Enable support for binary literals.
ConstrainedClassMethods    |τ|1| | |w|✓| | | | | Enable constrained class methods.
ConstraintKinds            |τ|1| | | |✓| | | | | Enable a kind of constraints.
DeriveDataTypeable         |δ|1| | |w|✓| | | | | Enable deriving for the Data class. Implied by the deprecated AutoDeriveTypeable.
DeriveFoldable             |δ|1| | |w|✓| | | | | Enable deriving for the Foldable class. Implied by DeriveTraversable.
DeriveFunctor              |δ|1| | |w|✓| | | | | Enable deriving for the Functor class. Implied by DeriveTraversable.
DeriveGeneric              |δ|1| | | |✓| | | | | Enable deriving for the Generic class.
DeriveLift                 |δ|1| | | |✓| | | | | Enable deriving for the Lift class
DeriveTraversable          |δ|1| | | |✓| | | | | Enable deriving for the Traversable class. Implies DeriveFunctor and DeriveFoldable.
EmptyCase                  |σ|1| | | |✓| | | | | Allow empty case alternatives.
EmptyDataDecls             |τ|1|0| |w|✓| | | | | Allow definition of empty data types.
EmptyDataDeriving          |δ|1| | | |✓| | | | | Allow deriving instances of standard type classes for empty data types.
ExistentialQuantification  |τ|1| | |w|✓| | | | | Enable liberalised type synonyms.
ExplicitForAll             |τ|1| | | |✓| | | | | Enable explicit universal quantification. Implied by ScopedTypeVariables, LiberalTypeSynonyms, RankNTypes and ExistentialQuantification.
FlexibleContexts           |τ|1| | |w|✓| | | | | Remove some restrictions on class contexts
FlexibleInstances          |τ|1| | |w|✓| | | | | Enable flexible instances. Implies TypeSynonymInstances.
GeneralisedNewtypeDeriving |δ|1| | |w|✓| | | | | Enable newtype deriving.
HexFloatLiterals           |σ|1| | | |✓| | | | | Enable support for hexadecimal floating point literals.
ImportQualifiedPost        |σ|1| | | |✓| | | | | ImportQualifiedPost allows the syntax import M qualified
InstanceSigs               |σ|1| | | |✓| | | | | Enable instance signatures.
KindSignatures             |σ|1| | |w|✓| | | | | Enable kind signatures. Implied by TypeFamilies and PolyKinds.
MultiParamTypeClasses      |σ|1| | |w|✓| | | | | Enable multi parameter type classes. Implied by FunctionalDependencies.
NumericUnderscores         |σ|1| | | |✓| | | | | Enable support for numeric underscores.
PatternGuards              |σ|1|0| |w|✓| | | | | Disable pattern guards. Implied by Haskell98.
PolyKinds                  |τ|1| | | |✓| | | | | Enable kind polymorphism. Implies KindSignatures.
PostfixOperators           |σ|1| | |w|✓| | | | | Enable postfix operators.
RankNTypes                 |τ|1| | |w|✓| | | | | Enable rank-N types. Implied by ImpredicativeTypes.
ScopedTypeVariables        |τ|1| | |w|✓| | | | | Enable lexically-scoped type variables.
StandaloneDeriving         |δ|1| | |w|✓| | | | | Enable standalone deriving.
StandaloneKindSignatures   |τ|1| | | |✓| | | | | Allow the use of standalone kind signatures.
StarIsType                 |σ|1|0|8| |✓| | | | | Treat * as Data.Kind.Type
TraditionalRecordSyntax    |ρ|1|0|8| |✓| | | | | Disable support for traditional record syntax (as supported by Haskell 98) C {f = x}
TupleSections              |τ|1| | | |✓| | | | | Enable tuple sections.
TypeApplications           |τ|1| | | |✓| | | | | Enable type application syntax in terms, patterns and types.
TypeOperators              |τ|1| | |w|✓| | | | | Enable type operators. Implies ExplicitNamespaces.
TypeSynonymInstances       |τ|1| | |w|✓| | | | | Enable type synonyms in instance heads. Implied by FlexibleInstances.
DoAndIfThenElse            |σ|1|0| | | | | | | | ?
FieldSelectors             |ρ|1|0|8| | | | | | | Control visibility of field selector functions.
GADTSyntax                 |σ|1| | | | | | | | | Enable generalised algebraic data type syntax.
ImplicitPrelude            |σ|1|0|8|W| | | | | | Don't implicitly import Prelude. Implied by RebindableSyntax.
MonomorphismRestriction    |τ|1|0|8| | | | | | | Disable the monomorphism restriction.
NamedFieldPuns             |ρ|1| | | | | | | | | Enable record puns.
NamedWildCards             |ρ|1| | | | | | | | | Enable named wildcards.
RelaxedPolyRec             |τ|1|0| | | | | | | |  ?
ForeignFunctionInterface   |φ|1|0| |w| | | | | | Enable foreign function interface.
AllowAmbiguousTypes        |τ| | | | | | | | | | Allow the user to write ambiguous types, and the type inference engine to infer them.
ApplicativeDo              |σ| | | | |✓| | | | | Enable Applicative do-notation desugaring
BlockArguments             |σ| | | | |✓| | | | | Allow do blocks and other constructs as function arguments.
DataKinds                  |τ| | | | |✓| | | | | Enable datatype promotion.
DefaultSignatures          |τ| | | | |✓| | | | | Enable default signatures.
DerivingStrategies         |δ| | | | |✓| | | | | Enables deriving strategies.
DisambiguateRecordFields   |ρ| | | | |✓| | | | | Enable record field disambiguation. Implied by RecordWildCards.
FunctionalDependencies     |τ| | | |w|✓| | | | | Enable functional dependencies. Implies MultiParamTypeClasses.
GADTs                      |τ| | | | |✓| | | | | Enable generalised algebraic data types. Implies GADTSyntax and MonoLocalBinds.
LambdaCase                 |σ| | | | |✓| | | | | Enable lambda-case expressions.
LexicalNegation            |σ| | | | |✓| | | | | Use whitespace to determine whether the minus sign stands for negation or subtraction.
LiberalTypeSynonyms        |σ| | | |w|✓| | | | | Enable liberalised type synonyms.
MonadComprehensions        |σ| | | | |✓| | | | | Enable monad comprehensions.
MultiWayIf                 |σ| | | | |✓| | | | | Enable multi-way if-expressions.
NegativeLiterals           |σ| | | | |✓| | | | | Enable support for negative literals.
NondecreasingIndentation   |σ| | |8| |✓| | | | | Allow nested contexts to be at the same indentation level as its enclosing context.
NumDecimals                |σ| | | | |✓| | | | | Enable support for 'fractional' integer literals.
OverloadedRecordDot        |ρ| | | | |✓| | | | | Record '.' syntax
OverloadedRecordUpdate     |ρ| | | | |✓| | | | | Record '.' syntax record updates
PartialTypeSignatures      |σ| | | | |✓| | | | | Enable partial type signatures.
PatternSynonyms            |σ| | | | |✓| | | | | Enable pattern synonyms.
QuantifiedConstraints      |σ| | | | |✓| | | | | Allow forall quantifiers in constraints.
RoleAnnotations            |τ| | | | |✓| | | | | Enable role annotations.
TypeFamilies               |τ| | | | |✓| | | | | Enable type families. Implies ExplicitNamespaces, KindSignatures, and MonoLocalBinds.
TypeFamilyDependencies     |τ| | | | |✓| | | | | Enable injective type families. Implies TypeFamilies.
ViewPatterns               |σ| | | | |✓| | | | | Enable view patterns.
CUSKs                      |τ| |0|8| | | | | | | Enable detection of complete user-supplied kind signatures.
DatatypeContexts           |τ| |0|8| | | | | | | Allow contexts on data types.
DeepSubsumption            |τ| | | | | | | | | | Enable deep subsumption
DeriveAnyClass             |δ| | | | | | | | | | Enable deriving for any class.
DerivingVia                |δ| | | | | | | | | | Enable deriving instances via types of the same runtime representation. Implies DerivingStrategies.
DuplicateRecordFields      |ρ| | | | | | | | | | Allow definition of record types with identically-named fields.
ExplicitNamespaces         |σ| | | |w| | | | | | Enable using the keyword type to specify the namespace of entries in imports and exports (Explicit namespaces in import/export). Implied by TypeOperators and TypeFamilies.
ExtendedDefaultRules       |τ| | | | | | | | | | Use GHCi's extended default rules in a normal module.
ImplicitParams             |σ| | | |w| | | | | | Enable Implicit Parameters.
ImpredicativeTypes         |τ| | | | | | | | | | Enable impredicative types. Implies RankNTypes.
IncoherentInstances        |τ| | | | | | | | | | Enable incoherent instances. Implies OverlappingInstances.
LinearTypes                |τ| | | | | | | | | | Enable linear types.
MagicHash                  |σ| | | |w| | | | | | Allow # as a postfix modifier on identifiers.
MonoLocalBinds             |τ| | | | | | | | | | Enable do not generalise local bindings. Implied by TypeFamilies and GADTs.
OverlappingInstances       |τ| | | | | | | | | | Enable overlapping instances.
OverloadedLabels           |σ| | | | | | | | | | Enable overloaded labels.
OverloadedLists            |σ| | | | | | | | | | Enable overloaded lists.
OverloadedStrings          |σ| | | | | | | | | | Enable overloaded string literals.
PackageImports             |σ| | | | | | | | | | Enable package-qualified imports.
ParallelListComp           |σ| | | |w| | | | | | Enable parallel list comprehensions.
QualifiedDo                |σ| | | | | | | | | | Enable qualified do-notation desugaring.
RebindableSyntax           |σ| | | | | | | | | | Employ rebindable syntax. Implies NoImplicitPrelude.
RecordWildCards            |ρ| | | | | | | | | | Enable record wildcards. Implies DisambiguateRecordFields.
RecursiveDo                |σ| | | |w| | | | | | Enable recursive do (mdo) notation.
TransformListComp          |σ| | | | | | | | | | Enable generalised list comprehensions.
UnboxedSums                |τ| | | | | | | | | | Enable unboxed sums.
UnboxedTuples              |τ| | | |w| | | | | | Enable the use of unboxed tuple syntax.
UndecidableInstances       |τ| | | | | | | | | | Enable undecidable instances.
UndecidableSuperClasses    |τ| | | | | | | | | | Allow all superclass constraints, including those that may result in non-termination of the typechecker.
UnicodeSyntax              |σ| | | |w| | | | | | Enable unicode syntax.
UnliftedDatatypes          |τ| | | | | | | | | | Enable unlifted data types.
UnliftedNewtypes           |τ| | | | | | | | | | Enable unlifted newtypes.
TemplateHaskell            |T| | | | | | | | | | Enable Template Haskell.
TemplateHaskellQuotes      |T| | | | | | | | | | Enable quotation subset of Template Haskell.
QuasiQuotes                |T| | | | | | | | | | Enable quasiquotation.
Arrows                     |A| | | | | | | | | | Enable arrow notation extension
CPP                        |A| | | | | | | | | | Enable the C preprocessor.
GHC2021                    |A| | | | | | | | | | Use GHC's set of default language extensions from 2021
Haskell2010                |A| | | | | | | | | | Use the Haskell 2010 language variant.
Haskell98                  |A| | | | | | | | | | Use the Haskell 98 language variant.
Trustworthy                |A| | | | | | | | | | Enable the Safe Haskell Trustworthy mode.
Strict                     |A| | | | | | | | | | Make bindings in the current module strict by default.
StrictData                 |A| | | | | | | | | | Enable default strict datatype fields.
Safe                       |A| | | | | | | | | | Enable the Safe Haskell Safe mode.
Unsafe                     |A| | | | | | | | | | Enable Safe Haskell Unsafe mode.
GHCForeignImportPrim       |φ| | | | | | | | | | Enable prim calling convention. Intended for internal use only.
CApiFFI                    |φ| | | | | | | | | | Enable the CAPI calling convention.
StaticPointers             |φ| | | | | | | | | | Enable static pointers.
InterruptibleFFI           |φ| | | |w| | | | | | Enable interruptible FFI.
UnliftedFFITypes           |φ| | | |w| | | | | | Enable unlifted FFI types
TypeInType                 |D| | | | | | | | | | Deprecated. Enable kind polymorphism and datatype promotion.
NullaryTypeClasses         |D| | | | | | | | | | Deprecated, does nothing. now enabled using MultiParamTypeClasses
Rank2Types                 |O| | | | | | | | | | Enable rank-2 types. Synonym for RankNTypes.
NPlusKPatterns             |O| | |8| | | | | | | Enable support for n+k patterns. Implied by Haskell98.


## GHC2021

The GHC2021 language set comprises the following extensions:

BangPatterns
BinaryLiterals
xonstrainedClassMethods
xonstraintKinds
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

## All

AllowAmbiguousTypes
ApplicativeDo
Arrows
BangPatterns
BinaryLiterals
BlockArguments
ConstrainedClassMethods
ConstraintKinds
CPP
CUSKs
DataKinds
DatatypeContexts
DeepSubsumption
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
DoAndIfThenElse
DisambiguateRecordFields
DuplicateRecordFields
EmptyCase
EmptyDataDecls
EmptyDataDeriving
ExistentialQuantification
ExplicitForAll
ExplicitNamespaces
ExtendedDefaultRules
FieldSelectors
FlexibleContexts
FlexibleInstances
FunctionalDependencies
GADTs
GADTSyntax
GeneralisedNewtypeDeriving
HexFloatLiterals
ImplicitParams
ImplicitPrelude
ImportQualifiedPost
ImpredicativeTypes
IncoherentInstances
InstanceSigs
KindSignatures
LambdaCase
LexicalNegation
LiberalTypeSynonyms
LinearTypes
MagicHash
MonadComprehensions
MonoLocalBinds
MonomorphismRestriction
MultiParamTypeClasses
MultiWayIf
NamedFieldPuns
NamedWildCards
NegativeLiterals
NondecreasingIndentation
NPlusKPatterns
NullaryTypeClasses
NumDecimals
NumericUnderscores
OverlappingInstances
OverloadedLabels
OverloadedLists
OverloadedRecordDot
OverloadedRecordUpdate
OverloadedStrings
PackageImports
ParallelListComp
PartialTypeSignatures
PatternGuards
PatternSynonyms
PolyKinds
PostfixOperators
QualifiedDo
QuantifiedConstraints
QuasiQuotes
RebindableSyntax
RecursiveDo
RelaxedPolyRec
ScopedTypeVariables
StandaloneDeriving
StandaloneKindSignatures
TraditionalRecordSyntax
TransformListComp
TupleSections
UnboxedSums
UnboxedTuples
UndecidableInstances
UndecidableSuperClasses
UnliftedDatatypes
UnliftedNewtypes

* Records
  RecordWildCards
* Syntax
  RoleAnnotations
  StarIsType
  UnicodeSyntax
  ViewPatterns
* Types
  RankNTypes
  TypeApplications
  TypeFamilies
  TypeFamilyDependencies
  TypeInType
  TypeOperators
  TypeSynonymInstances
* Template Haskell
  TemplateHaskell
  TemplateHaskellQuotes
* FFI
  CApiFFI
  ForeignFunctionInterface
  GHCForeignImportPrim
  InterruptibleFFI
  UnliftedFFITypes
  StaticPointers
* General
  Strict
  StrictData
  Safe
  Unsafe
  Trustworthy
* Reports
  GHC2021
  Haskell2010
  Haskell98
* Obsolete
  Rank2Types
* Deprecated


## My set

ApplicativeDo
BlockArguments
DataKinds
DefaultSignatures
DerivingStrategies
DisambiguateRecordFields
FunctionalDependencies
GADTs
LambdaCase
LexicalNegation
LiberalTypeSynonyms
MonadComprehensions
MultiWayIf
NegativeLiterals
NondecreasingIndentation
NumDecimals
OverloadedRecordDot
OverloadedRecordUpdate
PartialTypeSignatures
PatternSynonyms
QuantifiedConstraints
RoleAnnotations
TypeFamilies
TypeFamilyDependencies
ViewPatterns
