# LANGUAGE pragmas by category


## Deriving
- `DeriveTraversable` (implies *DeriveFunctor* and *DeriveFoldable*)
  - `DeriveFunctor`     (implied by *DeriveTraversable*)
  - `DeriveFoldable`    (implied by *DeriveTraversable*)

- `StandaloneDeriving` Enable standalone deriving.
- `GeneralisedNewtypeDeriving` Enable newtype deriving.

- `EmptyDataDeriving` deriving instances for empty data types.
- `DeriveDataTypeable` Enable deriving for the *Data* class.
- `DeriveGeneric`      Enable deriving for the *Generic* class.
- `DeriveLift`         Enable deriving for the *Lift* class
- `DeriveAnyClass`     Enable deriving for any class.

- `DerivingVia` deriving instances via types of the same RT representation 
  (implies *DerivingStrategies*)
  - `DerivingStrategies` Enables deriving strategies, implied by *DerivingVia*


## Literals
`BinaryLiterals`      Enable support for binary literal
`HexFloatLiterals`    Enable support for hex floating point literals
`NegativeLiterals`    Enable support for negative literals
`NumDecimals`         Enable support for 'fractional' integer literals

`UnicodeSyntax`       Enable unicode syntax.
`NumericUnderscores`  Enable support for numeric underscores.


## Records
`RecordWildCards`    record wildcards, implies *DisambiguateRecordFields*
  - `DisambiguateRecordFields` Record field disambiguation.
  (impl by RecordWildCards.)

`DuplicateRecordFields` Allow def of records with identically-named fields.
`NamedFieldPuns` Enable record puns.

`TraditionalRecordSyntax` Disable support for traditional record syntax
(as supported by Haskell 98) `C {f = x}`


## Pattern Matching

`ViewPatterns`    Enable view patterns.
`NamedWildCards`  Enable named wildcards.
`PatternSynonyms` Enable pattern synonyms.
`PatternGuards`   Disable pattern guards. Implied by Haskell98.







`BlockArguments`      Allow do blocks and other constructs as function args

`ApplicativeDo`       Enable Applicative do-notation desugaring
`AllowAmbiguousTypes` Allow writing and inferring ambiguous types
`Arrows`       Enable arrow notation extension
`BangPatterns` Enable bang patterns.
`ConstrainedClassMethods` Enable constrained class methods.
`ConstraintKinds` Enable a kind of constraints.
`CPP` Enable the C preprocessor.
`CUSKs` Enable detection of complete user-supplied kind signatures.
`DataKinds` Enable datatype promotion.
`DatatypeContexts` Allow contexts on data types.
`DefaultSignatures` Enable default signatures.

`EmptyCase` Allow empty case alternatives.
`EmptyDataDecls` Allow definition of empty data types.



`FlexibleInstances` Enable flexible instances. Implies *TypeSynonymInstances*.

`TypeSynonymInstances` Enable type synonyms in instance heads. Implied by *FlexibleInstances*.

`LiberalTypeSynonyms`       Enable liberalised type synonyms.
`ExistentialQuantification` Enable liberalised type synonyms.

`ExplicitForAll` Enable explicit universal quantification. Implied by ScopedTypeVariables, LiberalTypeSynonyms, ExistentialQuantification, RankNTypes

`FlexibleContexts`    Enable flexible contexts.
`InstanceSigs`         Enable instance signatures.

`IncoherentInstances` Enable incoherent instances. Implies *OverlappingInstances*.
- `OverlappingInstances` Enable overlapping instances.



`ExplicitNamespaces`
Enable using the keyword `type` to specify the namespace of entries in imports and exports (Explicit namespaces in import/export). Implied by TypeOperators and TypeFamilies.

`ExtendedDefaultRules` Use GHCi's extended default rules in a normal module.


`FunctionalDependencies` Enable functional dependencies. 
Implies *MultiParamTypeClasses*.

`GADTs` Enable generalised algebraic data types.
Implies *GADTSyntax* and *MonoLocalBinds*.
`GADTSyntax` Enable generalised algebraic data type syntax.

`ImplicitParams` Enable Implicit Parameters.

`ImplicitPrelude` Dont import Prelude. Implied by *RebindableSyntax*.
`RebindableSyntax` Employ rebindable syntax. Implies NoImplicitPrelude.

`ImportQualifiedPost` allows the syntax *import M qualified*

`ImpredicativeTypes` Enable impredicative types. Implies *RankNTypes*.


`KindSignatures` Enable kind signatures.
Implied by *TypeFamilies* and *PolyKinds*.

`LambdaCase` Enable lambda-case expressions.

`MagicHash` Allow `#` as a postfix modifier on identifiers.

`MonadComprehensions` Enable monad comprehensions.

`MonadFailDesugaring` Enable monadfail desugaring.

`MonoLocalBinds` Enable do not generalise local bindings.
Implied by *TypeFamilies* and *GADTs*.

`MonomorphismRestriction` Disable the monomorphism restriction.

`MultiParamTypeClasses` Enable multi parameter type classes.
Implied by *FunctionalDependencies*.

`MultiWayIf` Enable multi-way if-expressions.

`IndecreasingIndentation` Allow nested contexts to be at the same indentation level as its enclosing context.

`OverloadedLabels`  Enable overloaded labels.
`OverloadedLists`   Enable overloaded lists.
`OverloadedStrings` Enable overloaded string literals.

`PackageImports` Enable package-qualified imports.

`ParallelListComp` Enable parallel list comprehensions.

`PartialTypeSignatures` Enable partial type signatures.

`PostfixOperators` Enable postfix operators.

`QuantifiedConstraints` Allow forall quantifiers in constraints.

`PolyKinds` Enable kind polymorphism. Implies KindSignatures.
`RankNTypes` Enable rank-N types. Implied by ImpredicativeTypes.
  `Rank2Types` Enable rank-2 types. Synonym for RankNTypes.

`RecursiveDo` Enable recursive do (mdo) notation.

`RoleAnnotations` Enable role annotations.

`ScopedTypeVariables` Enable lexically-scoped type variables.

`StandaloneKindSignatures` (GHC 8.10.1)

`StarIsType` Treat `*` as *Data.Kind.Type*.

`Strict` Make bindings in the current module strict by default.
`StrictData` Enable default strict datatype fields.

`TransformListComp` Enable generalised list comprehensions.

`TupleSections` Enable tuple sections.

`TypeApplications` Enable type application syntax in terms and types.

`TypeFamilies
Enable type families. Implies *ExplicitNamespaces*, *KindSignatures*, and *MonoLocalBinds*.
`TypeFamilyDependencies` Enable injective type families. Implies *TypeFamilies*.


`TypeOperators` Enable type operators. Implies *ExplicitNamespaces*.

`UnboxedSums` Enable unboxed sums.
`UnboxedTuples` Enable the use of unboxed tuple syntax.

`UndecidableInstances` Enable undecidable instances.
`UndecidableSuperClasses`
Allow all superclass constraints, including those that may result in non-termination of the typechecker.

`UnliftedNewtypes` Enable unlifted newtypes (GHC 8.10.1)


`TemplateHaskell`       Enable Template Haskell.
`TemplateHaskellQuotes` Enable quotation subset of Template Haskell.
`QuasiQuotes` Enable quasiquotation.

`Haskell2010`    Use the Haskell 2010 language variant.
`Haskell98`      Use the Haskell 2010 language variant.

`Safe`           Enable the Safe Haskell Safe mode.
`Unsafe`         Enable Safe Haskell Unsafe mode.
`Trustworthy`    Enable the Safe Haskell Trustworthy mode.


`StaticPointers` Enable static pointers.


## FFI
`ForeignFunctionInterface` Enable FFI.
`InterruptibleFFI`         Enable interruptible FFI.
`CApiFFI`                  Enable the CAPI calling convention.
`UnliftedFFITypes`         Enable unlifted FFI types


## Deprecated
`TypeInType` Deprecated. Enable kind polymorphism and datatype promotion.
`NPlusKPatterns` Enable support for n+k patterns. Implied by *Haskell98*.

`NullaryTypeClasses` Deprecated, does nothing.
nullary type classes are now enabled using *MultiParamTypeClasses*.





---

## fglasgow-exts

The flag `-fglasgow-exts` (*deprecated*) is equivalent to enabling the following extensions:

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


Enabling these options is the only effect of `-fglasgow-exts`.
We are trying to move away from this portmanteau flag, and towards enabling 
features individually.
