# LANGUAGE pragmas



`AllowAmbiguousTypes`
Allow the user to write ambiguous types, and the type inference engine to infer them.

`ApplicativeDo`
Enable Applicative do-notation desugaring

`Arrows`
Enable arrow notation extension

`BangPatterns`
Enable bang patterns.

**BinaryLiterals**
Enable support for binary literals.

**BlockArguments**
Allow do blocks and other constructs as function arguments.

`CApiFFI`
Enable the CAPI calling convention.

`ConstrainedClassMethods`
Enable constrained class methods.

`ConstraintKinds`
Enable a kind of constraints.

`CPP`
Enable the C preprocessor.

`CUSKs`
Enable detection of complete user-supplied kind signatures.

`DataKinds`
Enable datatype promotion.

*DatatypeContexts*
Allow contexts on data types.

`DefaultSignatures`
Enable default signatures.

## deriving

**StandaloneDeriving**
Enable standalone deriving.

*DeriveAnyClass*
Enable deriving for any class.
:set -XDeriveAnyClass

`DeriveDataTypeable`
Enable deriving for the Data class.
(Implied by, deprecated, AutoDeriveTypeable)
:set -XDeriveDataTypeable

`DeriveFoldable` Enable deriving for the Foldable class.
Implied by *DeriveTraversable*.

`DeriveFunctor` Enable deriving for the Functor class.
Implied by *DeriveTraversable*.

*DeriveGeneric*
Enable deriving for the Generic class.
:set -XDeriveGeneric

*DeriveLift*
Enable deriving for the Lift class
:set -XDeriveLift

**DeriveTraversable**
Enable deriving for the Traversable class.
Implies DeriveFunctor and *DeriveFoldable*.

`DerivingStrategies`
Enables deriving strategies.

`DerivingVia`
Enable deriving instances via types of the same runtime representation.
Implies *DerivingStrategies*.



`DisambiguateRecordFields`
Enable record field disambiguation.
Implied by *RecordWildCards*.

`DuplicateRecordFields`
Allow definition of record types with identically-named fields.

`EmptyCase`
Allow empty case alternatives.

`EmptyDataDecls`
Allow definition of empty data types.

`EmptyDataDeriving`
Allow deriving instances of standard type classes for empty data types.

`GeneralisedNewtypeDeriving`
Enable newtype deriving.

`ExistentialQuantification`
Enable liberalised type synonyms.

`ExplicitForAll`
Enable explicit universal quantification. Implied by *ScopedTypeVariables*, *LiberalTypeSynonyms*, *RankNTypes* and *ExistentialQuantification*.

`ExplicitNamespaces`
Enable using the keyword `type` to specify the namespace of entries in imports and exports (Explicit namespaces in import/export). Implied by *TypeOperators* and *TypeFamilies*.

`ExtendedDefaultRules`
Use GHCi's extended default rules in a normal module.

`FlexibleContexts`
Enable flexible contexts.

`FlexibleInstances`
Enable flexible instances. Implies *TypeSynonymInstances*.

`ForeignFunctionInterface`
Enable FFI.

`FunctionalDependencies`
Enable functional dependencies.
Implies *MultiParamTypeClasses*.

`GADTs`
Enable generalised algebraic data types.
Implies *GADTSyntax* and *MonoLocalBinds*.

`GADTSyntax`
Enable generalised algebraic data type syntax.

`Haskell2010`
Use the Haskell 2010 language variant.

`Haskell98`
Use the Haskell 2010 language variant.

`HexFloatLiterals`
Enable support for hexadecimal floating point literals.

`ImplicitParams`
Enable Implicit Parameters.

`ImplicitPrelude`
Don’t implicitly import Prelude. Implied by *RebindableSyntax*.

`ImportQualifiedPost`
allows the syntax *import M qualified*

`ImpredicativeTypes`
Enable impredicative types. Implies *RankNTypes*.

`IncoherentInstances`
Enable incoherent instances. Implies *OverlappingInstances*.

`InstanceSigs`
Enable instance signatures.

`InterruptibleFFI`
Enable interruptible FFI.

`KindSignatures` Enable kind signatures.
Implied by *TypeFamilies* and *PolyKinds*.

`LambdaCase`
Enable lambda-case expressions.

`LiberalTypeSynonyms`
Enable liberalised type synonyms.

`MagicHash`
Allow `#` as a postfix modifier on identifiers.

`MonadComprehensions`
Enable monad comprehensions.

`MonadFailDesugaring`
Enable monadfail desugaring.

`MonoLocalBinds`
Enable do not generalise local bindings.
Implied by *TypeFamilies* and *GADTs*.

`MonomorphismRestriction`
Disable the monomorphism restriction.

`MultiParamTypeClasses`
Enable multi parameter type classes.
Implied by *FunctionalDependencies*.

`MultiWayIf`
Enable multi-way if-expressions.

`NamedFieldPuns`
Enable record puns.

`NamedWildCards`
Enable named wildcards.

`IndecreasingIndentation`
Allow nested contexts to be at the same indentation level as its enclosing context.

`NegativeLiterals`
Enable support for negative literals.

`NPlusKPatterns`
Enable support for n+k patterns. Implied by *Haskell98*.

`NullaryTypeClasses`
**Deprecated**, does nothing.
nullary type classes are now enabled using *MultiParamTypeClasses*.

`NumDecimals`
Enable support for ‘fractional’ integer literals.

`NumericUnderscores`
Enable support for numeric underscores.

`OverlappingInstances`
Enable overlapping instances.

`OverloadedLabels`
Enable overloaded labels.

`OverloadedLists`
Enable overloaded lists.

`OverloadedStrings`
Enable overloaded string literals.

`PackageImports`
Enable package-qualified imports.

`ParallelListComp`
Enable parallel list comprehensions.

`PartialTypeSignatures`
Enable partial type signatures.

`PatternGuards`
Disable pattern guards. Implied by Haskell98.

`PatternSynonyms`
Enable pattern synonyms.

`PolyKinds`
Enable kind polymorphism. Implies KindSignatures.

`PostfixOperators`
Enable postfix operators.

`QuantifiedConstraints`
Allow forall quantifiers in constraints.

`QuasiQuotes`
Enable quasiquotation.

`Rank2Types`
Enable rank-2 types. Synonym for RankNTypes.

`RankNTypes`
Enable rank-N types. Implied by ImpredicativeTypes.

`RebindableSyntax`
Employ rebindable syntax. Implies NoImplicitPrelude.

`RecordWildCards`
Enable record wildcards. Implies DisambiguateRecordFields.

`RecursiveDo`
Enable recursive do (mdo) notation.

`RoleAnnotations`
Enable role annotations.

`Safe`
Enable the Safe Haskell Safe mode.

`ScopedTypeVariables`
Enable lexically-scoped type variables.

`StandaloneKindSignatures`
Allow the use of standalone kind signatures.

`StarIsType`
Treat `*` as *Data.Kind.Type*.

`StaticPointers`
Enable static pointers.

`Strict`
Make bindings in the current module strict by default.

`StrictData`
Enable default strict datatype fields.

`TemplateHaskell`
Enable Template Haskell.

`TemplateHaskellQuotes`
Enable quotation subset of Template Haskell.

`TraditionalRecordSyntax`
Disable support for traditional record syntax
(as supported by Haskell 98) `C {f = x}`

`TransformListComp`
Enable generalised list comprehensions.

`Trustworthy`
Enable the Safe Haskell Trustworthy mode.

`TupleSections`
Enable tuple sections.

`TypeApplications`
Enable type application syntax in terms and types.

`TypeFamilies`
Enable type families. Implies *ExplicitNamespaces*, *KindSignatures*, and *MonoLocalBinds*.

`TypeFamilyDependencies`
Enable injective type families. Implies *TypeFamilies*.

TypeInType
**Deprecated**. Enable kind polymorphism and datatype promotion.

`TypeOperators`
Enable type operators. Implies *ExplicitNamespaces*.

`TypeSynonymInstances`
Enable type synonyms in instance heads. Implied by *FlexibleInstances*.

`UnboxedSums`
Enable unboxed sums.

`UnboxedTuples`
Enable the use of unboxed tuple syntax.

`UndecidableInstances`
Enable undecidable instances.

`UndecidableSuperClasses`
Allow all superclass constraints, including those that may result in non-termination of the typechecker.

`UnicodeSyntax`
Enable unicode syntax.

`UnliftedFFITypes`
Enable unlifted FFI types

`UnliftedNewtypes`
Enable unlifted newtypes.

`Unsafe`
Enable Safe Haskell Unsafe mode.

`ViewPatterns`
Enable view patterns.



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
