# Overview

* Overview of all language extensions
https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/table.html

* All language extensions in GHC 9.0.1
https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/table.html

* History: Language pragma
https://gitlab.haskell.org/ghc/ghc/-/wikis/language-pragma-history



Ext                        | Desc
---------------------------|---------------------------------------------------------------------
Safe                       | Enable the Safe Haskell Safe mode
Unsafe                     | Enable Safe Haskell Unsafe mode
Trustworthy                | Enable the Safe Haskell Trustworthy mode
Haskell2010                | Use the Haskell 2010 language variant
Haskell98                  | Use the Haskell 98 language variant
---------------------------|---------------------------------------------------------------------
CUSKs                      | [LEGACY] Enable detection of complete user-supplied kind signatures
DatatypeContexts           | [LEGACY] Allow contexts on data types
NPlusKPatterns             | [LEGACY] Enable support for n+k patterns. Implied by *Haskell98*
NullaryTypeClasses         | [LEGACY] use *MultiParamTypeClasses*
TypeInType                 | [LEGACY] Enable kind polymorphism and datatype promotion
---------------------------|---------------------------------------------------------------------
`CPP`                      | Enable the C preprocessor
ForeignFunctionInterface   | Enable foreign function interface
CApiFFI                    | Enable the CAPI calling convention
UnliftedFFITypes           | Enable unlifted FFI types
InterruptibleFFI           | Enable interruptible FFI
---------------------------|---------------------------------------------------------------------
AllowAmbiguousTypes        | Allow the user to write ambiguous types, and the type inference engine to infer them
`ApplicativeDo`            | Enable Applicative do-notation desugaring
Arrows                     | Enable arrow notation extension
`BangPatterns`             | Enable bang patterns
`BinaryLiterals`           | Enable support for binary literals
`BlockArguments`           | Allow do blocks and other constructs as function arguments
`ConstrainedClassMethods`  | Enable constrained class methods
ConstraintKinds            | Enable a kind of constraints
`DataKinds`                | Enable datatype promotion
DefaultSignatures          | Enable default signatures
DeriveAnyClass             | Enable deriving for any class
DeriveDataTypeable         | Enable deriving for the Data class
`DeriveFoldable`           | Implied by **DeriveTraversable**
`DeriveFunctor`            | Implied by **DeriveTraversable**
DeriveGeneric              | Enable deriving for the Generic class
DeriveLift                 | Enable deriving for the Lift class
DeriveTraversable          | Implies *DeriveFunctor* and *DeriveFoldable*
DerivingStrategies         | Enables deriving strategies
DerivingVia                | Enable deriving instances via types of the same runtime representation. Implies *DerivingStrategies*
DisambiguateRecordFields   | Enable record field disambiguation. Implied by *RecordWildCards*
DuplicateRecordFields      | Allow definition of record types with identically-named fields
`EmptyCase`                | Allow empty case alternatives
`EmptyDataDecls`           | Allow definition of empty data types
`EmptyDataDeriving`        | Allow deriving instances of standard type classes for empty data types
`ExistentialQuantification`| Enable liberalised type synonyms
`ExplicitForAll`           | Enable explicit universal quantification. Implied by ScopedTypeVariables, LiberalTypeSynonyms, RankNTypes, ExistentialQuantification
ExplicitNamespaces         | Enable using the keyword type to specify the namespace of entries in imports and exports (Explicit namespaces in import/export). Implied by TypeOperators and TypeFamilies
ExtendedDefaultRules       | Use GHCi's extended default rules in a normal module
FlexibleContexts           | Enable flexible contexts
FlexibleInstances          | Enable flexible instances. Implies *TypeSynonymInstances*
`FunctionalDependencies`   | Enable functional dependencies. Implies *MultiParamTypeClasses*
`GADTs`                    | Enable generalised algebraic data types. Implies *GADTSyntax*, *MonoLocalBinds*
`GADTSyntax`               | Enable generalised algebraic data type syntax
GeneralisedNewtypeDeriving | Enable newtype deriving
GHCForeignImportPrim       | Enable prim calling convention. Intended for internal use only
`HexFloatLiterals`         | Enable support for hexadecimal floating point literals
ImplicitParams             | Enable Implicit Parameters
ImplicitPrelude            | Don't implicitly import Prelude. Implied by *RebindableSyntax*
ImportQualifiedPost        | ImportQualifiedPost allows the syntax import M qualified
ImpredicativeTypes         | Enable impredicative types. Implies RankNTypes
IncoherentInstances        | Enable incoherent instances. Implies OverlappingInstances
InstanceSigs               | Enable instance signatures
KindSignatures             | Enable kind signatures. Implied by TypeFamilies and PolyKinds
LambdaCase                 | Enable lambda-case expressions
LexicalNegation            | Use whitespace to determine whether the minus sign stands for negation or subtraction
LiberalTypeSynonyms        | Enable liberalised type synonyms
LinearTypes                | Enable linear types
`MagicHash`                | Allow # as a postfix modifier on identifiers
MonadComprehensions        | Enable monad comprehensions
MonadFailDesugaring        | Enable monadfail desugaring
MonoLocalBinds             | Enable do not generalise local bindings. Implied by **TypeFamilies** and **GADTs**
MonomorphismRestriction    | Disable the monomorphism restriction
`MultiParamTypeClasses`    | Enable multi parameter type classes. Implied by **FunctionalDependencies**
MultiWayIf                 | Enable multi-way if-expressions
NamedFieldPuns             | Enable record puns
NamedWildCards             | Enable named wildcards
`NegativeLiterals`         | Enable support for negative literals
NondecreasingIndentation   | Allow nested contexts to be at the same indentation level as its enclosing context
`NumDecimals`              | Enable support for 'fractional' integer literals
`NumericUnderscores`       | Enable support for numeric underscores
OverlappingInstances       | Enable overlapping instances
OverloadedLabels           | Enable overloaded labels
`OverloadedLists`          | Enable overloaded lists
`OverloadedStrings`        | Enable overloaded string literals
PackageImports             | Enable package-qualified imports
ParallelListComp           | Enable parallel list comprehensions
`PartialTypeSignatures`    | Enable partial type signatures
PatternGuards              | Disable pattern guards. Implied by **Haskell98**
PatternSynonyms            | Enable pattern synonyms
PolyKinds                  | Enable kind polymorphism. Implies *KindSignatures*
`PostfixOperators`         | Enable postfix operators
QualifiedDo                | Enable qualified do-notation desugaring
QuantifiedConstraints      | Allow forall quantifiers in constraints
QuasiQuotes                | Enable quasiquotation
`Rank2Types`               | Enable rank-2 types. Synonym for *RankNTypes*
`RankNTypes`               | Enable rank-N types. Implied by **ImpredicativeTypes**
RebindableSyntax           | Employ rebindable syntax. Implies *NoImplicitPrelude*
RecordWildCards            | Enable record wildcards. Implies *DisambiguateRecordFields*
RecursiveDo                | Enable recursive do (mdo) notation
`RoleAnnotations`          | Enable role annotations
`ScopedTypeVariables`      | Enable lexically-scoped type variables
`StandaloneDeriving`       | Enable standalone deriving
`StandaloneKindSignatures` | Allow the use of standalone kind signatures
`StarIsType`               | Treat `*` as Data.Kind.`Type`
StaticPointers             | Enable static pointers
`Strict`                   | Make bindings in the current module strict by default
`StrictData`               | Enable default strict datatype fields
`TemplateHaskell`          | Enable Template Haskell
TemplateHaskellQuotes      | Enable quotation subset of Template Haskell
TraditionalRecordSyntax    | Disable support for traditional record syntax (as supported by Haskell 98) C {f = x}
TransformListComp          | Enable generalised list comprehensions
TupleSections              | Enable tuple sections
`TypeApplications`         | Enable type application syntax in terms and types
`TypeFamilies`             | Enable type families. Implies *ExplicitNamespaces*, *KindSignatures*, *MonoLocalBinds*
TypeFamilyDependencies     | Enable injective type families. Implies *TypeFamilies*
`TypeOperators`            | Enable type operators. Implies *ExplicitNamespaces*
`TypeSynonymInstances`     | Enable type synonyms in instance heads. Implied by **FlexibleInstances**
UnboxedSums                | Enable unboxed sums
UnboxedTuples              | Enable the use of unboxed tuple syntax
UndecidableInstances       | Enable undecidable instances
UndecidableSuperClasses    | Allow all superclass constraints, including those that may result in non-termination of the typechecker
`UnicodeSyntax`            | Enable unicode syntax
UnliftedNewtypes           | Enable unlifted newtypes
UnliftedDatatypes          | Enable the declaration of data types with unlifted or levity-polymorphic result kind. 9.2.1
ViewPatterns               | Enable view patterns
