# GHC :: Language extensions :: All extensions

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/table.html

Legend:
- Ty: Extension types
  - AA: Haskell modes (Arrows, Parallel, Un/Safe, Trustworthy, Linear, TH, …)
  - ff: FFI
  - de: deriving mechanisms
  - re: records
  - sy: syntax
  - tc: type classes
  - th: Template Haskell
  - ty: types (in general)
- Extension sets:
  - 1: GHC2021
  - 0: Haskell2010
  - 8: Haskell98
  - G: Glasgow (-fglasgow-exts)
  - m: Personal set



                                    /1/ / /w/
                                   /2/0/ /o/
                                  /0/1/ /g/
                                e/2/0/8/s/e
                               p/C/2/9/a/n
                              y/H/S/s/l/i
                             t/G/H/h/G/M
Extension                  |Ty|1|0|8|G|m| Since | Notes
---------------------------|--|-|-|-|-|-|-------|------------------------------------------------------------------------
AllowAmbiguousTypes        |tc| | | | | | 7.8.1 | Allow type signatures which appear they'd result in an unusable binding.
ApplicativeDo              |sy| | | | |m| 8.0.1 | do-notation for Applicatives.
Arrows                     |AA| | | | | | 6.8.1 | Hughes arrows, a generalisation of monads.
BangPatterns               |sy|1| | | |m| 6.8.1 | Bang patterns.
BinaryLiterals             |sy|1| | | |m| 7.10.1| Binary notation: `0b0101` or `0B1010`
BlockArguments             |sy| | | | |m| 8.6.1 | do-exp, lambda exp, etc. as args.
CApiFFI                    |ff| | | | | | 7.6.1 | calling convention `capi` in foreign declarations.
CPP                        |AA| | | | | | 6.8.1 | C pre-processor
CUSKs                      |ty| |0|8| | | 8.10.1| DEPR by `StandaloneKindSignatures`
ConstrainedClassMethods    |ty|1| | |w|m| 6.8.1 | Placing further constraints on methods.
ConstraintKinds            |ty|1| | | |m| 7.4.1 | types of kind `Constraint` in contexts.
DataKinds                  |ty| | | | |m| 7.4.1 | datatype promotion.
DatatypeContexts           |ty| |0|8| | | 7.0.1 | (DEPR) Put a context on a `data` declaration.
DeepSubsumption            |ty| | | | | | 9.2.4 | Relax subsumption rules; implicitly eta-expand when matching fn types with diff quantification structures.
DefaultSignatures          |ty| | | | |m| 7.2.1 | place default method signatures in class definitions.
DeriveAnyClass             |de| | | | | | 7.10.1| use any class in a `deriving` claus.
DeriveDataTypeable         |de|1| | |w|m| 6.8.1 | Auto-derive `Data` class instances.
DeriveFoldable             |de|1| | |w|m| 7.10.1| Auto-derive `Foldable` class instances.
DeriveFunctor              |de|1| | |w|m| 7.10.1| Auto-derive `Functor` class instances. Implied by `DeriveTraversable`.
DeriveGeneric              |de|1| | | |m| 7.2.1 | Auto-derive `Generic` class instances.
DeriveLift                 |de|1| | | |m| 8.0.1 | Auto-derive `Lift` class instances for Template Haskell.
DeriveTraversable          |de|1| | | |m| 7.10.1| Auto-derive `Traversable` class instances. Implies `DeriveFoldable`, `DeriveFunctor`.
DerivingStrategies         |de| | | | |m| 8.2.1 | Allow multiple deriving, each optionally qualified with a strategy.
DerivingVia                |de| | | | | | 8.6.1 | Derive an instance via another type.
DisambiguateRecordFields   |re| | | | |m| 6.8.1 | Allow the compiler to automatically choose between identically-named record fields (if the choice is unambiguous).
DoAndIfThenElse            |sy|1|0| | | |       | Manage if-then-else and do-blocks
DuplicateRecordFields      |re| | | | | | 8.0.1 | Allow definition of record types with identically-named fields.
EmptyCase                  |sy|1| | | |m| 7.8.1 | Allow empty case expressions.
EmptyDataDecls             |ty|1|0| |w|m| 6.8.1 | Allow definition of empty `data` types.
EmptyDataDeriving          |de|1| | | |m| 8.4.1 | Allow deriving instances of standard type classes for empty data types.
ExistentialQuantification  |ty|1| | |w|m| 6.8.1 | Allow existentially quantified type variables in types.
ExplicitForAll             |ty|1| | | |m| 6.12.1| Allow use of the forall keyword in places where universal quantification is implicit.
ExplicitNamespaces         |sy| | | |w| | 7.6.1 | Enable use of explicit namespaces in module export lists. Implied by `TypeOperators`, `TypeFamilies`.
ExtendedDefaultRules       |ty| | | | | | 6.8.1 | Allow defaulting to take place for more than just numeric classes.
FieldSelectors             |re|1|0|8| | | 9.2.1 | Make record field selector functions visible in expressions.
FlexibleContexts           |ty|1| | |w|m| 6.8.1 | Remove the type-variable restriction on class contexts.
FlexibleInstances          |ty|1| | |w|m| 6.8.1 | Allow definition of type class instances with arbitrary nested types in the instance head.
ForeignFunctionInterface   |ff|1|0| |w| | 6.8.1 | Allow use of the Haskell foreign function interface.
FunctionalDependencies     |ty| | | |w|m| 6.8.1 | Allow use of functional dependencies in class declarations.
GADTSyntax                 |sy|1| | | | | 7.2.1 | Allow the use of GADT syntax in data type definitions (but not GADTs themselves)
GADTs                      |ty| | | | |m| 6.8.1 | Generalised Algebraic Data Types. Implies `MonoLocalBinds`, `GADTSyntax`
GHC2021                    |AA| | | | | |       | Enable a bunch of safe extensions.
GHCForeignImportPrim       |ff| | | | | | 6.12.1| GHC extends the FFI with an additional calling convention `prim`
GeneralisedNewtypeDeriving |de|1| | |w|m| 8.6.1 | British spelling since 8.6.1.
GeneralizedNewtypeDeriving |de|1| | |w|m| 6.8.1 | Enable GHC's cunning generalised deriving mechanism for newtypes
Haskell2010                |AA| | | | | |       | Enable extensions that make the Haskell2010 standard.
Haskell98                  |AA| | | | | |       | Enable extensions to better comply with the Haskell98 standard.
HexFloatLiterals           |sy|1| | | |m| 8.4.1 | Allow writing floating point literals using hexadecimal notation.
ImplicitParams             |sy| | | |w| | 6.8.1 | Allow definition of functions expecting implicit parameters.
ImplicitPrelude            |sy|1|0|8|W| | 6.8.1 | GHC normally imports `Prelude.hi` files. Implied by `RebindableSyntax`.
ImportQualifiedPost        |sy|1| | | |m| 8.10.1| enable import syntax `import Mod qualified as M`
ImpredicativeTypes         |ty| | | | | | 9.2.1 | Allow impredicative polymorphic types. (since 9.2.1, was unreliable in 6.10 - 9.0)
IncoherentInstances        |ty| | | | | | 6.8.1 | DEPR. Weakens checks intended to ensure instance resolution termination.
InstanceSigs               |sy|1| | | |m| 7.6.1 | Type signatures on methods.
InterruptibleFFI           |ff| | | |w| | 7.2.1 | concerns the interaction of foreign calls with `Control.Concurrent.throwTo`
KindSignatures             |sy|1| | |w|m| 6.8.1 | explicit kind signatures on type vars. Implied by `TypeFamilies`, `PolyKinds`
LambdaCase                 |sy| | | | |m| 7.6.1 | enables exp of the form `\case { p1 -> e1; ...; pN -> eN }`
LexicalNegation            |sy| | | | |m| 9.0.1 | Differentiate between `-1` and `- 1`
LiberalTypeSynonyms        |sy| | | |w|m| 6.8.1 | Relax many of the Haskell 98 rules on type synonym definitions. Implies `ExplicitForAll`
LinearTypes                |ty| | | | | | 9.0.1 | Experimental.
MagicHash                  |sy| | | |w| | 6.8.1 | Use `#` char in names
MonadComprehensions        |sy| | | | |m| 7.2.1 | Comprehension syntax for arbitrary monads.
MonoLocalBinds             |ty| | | | | | 6.12.1| Infer less polymorphic types for local bindings by default. Implied by `TypeFamilies`, `GADTs`
MonomorphismRestriction    |ty|1|0|8| | | 6.8.1 | Prevents the compiler from applying the monomorphism restriction to bindings lacking explicit type signatures.
MultiParamTypeClasses      |sy|1| | |w|m| 6.8.1 | Implies `ConstrainedClassMethods`, Implied by `FunctionalDependencies`
MultiWayIf                 |sy| | | | |m| 7.6.1 | Multiway if branching
NPlusKPatterns             |sy| | |8| | |       | DEPR. Use `(n + 1)` patterns on the lhs.
NamedFieldPuns             |re|1| | | | |       | Records
NamedWildCards             |re|1| | | | |       | Records
NegativeLiterals           |sy| | | | |m|       | Differentiate between `-1` and `- 1`
NondecreasingIndentation   |sy| | |8| |m|       | Skip the first level of indentation.
NullaryTypeClasses         |tc| | | | | |       | DEPR
NumDecimals                |sy| | | | |m|       | whaa?
NumericUnderscores         |sy|1| | | |m|       | Use underscore as the digit separator.
OverlappingInstances       |ty| | | | | |       | Affects instance resolution.
OverloadedLabels           |sy| | | | | |       | Expose `HasField` class
OverloadedLists            |sy| | | | | |       | Expose `IsList` class
OverloadedRecordDot        |re| | | | |m|       | Differentiate between `r.f` and `r . f`
OverloadedRecordUpdate     |re| | | | |m|       | Records
OverloadedStrings          |sy| | | | | |       | Expose `IsString` class
PackageImports             |sy| | | | | |       | qualified imports by the packagename
ParallelListComp           |sy| | | |w| |       | list comprehensions with `par`
PartialTypeSignatures      |sy| | | | |m|       | Use `_` in sigs
PatternGuards              |sy|1|0| |w|m|       | Use `|` as guards
PatternSynonyms            |sy| | | | |m|       | Make aliases for patterns.
PolyKinds                  |ty|1| | | |m|       | polymorphic kinds, `forall k (a :: k)`
PostfixOperators           |sy|1| | |w|m|       | define factorial as `4 (!)`
QualifiedDo                |sy| | | | | |       | Qualify do-blocks
QuantifiedConstraints      |sy| | | | |m|       | use `(forall a. Eq a)`
QuasiQuotes                |th| | | | | |       | in TH
Rank2Types                 |ty| | | | | |       | DEPR by RankNTypes
RankNTypes                 |ty|1| | |w|m|       | (records)
RebindableSyntax           |sy| | | | | | 7.0.1 | Enable rebinding of a variety of usually-built-in operations. Implies `NoImplicitPrelude`.
RecordWildCards            |re| | | | | |       | (records)
RecursiveDo                |sy| | | |w| |       | `mdo`
RelaxedPolyRec             |ty|1|0| | | |       | wha?
RoleAnnotations            |ty| | | | |m|       | Annotate ty vars with roles: nominal, representational, phantom
Safe                       |AA| | | | | |       | Enable Safe Haskell subset.
ScopedTypeVariables        |ty|1| | |w|m|       | Scope forall tyvar over definitions.
StandaloneDeriving         |de|1| | |w|m|       | Standalone deriving statements.
StandaloneKindSignatures   |ty|1| | | |m| 8.10.1| implies `NoCUSKs`
StarIsType                 |sy|1|0|8| |m|       | In effect `NoStarIsType` means use `Type` instead of `*`
StaticPointers             |ff| | | | | |       | low-level
Strict                     |AA| | | | | |       | Enable strictness flag the current module.
StrictData                 |AA| | | | | |       | Enable strictness flag on all data in the current module.
TemplateHaskell            |th| | | | | |       | TH
TemplateHaskellQuotes      |th| | | | | |       | TH quoting mechanisms.
TraditionalRecordSyntax    |re|1|0|8| |m|       | affects records updates
TransformListComp          |sy| | | | | |       | Extended list comprehensions
Trustworthy                |AA| | | | | |       | Enable Trustworthy Haskell subset.
TupleSections              |ty|1| | | |m|       | USe `(,5) `
TypeApplications           |ty|1| | | |m|       | Use `T @Ty`
TypeData                   |ty| | | | | | 9.6.1 | Declare kinds only, supressing value-level names. Like promoted ctors.
TypeFamilies               |ty| | | | |m|       | TF: OTF and CTF
TypeFamilyDependencies     |ty| | | | |m|       | Place fundep on TF
TypeInType                 |ty| | | | | |       | DEPR in 9.6.1
TypeOperators              |ty|1| | |w|m|       | Allow symbolic names of (binary) data ctors
TypeSynonymInstances       |ty|1| | |w|m|       | Allow defining instances for type aliases (considered only as shorthands)
UnboxedSums                |ty| | | | | |       | .
UnboxedTuples              |ty| | | |w| |       | .
UndecidableInstances       |ty| | | | | |       | .
UndecidableSuperClasses    |ty| | | | | |       | Allow the possibility of superclass sending the typechecker into loop
UnicodeSyntax              |sy| | | |w| |       | Use ugly unicode glyphs
UnliftedDatatypes          |ty| | | | | |       | .
UnliftedFFITypes           |ff| | | |w| |       | ffi
UnliftedNewtypes           |ty| | | | | |       | .
Unsafe                     |AA| | | | | |       | Mark Module as Unsafe, as stepping away from the Safe Haskell subset.
ViewPatterns               |sy| | | | |m|       | use `<-` in patterns


Exts added
- GHC 9.6.1
- GHC 9.2.4
- GHC 9.2.1
- GHC 9.0.1
- GHC 8.10.1
- GHC 8.6.1
- GHC 8.4.1
- GHC 8.2.1
- GHC 8.0.1
- GHC 7.10.1
- GHC 7.8.1
- GHC 7.6.1
- GHC 7.4.1
- GHC 7.2.1
- GHC 7.0.1
- GHC 6.12.1
- GHC 6.8.1



* Latest GHC dev version
https://ghc.gitlab.haskell.org/ghc/doc/users_guide/index.html
(GHC 9.7.2022-12-25)
