# GHC all options


<!-- #region      d OPTIONS -->
-dno-debug-output
-dno-ppr-case-as-let
-dno-ppr-ticks
-dno-suppress-coercions
-dno-suppress-idinfo
-dno-suppress-module-prefixes
-dno-suppress-type-applications
-dno-suppress-type-signatures
-dno-suppress-unfoldings
-dno-suppress-uniques
-dno-suppress-var-kinds

-dppr-case-as-let
-dppr-cols
-dppr-debug
-dppr-ticks
-dppr-user-length

-dsuppress-coercions
-dsuppress-idinfo
-dsuppress-module-prefixes
-dsuppress-type-applications
-dsuppress-type-signatures
-dsuppress-unfoldings
-dsuppress-uniques
-dsuppress-var-kinds
<!-- #endregion -->

---

<!-- #region      f OPTIONS -->
-fallow-incoherent-instances
-fallow-overlapping-instances
-fallow-undecidable-instances
-farrows
-fbang-patterns
-fbreak-on-error
-fbreak-on-exception
-fbuilding-cabal-package
-fbyte-code
-fcall-arity
-fcase-merge
-fcmm-elim-common-blocks
-fcmm-sink
-fconstraint-solver-iterations
-fcontext-stack
-fcpr-anal
-fcross-module-specialise
-fcse
-fdefer-out-of-scope-variables
-fdefer-type-errors
-fdefer-typed-holes
-fdicts-cheap
-fdicts-strict
-fdmd-tx-dict-sel
-fdo-eta-reduction
-fdo-lambda-eta-expansion
-feager-blackholing
-fembed-manifest
-fenable-rewrite-rules
-ferror-spans
-fexcess-precision
-fexpose-all-unfoldings
-fextended-default-rules
-fexternal-interpreter
-fffi
-ffi
-fflat-cache
-ffloat-all-lams
-ffloat-in
-ffloat-lam-args
-fforce-recomp
-ffull-laziness
-ffun-to-thunk
-fgen-manifest
-fghci-hist-size
-fghci-history
-fghci-sandbox
-fglasgow-exts
-fhelpful-errors
-fhistory-size
-fhpc
-fignore-asserts
-fignore-interface-pragmas
-fimplicit-import-qualified
-fimplicit-params
-fimplicit-prelude
-firrefutable-tuples
-fkill-absence
-fkill-one-shot
-flate-dmd-anal
-fliberate-case
-fliberate-case-threshold
-floopification
-fmax-pmcheck-iterations
-fmax-relevant-binds
-fmax-simplifier-iterations
-fmax-worker-args
-fmono-pat-binds
-fmonomorphism-restriction
-fobject-code
-fomit-interface-pragmas
-fomit-yields
-foptimal-applicative-do
-fpackage-trust
-fPArr
-fparr
-fpedantic-bottoms
-fpre-inlining
-fprint-bind-contents
-fprint-bind-result
-fprint-equality-relations
-fprint-evld-with-show
-fprint-expanded-synonyms
-fprint-explicit-coercions
-fprint-explicit-foralls
-fprint-explicit-kinds
-fprint-explicit-runtime-reps
-fprint-potential-instances
-fprint-typechecker-elaboration
-fprint-unicode-syntax
-fprof-cafs
-fprof-count-entries
-framework
-framework-path
-freduction-depth
-fregs-graph
-fregs-iterative
-freverse-errors
-frewrite-rules
-frule-check
-fscoped-type-variables
-fshared-implib
-fshow-warning-groups
-fsimpl-tick-factor
-fsimplifier-phases
-fspec-constr
-fspec-constr-count
-fspec-constr-recursive
-fspec-constr-threshold
-fspecialise
-fspecialise-aggressively
-fstatic-argument-transformation
-fstrictness
-fstrictness-before
-fth
-ftype-function-depth
-funbox-small-strict-fields
-funbox-strict-fields
-funfolding-creation-threshold
-funfolding-dict-discount
-funfolding-fun-discount
-funfolding-keeness-factor
-funfolding-use-threshold
-fuse-rpaths
-fvectorisation-avoidance
-fvectorise
-fversion-macros
-fwarn-
-fworker-wrapper
-fwrite-interface
<!-- #endregion -->

<!-- #region      f NEGATED -->
-fno-allow-incoherent-instances
-fno-allow-overlapping-instances
-fno-allow-undecidable-instances
-fno-arrows
-fno-bang-patterns
-fno-break-on-error
-fno-break-on-exception
-fno-building-cabal-package
-fno-call-arity
-fno-case-merge
-fno-cmm-elim-common-blocks
-fno-cmm-sink
-fno-code
-fno-cpr-anal
-fno-cross-module-specialise
-fno-cse
-fno-defer-out-of-scope-variables
-fno-defer-type-errors
-fno-defer-typed-holes
-fno-dicts-cheap
-fno-dicts-strict
-fno-dmd-tx-dict-sel
-fno-do-eta-reduction
-fno-do-lambda-eta-expansion
-fno-eager-blackholing
-fno-embed-manifest
-fno-enable-rewrite-rules
-fno-error-spans
-fno-excess-precision
-fno-expose-all-unfoldings
-fno-extended-default-rules
-fno-external-interpreter
-fno-ffi
-fno-fi
-fno-flat-cache
-fno-float-in
-fno-force-recomp
-fno-full-laziness
-fno-fun-to-thunk
-fno-gen-manifest
-fno-ghci-history
-fno-ghci-sandbox
-fno-glasgow-exts
-fno-helpful-errors
-fno-hpc
-fno-ignore-asserts
-fno-ignore-interface-pragmas
-fno-implicit-import-qualified
-fno-implicit-params
-fno-implicit-prelude
-fno-irrefutable-tuples
-fno-kill-absence
-fno-kill-one-shot
-fno-late-dmd-anal
-fno-liberate-case
-fno-liberate-case-threshold
-fno-loopification
-fno-max-relevant-binds
-fno-mono-pat-binds
-fno-monomorphism-restriction
-fno-omit-interface-pragmas
-fno-omit-yields
-fno-opt-coercion
-fno-optimal-applicative-do
-fno-PArr
-fno-parr
-fno-pedantic-bottoms
-fno-pre-inlining
-fno-print-bind-contents
-fno-print-bind-result
-fno-print-equality-relations
-fno-print-evld-with-show
-fno-print-expanded-synonyms
-fno-print-explicit-coercions
-fno-print-explicit-foralls
-fno-print-explicit-kinds
-fno-print-explicit-runtime-reps
-fno-print-potential-instances
-fno-print-typechecker-elaboration
-fno-print-unicode-syntax
-fno-prof-cafs
-fno-prof-count-entries
-fno-regs-graph
-fno-regs-iterative
-fno-reverse-errors
-fno-rewrite-rules
-fno-safe-infer
-fno-scoped-type-variables
-fno-shared-implib
-fno-show-warning-groups
-fno-spec-constr
-fno-spec-constr-count
-fno-spec-constr-threshold
-fno-specialise
-fno-specialise-aggressively
-fno-state-hack
-fno-static-argument-transformation
-fno-strictness
-fno-th
-fno-unbox-small-strict-fields
-fno-unbox-strict-fields
-fno-use-rpaths
-fno-vectorisation-avoidance
-fno-vectorise
-fno-version-macros
-fno-warn-
-fno-worker-wrapper
-fno-write-interface

<!-- #endregion -->

---

<!-- #region      W -->
-Wall
-Wall-missed-specialisations
-Walternative-layout-rule-transitional
-Wamp
-Wauto-orphans
-Wcompat
-Wcontext-quantification
-Wdefault
-Wdeferred-out-of-scope-variables
-Wdeferred-type-errors
-Wdeprecated-flags
-Wdeprecations
-Wderiving-typeable
-Wdodgy-exports
-Wdodgy-foreign-imports
-Wdodgy-imports
-Wduplicate-constraints
-Wduplicate-exports
-Wempty-enumerations
-Werror
-Weverything
-Wextra
-Whi-shadowing
-Widentities
-Wimplicit-prelude
-Wincomplete-patterns
-Wincomplete-record-updates
-Wincomplete-uni-patterns
-Winline-rule-shadowing
-Wmissed-specialisations
-Wmissing-exported-signatures
-Wmissing-exported-sigs
-Wmissing-fields
-Wmissing-import-lists
-Wmissing-local-signatures
-Wmissing-local-sigs
-Wmissing-methods
-Wmissing-monadfail-instances
-Wmissing-pattern-synonym-signatures
-Wmissing-signatures
-Wmonomorphism-restriction
-Wname-shadowing
-Wnoncanonical-monad-instances
-Wnoncanonical-monadfail-instances
-Wnoncanonical-monoid-instances
-Wnot
-Worphans
-Woverflowed-literals
-Woverlapping-patterns
-Wpartial-type-signatures
-Wredundant-constraints
-Wsafe
-Wsemigroup
-Wtabs
-Wtrustworthy-safe
-Wtype-defaults
-Wtyped-holes
-Wunrecognised-pragmas
-Wunrecognised-warning-flags
-Wunsafe
-Wunsupported-calling-conventions
-Wunsupported-llvm-version
-Wunticked-promoted-constructors
-Wunused-binds
-Wunused-do-bind
-Wunused-foralls
-Wunused-imports
-Wunused-local-binds
-Wunused-matches
-Wunused-pattern-binds
-Wunused-top-binds
-Wunused-type-patterns
-Wwarn
-Wwarnings-deprecations
-Wwrong-do-bind
<!-- #endregion -->

<!-- #region      W NEGATED -->
-Wno-all
-Wno-all-missed-specialisations
-Wno-alternative-layout-rule-transitional
-Wno-amp
-Wno-auto-orphans
-Wno-compat
-Wno-context-quantification
-Wno-default
-Wno-deferred-out-of-scope-variables
-Wno-deferred-type-errors
-Wno-deprecated-flags
-Wno-deprecations
-Wno-deriving-typeable
-Wno-dodgy-exports
-Wno-dodgy-foreign-imports
-Wno-dodgy-imports
-Wno-duplicate-constraints
-Wno-duplicate-exports
-Wno-empty-enumerations
-Wno-everything
-Wno-extra
-Wno-hi-shadowing
-Wno-identities
-Wno-implicit-prelude
-Wno-incomplete-patterns
-Wno-incomplete-record-updates
-Wno-incomplete-uni-patterns
-Wno-inline-rule-shadowing
-Wno-missed-specialisations
-Wno-missing-exported-signatures
-Wno-missing-exported-sigs
-Wno-missing-fields
-Wno-missing-import-lists
-Wno-missing-local-signatures
-Wno-missing-local-sigs
-Wno-missing-methods
-Wno-missing-monadfail-instances
-Wno-missing-pattern-synonym-signatures
-Wno-missing-signatures
-Wno-monomorphism-restriction
-Wno-name-shadowing
-Wno-noncanonical-monad-instances
-Wno-noncanonical-monadfail-instances
-Wno-noncanonical-monoid-instances
-Wno-orphans
-Wno-overflowed-literals
-Wno-overlapping-patterns
-Wno-partial-type-signatures
-Wno-redundant-constraints
-Wno-safe
-Wno-semigroup
-Wno-tabs
-Wno-trustworthy-safe
-Wno-type-defaults
-Wno-typed-holes
-Wno-unrecognised-pragmas
-Wno-unrecognised-warning-flags
-Wno-unsafe
-Wno-unsupported-calling-conventions
-Wno-unsupported-llvm-version
-Wno-unticked-promoted-constructors
-Wno-unused-binds
-Wno-unused-do-bind
-Wno-unused-foralls
-Wno-unused-imports
-Wno-unused-local-binds
-Wno-unused-matches
-Wno-unused-pattern-binds
-Wno-unused-top-binds
-Wno-unused-type-patterns
-Wno-warnings-deprecations
-Wno-wrong-do-bind
<!-- #endregion -->

---

<!-- #region      X -->
-XAllowAmbiguousTypes
-XAlternativeLayoutRule
-XAlternativeLayoutRuleTransitional
-XApplicativeDo
-XArrows
-XAutoDeriveTypeable
`-XBangPatterns`
-XBinaryLiterals
-XCApiFFI
-XConstrainedClassMethods
-XConstraintKinds
-XCPP
-XDataKinds
-XDatatypeContexts
-XDefaultSignatures
-XDeriveAnyClass
-XDeriveDataTypeable
-XDeriveFoldable
`-XDeriveFunctor`
-XDeriveGeneric
-XDeriveLift
-XDeriveTraversable
-XDisambiguateRecordFields
-XDoAndIfThenElse
-XDoRec
-XDuplicateRecordFields
-XEmptyCase
-XEmptyDataDecls
-XExistentialQuantification
`-XExplicitForAll`
-XExplicitNamespaces
-XExtendedDefaultRules
-XFlexibleContexts
-XFlexibleInstances
-XForeignFunctionInterface
`-XFunctionalDependencies`
-XGADTs
-XGADTSyntax
-XGeneralizedNewtypeDeriving
-XGenerics
-XGHCForeignImportPrim
`-XHaskell2010`
`-XHaskell98`
-XImplicitParams
-XImplicitPrelude
-XImpredicativeTypes
-XIncoherentInstances
-XInstanceSigs
-XInterruptibleFFI
-XJavaScriptFFI
`-XKindSignatures`
-XLambdaCase
-XLiberalTypeSynonyms
-XMagicHash
-XMonadComprehensions
-XMonadFailDesugaring
-XMonoLocalBinds
-XMonomorphismRestriction
-XMonoPatBinds
-XMultiParamTypeClasses
-XMultiWayIf
-XNamedFieldPuns
-XNamedWildCards
`-XNegativeLiterals`
`-XNPlusKPatterns`
`-XNullaryTypeClasses`
-XNumDecimals
-XOverlappingInstances
-XOverloadedLabels
-XOverloadedLists
-XOverloadedStrings
-XPackageImports
-XParallelArrays
-XParallelListComp
-XPartialTypeSignatures
-XPatternGuards
-XPatternSignatures
-XPatternSynonyms
-XPolyKinds
-XPolymorphicComponents
`-XPostfixOperators`
-XQuasiQuotes
-XRank2Types
-XRankNTypes
-XRebindableSyntax
-XRecordPuns
-XRecordWildCards
-XRecursiveDo
-XRelaxedLayout
-XRelaxedPolyRec
-XRoleAnnotations
-XSafe
-XScopedTypeVariables
`-XStandaloneDeriving`
-XStaticPointers
-XStrict
-XStrictData
-XTemplateHaskell
-XTemplateHaskellQuotes
-XTraditionalRecordSyntax
-XTransformListComp
-XTrustworthy
-XTupleSections
-XTypeApplications
-XTypeFamilies
-XTypeFamilyDependencies
-XTypeInType
-XTypeOperators
-XTypeSynonymInstances
-XUnboxedTuples
-XUndecidableInstances
-XUndecidableSuperClasses
`-XUnicodeSyntax`
-XUnliftedFFITypes
-XUnsafe
-XViewPatterns
<!-- #endregion -->

<!-- #region      X NEGATED -->
-XNoAllowAmbiguousTypes
-XNoAlternativeLayoutRule
-XNoAlternativeLayoutRuleTransitional
-XNoApplicativeDo
-XNoArrows
-XNoAutoDeriveTypeable
-XNoBangPatterns
-XNoBinaryLiterals
-XNoCApiFFI
-XNoConstrainedClassMethods
-XNoConstraintKinds
-XNoCPP
-XNoDataKinds
-XNoDatatypeContexts
-XNoDefaultSignatures
-XNoDeriveAnyClass
-XNoDeriveDataTypeable
-XNoDeriveFoldable
-XNoDeriveFunctor
-XNoDeriveGeneric
-XNoDeriveLift
-XNoDeriveTraversable
-XNoDisambiguateRecordFields
-XNoDoAndIfThenElse
-XNoDoRec
-XNoDuplicateRecordFields
-XNoEmptyCase
-XNoEmptyDataDecls
-XNoExistentialQuantification
-XNoExplicitForAll
-XNoExplicitNamespaces
-XNoExtendedDefaultRules
-XNoFlexibleContexts
-XNoFlexibleInstances
-XNoForeignFunctionInterface
-XNoFunctionalDependencies
-XNoGADTs
-XNoGADTSyntax
-XNoGeneralizedNewtypeDeriving
-XNoGenerics
-XNoGHCForeignImportPrim
-XNoImplicitParams
-XNoImplicitPrelude
-XNoImpredicativeTypes
-XNoIncoherentInstances
-XNoInstanceSigs
-XNoInterruptibleFFI
-XNoJavaScriptFFI
-XNoKindSignatures
-XNoLambdaCase
-XNoLiberalTypeSynonyms
-XNoMagicHash
-XNoMonadComprehensions
-XNoMonadFailDesugaring
-XNoMonoLocalBinds
-XNoMonomorphismRestriction
-XNoMonoPatBinds
-XNoMultiParamTypeClasses
-XNoMultiWayIf
-XNoNamedFieldPuns
-XNoNamedWildCards
-XNondecreasingIndentation
-XNoNegativeLiterals
-XNoNondecreasingIndentation
-XNoNPlusKPatterns
-XNoNullaryTypeClasses
-XNoNumDecimals
-XNoOverlappingInstances
-XNoOverloadedLabels
-XNoOverloadedLists
-XNoOverloadedStrings
-XNoPackageImports
-XNoParallelArrays
-XNoParallelListComp
-XNoPartialTypeSignatures
-XNoPatternGuards
-XNoPatternSignatures
-XNoPatternSynonyms
-XNoPolyKinds
-XNoPolymorphicComponents
-XNoPostfixOperators
-XNoQuasiQuotes
-XNoRank2Types
-XNoRankNTypes
-XNoRebindableSyntax
-XNoRecordPuns
-XNoRecordWildCards
-XNoRecursiveDo
-XNoRelaxedLayout
-XNoRelaxedPolyRec
-XNoRoleAnnotations
-XNoScopedTypeVariables
-XNoStandaloneDeriving
-XNoStaticPointers
-XNoStrict
-XNoStrictData
-XNoTemplateHaskell
-XNoTemplateHaskellQuotes
-XNoTraditionalRecordSyntax
-XNoTransformListComp
-XNoTupleSections
-XNoTypeApplications
-XNoTypeFamilies
-XNoTypeFamilyDependencies
-XNoTypeInType
-XNoTypeOperators
-XNoTypeSynonymInstances
-XNoUnboxedTuples
-XNoUndecidableInstances
-XNoUndecidableSuperClasses
-XNoUnicodeSyntax
-XNoUnliftedFFITypes
-XNoViewPatterns
<!-- #endregion -->

---

<!-- #region      LONG OPTIONS -->
--abi-hash
--frontend
--help
--info
--interactive
--make
--numeric-version
--version
--supported-extensions
--supported-languages
--show-iface
--show-options
--show-packages
--print-booter-version
--print-build-platform
--print-c-compiler-flags
--print-c-compiler-link-flags
--print-debug-on
--print-global-package-db
--print-have-interpreter
--print-have-native-code-generator
--print-host-platform
--print-ld-flags
--print-leading-underscore
--print-libdir
--print-object-splitting-supported
--print-project-git-commit-id
--print-project-version
--print-rts-ways
--print-stage
--print-support-smp
--print-tables-next-to-code
--print-target-platform
--print-unregisterised
<!-- #endregion -->

<!-- #region      MISC OPTIONS -->
-?
-#include
-c
-C
-clear-package-db
-cpp
-D

-distrust
-distrust-all-packages

-e
-E
-F
-ghci-script
-global-package-db
-H

-hide-all-packages
-hide-all-plugin-packages
-hide-package

-i
-I
-ignore-dot-ghci
-ignore-package
-interactive-print
-L
-l
-M
-n

-no-global-package-db
-no-ignore-dot-ghci
-no-user-package-conf
-no-user-package-db

-opta
-optc
-optF
-opti
-optL
-optl
-optlc
-optlo
-optP
-optwindres

-package
-package-conf
-package-db
-package-env
-package-id

-pgma
-pgmc
-pgmdll
-pgmF
-pgmi
-pgml
-pgmL
-pgmlc
-pgmlibtool
-pgmlo
-pgmP
-pgms
-pgmwindres

-plugin-package
-plugin-package-id

-Rghc-timing
-S
-sig-of
-syslib
-trust
-user-package-db
-U
-v
-V
-w
-W
-W
<!-- #endregion -->

---
