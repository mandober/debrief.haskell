# GHC ALL Flags

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html


## Flag reference

- Verbosity options
- Alternative modes of operation
- Which phases to run
- Redirecting output
- Keeping intermediate files
- Temporary files
- Finding imports
- Interface file options
- Recompilation checking
- Interactive-mode options
- Packages
- Language options
- Warnings
- Optimisation levels
- Individual optimisations
- Profiling options
- Program coverage options
- C pre-processor options
- Code generation options
- Linking options
- Plugin options
- Replacing phases
- Forcing options to particular phases
- Platform-specific options
- Compiler debugging options
- Miscellaneous compiler options


## Verbosity options

-fabstract-refinement-hole-fits	default: off. Toggles whether refinements where one or more of the holes are abstract are reported.	dynamic	-fno-abstract-refinement-hole-fits
-fdefer-diagnostics	Defer and group diagnostic messages by severity	dynamic	
-fdiagnostics-color=⟨always|auto|never⟩	Use colors in error messages	dynamic	
-fdiagnostics-show-caret	Whether to show snippets of original source code	dynamic	-fno-diagnostics-show-caret
-ferror-spans	Output full span in error messages	dynamic	
-fhide-source-paths	hide module source and object paths	dynamic	
-fkeep-going	Continue compilation as far as possible on errors	dynamic	
-fmax-refinement-hole-fits=⟨n⟩	default: 6. Set the maximum number of refinement hole fits for typed holes to display in type error messages.	dynamic	-fno-max-refinement-hole-fits
-fmax-relevant-binds=⟨n⟩	default: 6. Set the maximum number of bindings to display in type error messages.	dynamic	-fno-max-relevant-binds
-fmax-valid-hole-fits=⟨n⟩	default: 6. Set the maximum number of valid hole fits for typed holes to display in type error messages.	dynamic	-fno-max-valid-hole-fits
-fno-show-valid-hole-fits	Disables showing a list of valid hole fits for typed holes in type error messages.	dynamic	
-fno-sort-valid-hole-fits	Disables the sorting of the list of valid hole fits for typed holes in type error messages.	dynamic	-fsort-valid-hole-fits
-fprint-axiom-incomps	Display equation incompatibilities in closed type families	dynamic	-fno-print-axiom-incomps
-fprint-equality-relations	Distinguish between equality relations when printing	dynamic	-fno-print-equality-relations
-fprint-expanded-synonyms	In type errors, also print type-synonym-expanded types.	dynamic	-fno-print-expanded-synonyms
-fprint-explicit-coercions	Print coercions in types	dynamic	-fno-print-explicit-coercions
-fprint-explicit-foralls	Print explicit forall quantification in types. See also ExplicitForAll	dynamic	-fno-print-explicit-foralls
-fprint-explicit-kinds	Print explicit kind foralls and kind arguments in types. See also KindSignatures	dynamic	-fno-print-explicit-kinds
-fprint-explicit-runtime-reps	Print RuntimeRep variables in types which are runtime-representation polymorphic.	dynamic	-fno-print-explicit-runtime-reps
-fprint-potential-instances	display all available instances in type error messages	dynamic	-fno-print-potential-instances
-fprint-typechecker-elaboration	Print extra information from typechecker.	dynamic	-fno-print-typechecker-elaboration
-fprint-unicode-syntax	Use unicode syntax when printing expressions, types and kinds. See also UnicodeSyntax	dynamic	-fno-print-unicode-syntax
-frefinement-level-hole-fits=⟨n⟩	default: off. Sets the level of refinement of the refinement hole fits, where level n means that hole fits of up to n holes will be considered.	dynamic	-fno-refinement-level-hole-fits
-freverse-errors	Output errors in reverse order	dynamic	-fno-reverse-errors
-fshow-docs-of-hole-fits	Toggles whether to show the documentation of the valid hole fits in the output.	dynamic	-fno-show-docs-of-hole-fits
-fshow-hole-constraints	Show constraints when reporting typed holes.	dynamic	
-fshow-hole-matches-of-hole-fits	Toggles whether to show the type of the additional holes in refinement hole fits.	dynamic	-fno-show-hole-matches-of-hole-fits
-fshow-provenance-of-hole-fits	Toggles whether to show the provenance of the valid hole fits in the output.	dynamic	-fno-show-provenance-of-hole-fits
-fshow-type-app-of-hole-fits	Toggles whether to show the type application of the valid hole fits in the output.	dynamic	-fno-show-type-app-of-hole-fits
-fshow-type-app-vars-of-hole-fits	Toggles whether to show what type each quantified variable takes in a valid hole fit.	dynamic	-fno-show-type-app-vars-of-hole-fits
-fshow-type-of-hole-fits	Toggles whether to show the type of the valid hole fits in the output.	dynamic	-fno-type-of-hole-fits
-funclutter-valid-hole-fits	Unclutter the list of valid hole fits by not showing provenance nor type applications of suggestions.	dynamic	
-Rghc-timing	Summarise timing stats for GHC (same as +RTS -tstderr).	dynamic	
-v	verbose mode (equivalent to -v3)	dynamic	
-v⟨n⟩	set verbosity level	dynamic	


## Alternative modes of operation

--frontend ⟨module⟩	run GHC with the given frontend plugin; see Frontend plugins for details.	mode	
--help, -?	Display help	mode	
--info	display information about the compiler	mode	
--interactive	Interactive mode - normally used by just running ghci; see Using GHCi for details.	mode	
--make	Build a multi-module Haskell program, automatically figuring out dependencies. Likely to be much easier, and faster, than using make; see Using ghc –make for details.	mode	
--mk-dll	DLL-creation mode (Windows only)	mode	
--numeric-version	display GHC version (numeric only)	mode	
--print-libdir	display GHC library directory	mode	
--show-iface ⟨file⟩	display the contents of an interface file.	mode	
--show-options	display the supported command line options	mode	
--supported-extensions, --supported-languages	display the supported language extensions	mode	
--version, -V	display GHC version	mode	
-e ⟨expr⟩	Evaluate expr; see Expression evaluation mode for details.	mode	
-M	generate dependency information suitable for use in a Makefile; see Dependency generation for details.	mode	

## Which phases to run

-C	Stop after generating C (.hc file)	mode	
-c	Stop after generating object (.o) file	mode	
-E	Stop after preprocessing (.hspp file)	mode	
-F	Enable the use of a pre-processor (set with -pgmF ⟨cmd⟩)	dynamic	
-S	Stop after generating assembly (.s file)	mode	
-x ⟨suffix⟩	Override default behaviour for source files	dynamic	

## Redirecting output

--exclude-module=⟨file⟩	Regard ⟨file⟩ as “stable”; i.e., exclude it from having dependencies on it.	dynamic	
-ddump-mod-cycles	Dump module cycles	dynamic	
-dep-makefile ⟨file⟩	Use ⟨file⟩ as the makefile	dynamic	
-dep-suffix ⟨suffix⟩	Make dependencies that declare that files with suffix .⟨suf⟩⟨osuf⟩ depend on interface files with suffix .⟨suf⟩hi	dynamic	
-dumpdir ⟨dir⟩	redirect dump files	dynamic	
-hcsuf ⟨suffix⟩	set the suffix to use for intermediate C files	dynamic	
-hidir ⟨dir⟩	set directory for interface files	dynamic	
-hiedir ⟨dir⟩	set directory for extended interface files	dynamic	
-hiesuf ⟨suffix⟩	set the suffix to use for extended interface files	dynamic	
-hisuf ⟨suffix⟩	set the suffix to use for interface files	dynamic	
-include-cpp-deps	Include preprocessor dependencies	dynamic	
-include-pkg-deps	Regard modules imported from packages as unstable	dynamic	
-o ⟨file⟩	set output filename	dynamic	
-odir ⟨dir⟩	set directory for object files	dynamic	
-ohi ⟨file⟩	set the filename in which to put the interface	dynamic	
-osuf ⟨suffix⟩	set the output file suffix	dynamic	
-outputdir ⟨dir⟩	set output directory	dynamic	
-stubdir ⟨dir⟩	redirect FFI stub files	dynamic	


## Keeping intermediate files

-keep-hc-file, -keep-hc-files	Retain intermediate .hc files.	dynamic	
-keep-hi-files	Retain intermediate .hi files (the default).	dynamic	-no-keep-hi-files
-keep-hscpp-file, -keep-hscpp-files	Retain intermediate .hscpp files.	dynamic	
-keep-llvm-file, -keep-llvm-files	Retain intermediate LLVM .ll files. Implies -fllvm.	dynamic	
-keep-o-files	Retain intermediate .o files (the default).	dynamic	-no-keep-o-files
-keep-s-file, -keep-s-files	Retain intermediate .s files.	dynamic	
-keep-tmp-files	Retain all intermediate temporary files.	dynamic	

## Temporary files

-tmpdir ⟨dir⟩	set the directory for temporary files	dynamic	

## Finding imports

-i	Empty the import directory list	dynamic	
-i⟨dir⟩[:⟨dir⟩]*	add ⟨dir⟩, ⟨dir2⟩, etc. to import path	dynamic	

## Interface file options

--show-iface ⟨file⟩	See Modes of operation.	mode	
-ddump-hi	Dump the new interface to stdout	dynamic	
-ddump-hi-diffs	Show the differences vs. the old interface	dynamic	
-ddump-minimal-imports	Dump a minimal set of imports	dynamic	

## Recompilation checking

-fforce-recomp	Turn off recompilation checking. This is implied by any -ddump-X option when compiling a single file (i.e. when using -c).	dynamic	-fno-force-recomp
-fignore-hpc-changes	Do not recompile modules just to match changes to HPC flags. This is especially useful for avoiding recompilation when using GHCi, and is enabled by default for GHCi.	dynamic	-fno-ignore-hpc-changes
-fignore-optim-changes	Do not recompile modules just to match changes to optimisation flags. This is especially useful for avoiding recompilation when using GHCi, and is enabled by default for GHCi.	dynamic	-fno-ignore-optim-changes

## Interactive-mode options

-fbreak-on-error	Break on uncaught exceptions and errors	dynamic	-fno-break-on-error
-fbreak-on-exception	Break on any exception thrown	dynamic	-fno-break-on-exception
-fghci-hist-size=⟨n⟩	Set the number of entries GHCi keeps for :history. See The GHCi Debugger.	dynamic	
-fghci-leak-check	(Debugging only) check for space leaks when loading new modules in GHCi.	dynamic	-fno-ghci-leak-check
-flocal-ghci-history	Use current directory for the GHCi command history file .ghci-history.	dynamic	-fno-local-ghci-history
-fno-it	No longer set the special variable it.	dynamic	-fno-no-it
-fprint-bind-result	Turn on printing of binding results in GHCi	dynamic	-fno-print-bind-result
-fprint-evld-with-show	Instruct :print to use Show instances where possible.	dynamic	
-fshow-loaded-modules	Show the names of modules that GHCi loaded after a :load command.	dynamic	
-ghci-script	Read additional .ghci files	dynamic	
-ignore-dot-ghci	Disable reading of .ghci files	dynamic	
-interactive-print ⟨name⟩	Select the function to use for printing evaluated expressions in GHCi	dynamic	


## Packages

-clear-package-db	Clear the package db stack.	dynamic	
-distrust ⟨pkg⟩	Expose package ⟨pkg⟩ and set it to be distrusted. See Safe Haskell.	dynamic	
-distrust-all-packages	Distrust all packages by default. See Safe Haskell.	dynamic	
-fpackage-trust	Enable Safe Haskell trusted package requirement for trustworthy modules.	dynamic	
-global-package-db	Add the global package db to the stack.	dynamic	
-hide-all-packages	Hide all packages by default	dynamic	
-hide-package ⟨pkg⟩	Hide package ⟨pkg⟩	dynamic	
-ignore-package ⟨pkg⟩	Ignore package ⟨pkg⟩	dynamic	
-no-auto-link-packages	Don’t automatically link in the base and rts packages.	dynamic	
-no-global-package-db	Remove the global package db from the stack.	dynamic	
-no-user-package-db	Remove the user’s package db from the stack.	dynamic	
-package ⟨pkg⟩	Expose package ⟨pkg⟩	dynamic	
-package-db ⟨file⟩	Add ⟨file⟩ to the package db stack.	dynamic	
-package-env ⟨file⟩|⟨name⟩	Use the specified package environment.	dynamic	
-package-id ⟨unit-id⟩	Expose package by id ⟨unit-id⟩	dynamic	
-this-unit-id ⟨unit-id⟩	Compile to be part of unit (i.e. package) ⟨unit-id⟩	dynamic	
-trust ⟨pkg⟩	Expose package ⟨pkg⟩ and set it to be trusted. See Safe Haskell.	dynamic	
-user-package-db	Add the user’s package db to the stack.	dynamic	



## Warnings

-fdefer-out-of-scope-variables	Convert variable out of scope variables errors into warnings. Implied by -fdefer-type-errors. See also -Wdeferred-out-of-scope-variables.	dynamic	-fno-defer-out-of-scope-variables
-fdefer-type-errors	Turn type errors into warnings, deferring the error until runtime. Implies -fdefer-typed-holes and -fdefer-out-of-scope-variables. See also -Wdeferred-type-errors	dynamic	-fno-defer-type-errors
-fdefer-typed-holes	Convert typed hole errors into warnings, deferring the error until runtime. Implied by -fdefer-type-errors. See also -Wtyped-holes.	dynamic	-fno-defer-typed-holes
-fenable-th-splice-warnings	Generate warnings for Template Haskell splices	dynamic	-fno-enable-th-splices
-fhelpful-errors	Make suggestions for mis-spelled names.	dynamic	-fno-helpful-errors
-fmax-pmcheck-models=⟨n⟩	soft limit on the number of parallel models the pattern match checker should check a pattern match clause against	dynamic	
-fshow-warning-groups	show which group an emitted warning belongs to.	dynamic	-fno-show-warning-groups
-fvia-C	use the C code generator	dynamic	
-W	enable normal warnings	dynamic	-w
-w	disable all warnings	dynamic	
-Wall	enable almost all warnings (details in Warnings and sanity-checking)	dynamic	-w
-Wall-missed-specialisations	warn when specialisation of any overloaded function fails.	dynamic	-Wno-all-missed-specialisations
-Wcompat	enable future compatibility warnings (details in Warnings and sanity-checking)	dynamic	-Wno-compat
-Wcompat-unqualified-imports	Report unqualified imports of core libraries which are expected to cause compatibility problems in future releases.	dynamic	-Wno-compat-unqualified-imports
-Wcpp-undef	warn on uses of the #if directive on undefined identifiers	dynamic	
-Wdeferred-out-of-scope-variables	Report warnings when variable out-of-scope errors are deferred until runtime. See -fdefer-out-of-scope-variables.	dynamic	-Wno-deferred-out-of-scope-variables
-Wdeferred-type-errors	Report warnings when deferred type errors are enabled. This option is enabled by default. See -fdefer-type-errors.	dynamic	-Wno-deferred-type-errors
-Wdeprecated-flags	warn about uses of commandline flags that are deprecated	dynamic	-Wno-deprecated-flags
-Wdeprecations	warn about uses of functions & types that have warnings or deprecated pragmas. Alias for -Wwarnings-deprecations	dynamic	-Wno-deprecations
-Wderiving-defaults	warn about default deriving when using both DeriveAnyClass and GeneralizedNewtypeDeriving	dynamic	-Wno-deriving-defaults
-Wdodgy-exports	warn about dodgy exports	dynamic	-Wno-dodgy-exports
-Wdodgy-foreign-imports	warn about dodgy foreign imports	dynamic	-Wno-dodgy-foreign-import
-Wdodgy-imports	warn about dodgy imports	dynamic	-Wno-dodgy-imports
-Wduplicate-constraints	warn when a constraint appears duplicated in a type signature	dynamic	-Wno-duplicate-constraints
-Wduplicate-exports	warn when an entity is exported multiple times	dynamic	-Wno-duplicate-exports
-Wempty-enumerations	warn about enumerations that are empty	dynamic	-Wno-empty-enumerations
-Werror	make warnings fatal	dynamic	-Wwarn
-Weverything	enable all warnings supported by GHC	dynamic	
-Whi-shadowing	(deprecated) warn when a .hi file in the current directory shadows a library	dynamic	-Wno-hi-shadowing
-Widentities	warn about uses of Prelude numeric conversions that are probably the identity (and hence could be omitted)	dynamic	-Wno-identities
-Wimplicit-kind-vars	warn when kind variables are implicitly quantified over.	dynamic	-Wno-implicit-kind-vars
-Wimplicit-prelude	warn when the Prelude is implicitly imported	dynamic	-Wno-implicit-prelude
-Winaccessible-code	warn about inaccessible code	dynamic	-Wno-inaccessible-code
-Wincomplete-patterns	warn when a pattern match could fail	dynamic	-Wno-incomplete-patterns
-Wincomplete-record-updates	warn when a record update could fail	dynamic	-Wno-incomplete-record-updates
-Wincomplete-uni-patterns	warn when a pattern match in a lambda expression or pattern binding could fail	dynamic	-Wno-incomplete-uni-patterns
-Winline-rule-shadowing	Warn if a rewrite RULE might fail to fire because the function might be inlined before the rule has a chance to fire. See How rules interact with INLINE/NOINLINE pragmas.	dynamic	-Wno-inline-rule-shadowing
-Wmissed-extra-shared-lib	Warn when GHCi can’t load a shared lib.	dynamic	-Wno-missed-extra-shared-lib
-Wmissed-specialisations	warn when specialisation of an imported, overloaded function fails.	dynamic	-Wno-missed-specialisations
-Wmissing-deriving-strategies	warn when a deriving clause is missing a deriving strategy	dynamic	-Wno-missing-deriving-strategies
-Wmissing-export-lists	warn when a module declaration does not explicitly list all exports	dynamic	-fnowarn-missing-export-lists
-Wmissing-exported-signatures	warn about top-level functions without signatures, only if they are exported. takes precedence over -Wmissing-signatures	dynamic	-Wno-missing-exported-signatures
-Wmissing-exported-sigs	(deprecated) warn about top-level functions without signatures, only if they are exported. takes precedence over -Wmissing-signatures	dynamic	-Wno-missing-exported-sigs
-Wmissing-fields	warn when fields of a record are uninitialised	dynamic	-Wno-missing-fields
-Wmissing-home-modules	warn when encountering a home module imported, but not listed on the command line. Useful for cabal to ensure GHC won’t pick up modules, not listed neither in exposed-modules, nor in other-modules.	dynamic	-Wno-missing-home-modules
-Wmissing-import-lists	warn when an import declaration does not explicitly list all the names brought into scope	dynamic	-fnowarn-missing-import-lists
-Wmissing-local-signatures	warn about polymorphic local bindings without signatures	dynamic	-Wno-missing-local-signatures
-Wmissing-local-sigs	(deprecated) warn about polymorphic local bindings without signatures	dynamic	-Wno-missing-local-sigs
-Wmissing-methods	warn when class methods are undefined	dynamic	-Wno-missing-methods
-Wmissing-monadfail-instances	Warn when a failable pattern is used in a do-block that does not have a MonadFail instance.	dynamic	-Wno-missing-monadfail-instances
-Wmissing-pattern-synonym-signatures	warn when pattern synonyms do not have type signatures	dynamic	-Wno-missing-pattern-synonym-signatures
-Wmissing-signatures	warn about top-level functions without signatures	dynamic	-Wno-missing-signatures
-Wmonomorphism-restriction	warn when the Monomorphism Restriction is applied	dynamic	-Wno-monomorphism-restriction
-Wname-shadowing	warn when names are shadowed	dynamic	-Wno-name-shadowing
-Wno-compat	Disables all warnings enabled by -Wcompat.	dynamic	-Wcompat
-Wnoncanonical-monad-instances	warn when Applicative or Monad instances have noncanonical definitions of return, pure, (>>), or (*>). See flag description in Warnings and sanity-checking for more details.	dynamic	-Wno-noncanonical-monad-instances
-Wnoncanonical-monadfail-instances	warn when Monad or MonadFail instances have noncanonical definitions of fail. See flag description in Warnings and sanity-checking for more details.	dynamic	-Wno-noncanonical-monadfail-instances
-Wnoncanonical-monoid-instances	warn when Semigroup or Monoid instances have noncanonical definitions of (<>) or mappend. See flag description in Warnings and sanity-checking for more details.	dynamic	-Wno-noncanonical-monoid-instances
-Worphans	warn when the module contains orphan instance declarations or rewrite rules	dynamic	-Wno-orphans
-Woverflowed-literals	warn about literals that will overflow their type	dynamic	-Wno-overflowed-literals
-Woverlapping-patterns	warn about overlapping patterns	dynamic	-Wno-overlapping-patterns
-Wpartial-fields	warn when defining a partial record field.	dynamic	-Wno-partial-fields
-Wpartial-type-signatures	warn about holes in partial type signatures when PartialTypeSignatures is enabled. Not applicable when PartialTypeSignatures is not enabled, in which case errors are generated for such holes.	dynamic	-Wno-partial-type-signatures
-Wredundant-constraints	Have the compiler warn about redundant constraints in type signatures.	dynamic	-Wno-redundant-constraints
-Wredundant-record-wildcards	Warn about record wildcard matches when the wildcard binds no patterns.	dynamic	-Wno-redundant-record-wildcards
-Wsafe	warn if the module being compiled is regarded to be safe.	dynamic	-Wno-safe
-Wsemigroup	warn when a Monoid is not Semigroup, and on non- Semigroup definitions of (<>)?	dynamic	-Wno-semigroup
-Wsimplifiable-class-constraints	Warn about class constraints in a type signature that can be simplified using a top-level instance declaration.	dynamic	-Wno-simplifiable-class-constraints
-Wspace-after-bang	warn for missing space before the second argument of an infix definition of (!) when BangPatterns are not enabled	dynamic	-Wno-missing-space-after-bang
-Wstar-binder	warn about binding the (*) type operator despite StarIsType	dynamic	-Wno-star-binder
-Wstar-is-type	warn when * is used to mean Data.Kind.Type	dynamic	-Wno-star-is-type
-Wtabs	warn if there are tabs in the source file	dynamic	-Wno-tabs
-Wtrustworthy-safe	warn if the module being compiled is marked as Trustworthy but it could instead be marked as Safe, a more informative bound.	dynamic	-Wno-safe
-Wtype-defaults	warn when defaulting happens	dynamic	-Wno-type-defaults
-Wtyped-holes	Report warnings when typed hole errors are deferred until runtime. See -fdefer-typed-holes.	dynamic	-Wno-typed-holes
-Wunbanged-strict-patterns	warn on pattern bind of unlifted variable that is neither bare nor banged	dynamic	-Wno-unbanged-strict-patterns
-Wunrecognised-pragmas	warn about uses of pragmas that GHC doesn’t recognise	dynamic	-Wno-unrecognised-pragmas
-Wunrecognised-warning-flags	throw a warning when an unrecognised -W... flag is encountered on the command line.	dynamic	-Wno-unrecognised-warning-flags
-Wunsafe	warn if the module being compiled is regarded to be unsafe. See Safe Haskell	dynamic	-Wno-unsafe
-Wunsupported-calling-conventions	warn about use of an unsupported calling convention	dynamic	-Wno-unsupported-calling-conventions
-Wunsupported-llvm-version	Warn when using -fllvm with an unsupported version of LLVM.	dynamic	-Wno-monomorphism-restriction
-Wunticked-promoted-constructors	warn if promoted constructors are not ticked	dynamic	-Wno-unticked-promoted-constructors
-Wunused-binds	warn about bindings that are unused. Alias for -Wunused-top-binds, -Wunused-local-binds and -Wunused-pattern-binds	dynamic	-Wno-unused-binds
-Wunused-do-bind	warn about do bindings that appear to throw away values of types other than ()	dynamic	-Wno-unused-do-bind
-Wunused-foralls	warn about type variables in user-written forall\s that are unused	dynamic	-Wno-unused-foralls
-Wunused-imports	warn about unnecessary imports	dynamic	-Wno-unused-imports
-Wunused-local-binds	warn about local bindings that are unused	dynamic	-Wno-unused-local-binds
-Wunused-matches	warn about variables in patterns that aren’t used	dynamic	-Wno-unused-matches
-Wunused-packages	warn when package is requested on command line, but was never loaded.	dynamic	-Wno-unused-packages
-Wunused-pattern-binds	warn about pattern match bindings that are unused	dynamic	-Wno-unused-pattern-binds
-Wunused-record-wildcards	Warn about record wildcard matches when none of the bound variables are used.	dynamic	-Wno-unused-record-wildcards
-Wunused-top-binds	warn about top-level bindings that are unused	dynamic	-Wno-unused-top-binds
-Wunused-type-patterns	warn about unused type variables which arise from patterns in in type family and data family instances	dynamic	-Wno-unused-type-patterns
-Wwarn	make warnings non-fatal	dynamic	-Werror
-Wwarnings-deprecations	warn about uses of functions & types that have warnings or deprecated pragmas	dynamic	-Wno-warnings-deprecations
-Wwrong-do-bind	warn about do bindings that appear to throw away monadic values that you should have bound instead	dynamic	-Wno-wrong-do-bind

## Optimisation levels

-O, -O1	Enable level 1 optimisations	dynamic	-O0
-O0	Disable optimisations (default)	dynamic	
-O2	Enable level 2 optimisations	dynamic	-O0
-O⟨n⟩	Any -On where n > 2 is the same as -O2.	dynamic	-O0


## Individual optimisations

These options are described in more detail in -f*: platform-independent flags. If a flag is implied by -O then it is also implied by -O2 (unless flag description explicitly says otherwise). If a flag is implied by -O0 only then the flag is not implied by -O and -O2.

-fasm-shortcutting	Enable shortcutting on assembly. Implied by -O2.	dynamic	-fno-asm-shortcutting
-fbinary-blob-threshold=⟨n⟩	default: 500K. Tweak assembly generator for binary blobs.	dynamic	
-fblock-layout-cfg	Use the new cfg based block layout algorithm.	dynamic	-fno-block-layout-cfg
-fblock-layout-weightless	Ignore cfg weights for code layout.	dynamic	-fno-block-layout-weightless
-fblock-layout-weights	Sets edge weights used by the new code layout algorithm.	dynamic	
-fcall-arity	Enable call-arity optimisation. Implied by -O.	dynamic	-fno-call-arity
-fcase-folding	Enable constant folding in case expressions. Implied by -O.	dynamic	-fno-case-folding
-fcase-merge	Enable case-merging. Implied by -O.	dynamic	-fno-case-merge
-fcmm-elim-common-blocks	Enable Cmm common block elimination. Implied by -O.	dynamic	-fno-cmm-elim-common-blocks
-fcmm-sink	Enable Cmm sinking. Implied by -O.	dynamic	-fno-cmm-sink
-fcpr-anal	Turn on CPR analysis in the demand analyser. Implied by -O.	dynamic	-fno-cpr-anal
-fcross-module-specialise	Turn on specialisation of overloaded functions imported from other modules.	dynamic	-fno-cross-module-specialise
-fcse	Enable common sub-expression elimination. Implied by -O.	dynamic	-fno-cse
-fdicts-cheap	Make dictionary-valued expressions seem cheap to the optimiser.	dynamic	-fno-dicts-cheap
-fdicts-strict	Make dictionaries strict	dynamic	-fno-dicts-strict
-fdmd-tx-dict-sel	Use a special demand transformer for dictionary selectors. Always enabled by default.	dynamic	-fno-dmd-tx-dict-sel
-fdo-eta-reduction	Enable eta-reduction. Implied by -O.	dynamic	-fno-do-eta-reduction
-fdo-lambda-eta-expansion	Enable lambda eta-expansion. Always enabled by default.	dynamic	-fno-do-lambda-eta-expansion
-feager-blackholing	Turn on eager blackholing	dynamic	
-fenable-rewrite-rules	Switch on all rewrite rules (including rules generated by automatic specialisation of overloaded functions). Implied by -O.	dynamic	-fno-enable-rewrite-rules
-fexcess-precision	Enable excess intermediate precision	dynamic	-fno-excess-precision
-fexitification	Enables exitification optimisation. Implied by -O.	dynamic	-fno-exitification
-fexpose-all-unfoldings	Expose all unfoldings, even for very large or recursive functions.	dynamic	-fno-expose-all-unfoldings
-ffloat-in	Turn on the float-in transformation. Implied by -O.	dynamic	-fno-float-in
-ffull-laziness	Turn on full laziness (floating bindings outwards). Implied by -O.	dynamic	-fno-full-laziness
-ffun-to-thunk	Allow worker-wrapper to convert a function closure into a thunk if the function does not use any of its arguments. Off by default.	dynamic	-fno-fun-to-thunk
-fignore-asserts	Ignore assertions in the source. Implied by -O.	dynamic	-fno-ignore-asserts
-fignore-interface-pragmas	Ignore pragmas in interface files. Implied by -O0 only.	dynamic	-fno-ignore-interface-pragmas
-flate-dmd-anal	Run demand analysis again, at the end of the simplification pipeline	dynamic	-fno-late-dmd-anal
-flate-specialise	Run a late specialisation pass	dynamic	-fno-late-specialise
-fliberate-case	Turn on the liberate-case transformation. Implied by -O2.	dynamic	-fno-liberate-case
-fliberate-case-threshold=⟨n⟩	default: 2000. Set the size threshold for the liberate-case transformation to ⟨n⟩	dynamic	-fno-liberate-case-threshold
-fllvm-pass-vectors-in-regs	Pass vector value in vector registers for function calls	dynamic	-fno-llvm-pass-vectors-in-regs
-floopification	Turn saturated self-recursive tail-calls into local jumps in the generated assembly. Implied by -O.	dynamic	-fno-loopification
-fmax-inline-alloc-size=⟨n⟩	default: 128. Set the maximum size of inline array allocations to ⟨n⟩ bytes (default: 128).	dynamic	
-fmax-inline-memcpy-insns=⟨n⟩	default: 32. Inline memcpy calls if they would generate no more than ⟨n⟩ pseudo instructions.	dynamic	
-fmax-inline-memset-insns=⟨n⟩	default: 32. Inline memset calls if they would generate no more than ⟨n⟩ pseudo instructions	dynamic	
-fmax-simplifier-iterations=⟨n⟩	default: 4. Set the max iterations for the simplifier.	dynamic	
-fmax-uncovered-patterns=⟨n⟩	default: 4. Set the maximum number of patterns to display in warnings about non-exhaustive ones.	dynamic	
-fmax-worker-args=⟨n⟩	default: 10. If a worker has that many arguments, none will be unpacked anymore.	dynamic	
-fno-opt-coercion	Turn off the coercion optimiser	dynamic	
-fno-pre-inlining	Turn off pre-inlining	dynamic	
-fno-state-hack	Turn off the state hackwhereby any lambda with a real-world state token as argument is considered to be single-entry. Hence OK to inline things inside it.	dynamic	
-fomit-interface-pragmas	Don’t generate interface pragmas. Implied by -O0 only.	dynamic	-fno-omit-interface-pragmas
-fomit-yields	Omit heap checks when no allocation is being performed.	dynamic	-fno-omit-yields
-foptimal-applicative-do	Use a slower but better algorithm for ApplicativeDo	dynamic	-fno-optimal-applicative-do
-fpedantic-bottoms	Make GHC be more precise about its treatment of bottom (but see also -fno-state-hack). In particular, GHC will not eta-expand through a case expression.	dynamic	-fno-pedantic-bottoms
-fregs-graph	Use the graph colouring register allocator for register allocation in the native code generator. Implied by -O2.	dynamic	-fno-regs-graph
-fregs-iterative	Use the iterative coalescing graph colouring register allocator in the native code generator.	dynamic	-fno-regs-iterative
-fsimpl-tick-factor=⟨n⟩	default: 100. Set the percentage factor for simplifier ticks.	dynamic	
-fsimplifier-phases=⟨n⟩	default: 2. Set the number of phases for the simplifier. Ignored with -O0.	dynamic	
-fsolve-constant-dicts	When solving constraints, try to eagerly solve super classes using available dictionaries.	dynamic	-fno-solve-constant-dicts
-fspec-constr	Turn on the SpecConstr transformation. Implied by -O2.	dynamic	-fno-spec-constr
-fspec-constr-count=⟨n⟩	default: 3.* Set to ⟨n⟩ the maximum number of specialisations that will be created for any one function by the SpecConstr transformation.	dynamic	-fno-spec-constr-count
-fspec-constr-keen	Specialize a call with an explicit constructor argument, even if the argument is not scrutinised in the body of the function	dynamic	-fno-spec-constr-keen
-fspec-constr-threshold=⟨n⟩	default: 2000. Set the size threshold for the SpecConstr transformation to ⟨n⟩.	dynamic	-fno-spec-constr-threshold
-fspecialise	Turn on specialisation of overloaded functions. Implied by -O.	dynamic	-fno-specialise
-fspecialise-aggressively	Turn on specialisation of overloaded functions regardless of size, if unfolding is available	dynamic	-fno-specialise-aggressively
-fstatic-argument-transformation	Turn on the static argument transformation.	dynamic	-fno-static-argument-transformation
-fstg-cse	Enable common sub-expression elimination on the STG intermediate language	dynamic	-fno-stg-cse
-fstg-lift-lams	Enable late lambda lifting on the STG intermediate language. Implied by -O2.	dynamic	-fno-stg-lift-lams
-fstg-lift-lams-known	Allow turning known into unknown calls while performing late lambda lifting.	dynamic	-fno-stg-lift-lams-known
-fstg-lift-lams-non-rec-args	Create top-level non-recursive functions with at most <n> parameters while performing late lambda lifting.	dynamic	-fno-stg-lift-lams-non-rec-args-any
-fstg-lift-lams-rec-args	Create top-level recursive functions with at most <n> parameters while performing late lambda lifting.	dynamic	-fno-stg-lift-lams-rec-args-any
-fstrictness	Turn on strictness analysis. Implied by -O. Implies -fworker-wrapper	dynamic	-fno-strictness
-fstrictness-before=⟨n⟩	Run an additional strictness analysis before simplifier phase ⟨n⟩	dynamic	
-funbox-small-strict-fields	Flatten strict constructor fields with a pointer-sized representation. Implied by -O.	dynamic	-fno-unbox-small-strict-fields
-funbox-strict-fields	Flatten strict constructor fields	dynamic	-fno-unbox-strict-fields
-funfolding-creation-threshold=⟨n⟩	default: 750. Tweak unfolding settings.	dynamic	
-funfolding-dict-discount=⟨n⟩	default: 30. Tweak unfolding settings.	dynamic	
-funfolding-fun-discount=⟨n⟩	default: 60. Tweak unfolding settings.	dynamic	
-funfolding-keeness-factor=⟨n⟩	default: 1.5. Tweak unfolding settings.	dynamic	
-funfolding-use-threshold=⟨n⟩	default: 60. Tweak unfolding settings.	dynamic	
-fworker-wrapper	Enable the worker-wrapper transformation.	dynamic	

## Profiling options

-fno-prof-auto	Disables any previous -fprof-auto, -fprof-auto-top, or -fprof-auto-exported options.	dynamic	-fprof-auto
-fno-prof-cafs	Disables any previous -fprof-cafs option.	dynamic	-fprof-cafs
-fno-prof-count-entries	Do not collect entry counts	dynamic	-fprof-count-entries
-fprof-auto	Auto-add SCC\ s to all bindings not marked INLINE	dynamic	-fno-prof-auto
-fprof-auto-calls	Auto-add SCC\ s to all call sites	dynamic	-fno-prof-auto-calls
-fprof-auto-exported	Auto-add SCC\ s to all exported bindings not marked INLINE	dynamic	-fno-prof-auto
-fprof-auto-top	Auto-add SCC\ s to all top-level bindings not marked INLINE	dynamic	-fno-prof-auto
-fprof-cafs	Auto-add SCC\ s to all CAFs	dynamic	-fno-prof-cafs
-prof	Turn on profiling	dynamic	
-ticky	Turn on ticky-ticky profiling	dynamic	

## Program coverage options

-fhpc	Turn on Haskell program coverage instrumentation	dynamic	

## C pre-processor options

-cpp	Run the C pre-processor on Haskell source files	dynamic	
-D⟨symbol⟩[=⟨value⟩]	Define a symbol in the C pre-processor	dynamic	-U⟨symbol⟩
-I⟨dir⟩	Add ⟨dir⟩ to the directory search list for #include files	dynamic	
-U⟨symbol⟩	Undefine a symbol in the C pre-processor	dynamic	

## Code generation options

-dynamic-too	Build dynamic object files as well as static object files during compilation	dynamic	
-fasm	Use the native code generator	dynamic	-fllvm
-fbyte-code	Generate byte-code	dynamic	
-fexternal-dynamic-refs	Generate code for linking against dynamic libraries	dynamic	
-fllvm	Compile using the LLVM code generator	dynamic	-fasm
-fno-code	Omit code generation	dynamic	
-fobject-code	Generate object code	dynamic	
-fPIC	Generate position-independent code (where available)	dynamic	
-fPIE	Generate code for a position-independent executable (where available)	dynamic	
-fwrite-interface	Always write interface files	dynamic	

## Linking options

-c	Stop after generating object (.o) file	mode	
-debug	Use the debugging runtime	dynamic	
-dylib-install-name ⟨path⟩	Set the install name (via -install_name passed to Apple’s linker), specifying the full install path of the library file. Any libraries or executables that link with it later will pick up that path as their runtime search location for it. (Darwin/OS X only)	dynamic	
-dynamic	Build dynamically-linked object files and executables	dynamic	
-dynload	Selects one of a number of modes for finding shared libraries at runtime.	dynamic	
-eventlog	Enable runtime event tracing	dynamic	
-fno-embed-manifest	Do not embed the manifest in the executable (Windows only)	dynamic	
-fno-gen-manifest	Do not generate a manifest file (Windows only)	dynamic	
-fno-shared-implib	Don’t generate an import library for a DLL (Windows only)	dynamic	
-framework ⟨name⟩	On Darwin/OS X/iOS only, link in the framework ⟨name⟩. This option corresponds to the -framework option for Apple’s Linker.	dynamic	
-framework-path ⟨dir⟩	On Darwin/OS X/iOS only, add ⟨dir⟩ to the list of directories searched for frameworks. This option corresponds to the -F option for Apple’s Linker.	dynamic	
-fwhole-archive-hs-libs	When linking a binary executable, this inserts the flag -Wl,--whole-archive before any -l flags for Haskell libraries, and -Wl,--no-whole-archive afterwards	dynamic	
-keep-cafs	Do not garbage-collect CAFs (top-level expressions) at runtime	dynamic	
-L ⟨dir⟩	Add ⟨dir⟩ to the list of directories searched for libraries	dynamic	
-l ⟨lib⟩	Link in library ⟨lib⟩	dynamic	
-main-is ⟨thing⟩	Set main module and function	dynamic	
-no-hs-main	Don’t assume this program contains main	dynamic	
-no-rtsopts-suggestions	Don’t print RTS suggestions about linking with -rtsopts[=⟨none|some|all|ignore|ignoreAll⟩].	dynamic	
-package ⟨name⟩	Expose package ⟨pkg⟩	dynamic	
-pie	Instruct the linker to produce a position-independent executable.	dynamic	
-rdynamic	This instructs the linker to add all symbols, not only used ones, to the dynamic symbol table. Currently Linux and Windows/MinGW32 only. This is equivalent to using -optl -rdynamic on Linux, and -optl -export-all-symbols on Windows.	dynamic	
-rtsopts[=⟨none|some|all|ignore|ignoreAll⟩]	Control whether the RTS behaviour can be tweaked via command-line flags and the GHCRTS environment variable. Using none means no RTS flags can be given; some means only a minimum of safe options can be given (the default); all (or no argument at all) means that all RTS flags are permitted; ignore means RTS flags can be given, but are treated as regular arguments and passed to the Haskell program as arguments; ignoreAll is the same as ignore, but GHCRTS is also ignored. -rtsopts does not affect -with-rtsopts behavior; flags passed via -with-rtsopts are used regardless of -rtsopts.	dynamic	
-shared	Generate a shared library (as opposed to an executable)	dynamic	
-split-sections	Split sections for link-time dead-code stripping	dynamic	
-static	Use static Haskell libraries	dynamic	
-staticlib	Generate a standalone static library (as opposed to an executable). This is useful when cross compiling. The library together with all its dependencies ends up in in a single static library that can be linked against.	dynamic	
-threaded	Use the threaded runtime	dynamic	
-with-rtsopts=⟨opts⟩	Set the default RTS options to ⟨opts⟩.	dynamic	

## Plugin options

-fclear-plugins	Clear the list of active plugins	dynamic	
-fplugin-opt=⟨module⟩:⟨args⟩	Give arguments to a plugin module; module must be specified with -fplugin=⟨module⟩	dynamic	
-fplugin-trustworthy	Trust the used plugins and no longer mark the compiled module as unsafe	dynamic	
-fplugin=⟨module⟩	Load a plugin exported by a given module	dynamic	
-hide-all-plugin-packages	Hide all packages for plugins by default	dynamic	
-plugin-package ⟨pkg⟩	Expose ⟨pkg⟩ for plugins	dynamic	
-plugin-package-id ⟨pkg-id⟩	Expose ⟨pkg-id⟩ for plugins	dynamic	

## Replacing phases

-pgma ⟨cmd⟩	Use ⟨cmd⟩ as the assembler	dynamic	
-pgmc ⟨cmd⟩	Use ⟨cmd⟩ as the C compiler	dynamic	
-pgmdll ⟨cmd⟩	Use ⟨cmd⟩ as the DLL generator	dynamic	
-pgmF ⟨cmd⟩	Use ⟨cmd⟩ as the pre-processor (with -F only)	dynamic	
-pgmi ⟨cmd⟩	Use ⟨cmd⟩ as the external interpreter command.	dynamic	
-pgmL ⟨cmd⟩	Use ⟨cmd⟩ as the literate pre-processor	dynamic	
-pgml ⟨cmd⟩	Use ⟨cmd⟩ as the linker	dynamic	
-pgmlc ⟨cmd⟩	Use ⟨cmd⟩ as the LLVM compiler	dynamic	
-pgmlibtool ⟨cmd⟩	Use ⟨cmd⟩ as the command for libtool (with -staticlib only).	dynamic	
-pgmlo ⟨cmd⟩	Use ⟨cmd⟩ as the LLVM optimiser	dynamic	
-pgmP ⟨cmd⟩	Use ⟨cmd⟩ as the C pre-processor (with -cpp only)	dynamic	
-pgms ⟨cmd⟩	Use ⟨cmd⟩ as the splitter	dynamic	
-pgmwindres ⟨cmd⟩	Use ⟨cmd⟩ as the program for embedding manifests on Windows.	dynamic	

## Forcing options to particular phases

-opta ⟨option⟩	pass ⟨option⟩ to the assembler	dynamic	
-optc ⟨option⟩	pass ⟨option⟩ to the C compiler	dynamic	
-optcxx ⟨option⟩	pass ⟨option⟩ to the C++ compiler	dynamic	
-optdll ⟨option⟩	pass ⟨option⟩ to the DLL generator	dynamic	
-optF ⟨option⟩	pass ⟨option⟩ to the custom pre-processor	dynamic	
-opti ⟨option⟩	pass ⟨option⟩ to the interpreter sub-process.	dynamic	
-optL ⟨option⟩	pass ⟨option⟩ to the literate pre-processor	dynamic	
-optl ⟨option⟩	pass ⟨option⟩ to the linker	dynamic	
-optlc ⟨option⟩	pass ⟨option⟩ to the LLVM compiler	dynamic	
-optlo ⟨option⟩	pass ⟨option⟩ to the LLVM optimiser	dynamic	
-optP ⟨option⟩	pass ⟨option⟩ to cpp (with -cpp only)	dynamic	
-optwindres ⟨option⟩	pass ⟨option⟩ to windres.	dynamic	

## Platform-specific options

-mbmi2	(x86 only) Use BMI2 for bit manipulation operations	dynamic	
-msse2	(x86 only) Use SSE2 for floating-point operations	dynamic	
-msse4.2	(x86 only) Use SSE4.2 for floating-point operations	dynamic	

## Compiler debugging options

-dcmm-lint	C-\- pass sanity checking	dynamic	
-dcore-lint	Turn on internal sanity checking	dynamic	
-ddump-asm	Dump final assembly	dynamic	
-ddump-asm-expanded	Dump the result of the synthetic instruction expansion pass.	dynamic	
-ddump-asm-liveness	Dump assembly augmented with register liveness	dynamic	
-ddump-asm-native	Dump initial assembly	dynamic	
-ddump-asm-regalloc	Dump the result of register allocation	dynamic	
-ddump-asm-regalloc-stages	Dump the build/spill stages of the -fregs-graph register allocator.	dynamic	
-ddump-asm-stats	Dump statistics from the register allocator.	dynamic	
-ddump-bcos	Dump interpreter byte code	dynamic	
-ddump-cfg-weights	Dump the assumed weights of the CFG.	dynamic	
-ddump-cmm	Dump the final C-\- output	dynamic	
-ddump-cmm-caf	Dump the results of the C-\- CAF analysis pass.	dynamic	
-ddump-cmm-cbe	Dump the results of common block elimination	dynamic	
-ddump-cmm-cfg	Dump the results of the C-\- control flow optimisation pass.	dynamic	
-ddump-cmm-cps	Dump the results of the CPS pass	dynamic	
-ddump-cmm-from-stg	Dump STG-to-C-\- output	dynamic	
-ddump-cmm-info	Dump the results of the C-\- info table augmentation pass.	dynamic	
-ddump-cmm-proc	Dump the results of proc-point analysis	dynamic	
-ddump-cmm-procmap	Dump the results of the C-\- proc-point map pass.	dynamic	
-ddump-cmm-raw	Dump raw C-\-	dynamic	
-ddump-cmm-sink	Dump the results of the C-\- sinking pass.	dynamic	
-ddump-cmm-sp	Dump the results of the C-\- stack layout pass.	dynamic	
-ddump-cmm-split	Dump the results of the C-\- proc-point splitting pass.	dynamic	
-ddump-cmm-switch	Dump the results of switch lowering passes	dynamic	
-ddump-cmm-verbose	Write output from main C-\- pipeline passes to files	dynamic	
-ddump-cmm-verbose-by-proc	Show output from main C-\- pipeline passes (grouped by proc)	dynamic	
-ddump-core-stats	Print a one-line summary of the size of the Core program at the end of the optimisation pipeline	dynamic	
-ddump-cse	Dump CSE output	dynamic	
-ddump-deriv	Dump deriving output	dynamic	
-ddump-ds, -ddump-ds-preopt	Dump desugarer output.	dynamic	
-ddump-ec-trace	Trace exhaustiveness checker	dynamic	
-ddump-file-prefix=⟨str⟩	Set the prefix of the filenames used for debugging output.	dynamic	
-ddump-foreign	Dump foreign export stubs	dynamic	
-ddump-hpc	An alias for -ddump-ticked.	dynamic	
-ddump-if-trace	Trace interface files	dynamic	
-ddump-inlinings	Dump inlining info	dynamic	
-ddump-json	Dump error messages as JSON documents	dynamic	
-ddump-llvm	Dump LLVM intermediate code.	dynamic	
-ddump-mod-map	Dump the state of the module mapping database.	dynamic	
-ddump-occur-anal	Dump occurrence analysis output	dynamic	
-ddump-opt-cmm	Dump the results of C-\- to C-\- optimising passes	dynamic	
-ddump-parsed	Dump parse tree	dynamic	
-ddump-parsed-ast	Dump parser output as a syntax tree	dynamic	
-ddump-prep	Dump prepared core	dynamic	
-ddump-rn	Dump renamer output	dynamic	
-ddump-rn-ast	Dump renamer output as a syntax tree	dynamic	
-ddump-rn-stats	Renamer stats	dynamic	
-ddump-rn-trace	Trace renamer	dynamic	
-ddump-rtti	Trace runtime type inference	dynamic	
-ddump-rule-firings	Dump rule firing info	dynamic	
-ddump-rule-rewrites	Dump detailed rule firing info	dynamic	
-ddump-rules	Dump rewrite rules	dynamic	
-ddump-simpl	Dump final simplifier output	dynamic	
-ddump-simpl-iterations	Dump output from each simplifier iteration	dynamic	
-ddump-simpl-stats	Dump simplifier stats	dynamic	
-ddump-spec	Dump specialiser output	dynamic	
-ddump-splices	Dump TH spliced expressions, and what they evaluate to	dynamic	
-ddump-stg	Show CoreToStg output	dynamic	
-ddump-stg-final	Show output of last STG pass.	dynamic	
-ddump-stg-unarised	Show unarised STG	dynamic	
-ddump-str-signatures	Dump strictness signatures	dynamic	
-ddump-stranal	Dump strictness analyser output	dynamic	
-ddump-tc	Dump typechecker output	dynamic	
-ddump-tc-ast	Dump typechecker output as a syntax tree	dynamic	
-ddump-tc-trace	Trace typechecker	dynamic	
-ddump-ticked	Dump the code instrumented by HPC (Observing Code Coverage).	dynamic	
-ddump-timings	Dump per-pass timing and allocation statistics	dynamic	
-ddump-to-file	Dump to files instead of stdout	dynamic	
-ddump-types	Dump type signatures	dynamic	
-ddump-worker-wrapper	Dump worker-wrapper output	dynamic	
-dfaststring-stats	Show statistics for fast string usage when finished	dynamic	
-dhex-word-literals	Print values of type Word# in hexadecimal.	dynamic	
-dinitial-unique=⟨s⟩	Start UniqSupply allocation from ⟨s⟩.	dynamic	
-dinline-check=⟨str⟩	Dump information about inlining decisions	dynamic	
-dno-debug-output	Suppress unsolicited debugging output	dynamic	-ddebug-output
-dno-typeable-binds	Don’t generate bindings for Typeable methods	dynamic	
-dppr-case-as-let	Print single alternative case expressions as strict lets.	dynamic	
-dppr-cols=⟨n⟩	Set the width of debugging output. For example -dppr-cols200	dynamic	
-dppr-debug	Turn on debug printing (more verbose)	dynamic	
-dppr-user-length	Set the depth for printing expressions in error msgs	dynamic	
-drule-check=⟨str⟩	Dump information about potential rule application	dynamic	
-dshow-passes	Print out each pass name as it happens	dynamic	
-dstg-lint	STG pass sanity checking	dynamic	
-dsuppress-all	In dumps, suppress everything (except for uniques) that is suppressible.	dynamic	
-dsuppress-coercions	Suppress the printing of coercions in Core dumps to make them shorter	dynamic	
-dsuppress-idinfo	Suppress extended information about identifiers where they are bound	dynamic	
-dsuppress-module-prefixes	Suppress the printing of module qualification prefixes	dynamic	
-dsuppress-stg-free-vars	Suppress the printing of closure free variable lists in STG output	dynamic	
-dsuppress-ticks	Suppress “ticks” in the pretty-printer output.	dynamic	
-dsuppress-timestamps	Suppress timestamps in dumps	dynamic	
-dsuppress-type-applications	Suppress type applications	dynamic	
-dsuppress-type-signatures	Suppress type signatures	dynamic	
-dsuppress-unfoldings	Suppress the printing of the stable unfolding of a variable at its binding site	dynamic	
-dsuppress-uniques	Suppress the printing of uniques in debug output (easier to use diff)	dynamic	
-dsuppress-var-kinds	Suppress the printing of variable kinds	dynamic	
-dth-dec-file	Dump evaluated TH declarations into *.th.hs files	dynamic	
-dunique-increment=⟨i⟩	Set the increment for the generated Unique‘s to ⟨i⟩.	dynamic	
-dverbose-core2core	Show output from each core-to-core pass	dynamic	
-dverbose-stg2stg	Show output from each STG-to-STG pass	dynamic	
-falignment-sanitisation	Compile with alignment checks for all info table dereferences.	dynamic	
-fcatch-bottoms	Insert error expressions after bottoming expressions; useful when debugging the compiler.	dynamic	
-fllvm-fill-undef-with-garbage	Intruct LLVM to fill dead STG registers with garbage	dynamic	
-fproc-alignment	Align functions at given boundary.	dynamic	
-g, -g⟨n⟩	Produce DWARF debug information in compiled object files. ⟨n⟩ can be 0, 1, or 2, with higher numbers producing richer output. If ⟨n⟩ is omitted, level 2 is assumed.	dynamic	


## Miscellaneous compiler options

-fexternal-interpreter	Run interpreted code in a separate process	dynamic	
-fglasgow-exts	Deprecated. Enable most language extensions; see Language options for exactly which ones.	dynamic	-fno-glasgow-exts
-ghcversion-file ⟨path to ghcversion.h⟩	(GHC as a C compiler only) Use this ghcversion.h file	dynamic	
-H ⟨size⟩	Set the minimum size of the heap to ⟨size⟩	dynamic	
-j[⟨n⟩]	When compiling with --make, compile ⟨n⟩ modules in parallel.	dynamic	
