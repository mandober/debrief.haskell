# GHC Flags

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/flags.html


## Fav Flags

`-fprint-explicit-foralls` Print explicit forall quantification in types




## Flag reference

- Finding imports
- Interactive-mode options
- Packages
- Verbosity
- Warnings


## Dynamic and Mode options
Each of GHC command line options is classified as dynamic or mode:

**Mode**: may be used on the command line only.
You can pass only one mode flag. For example, --make or -E.

**Dynamic**: may be used on the command line, in a `OPTIONS_GHC` pragma, or with :set in GHCi.

All flags are dynamic unless stated otherwise.


## Interactive-mode options
-interactive-print FN   Function for printing evaluated expressions in GHCi
-ignore-dot-ghci        Disable reading of .ghci files
-ghci-script            Read additional .ghci files
-fghci-hist-size=N      Set the number of GHCi :history entries
-flocal-ghci-history    Use cwd for .ghci-history
-fno-it                 No longer set the special variable `it`
-fbreak-on-error        Break on uncaught exceptions and errors
-fbreak-on-exception    Break on any exception thrown
-fprint-bind-result     Turn on printing of binding results in GHC
-fprint-evld-with-show  Instruct :print to use Show instances if possible
-fshow-loaded-modules   Show names of modules GHCi loaded w :load
-fghci-leak-check       Check space leaks on module load in GHCi (debugging)


## Finding imports
-i                     Empty the import directory list
-i⟨dir⟩[:⟨dir⟩]*         add ⟨dir⟩, ⟨dir2⟩, etc. to import path


## Packages
-no-global-package-db  Remove the global package db from the stack
   -global-package-db  Add the global package db to the stack
  -no-user-package-db  Remove the user package db from the stack
     -user-package-db  Add user package db to the stack
    -clear-package-db  Clear the package db stack
          -package-db  ⟨file⟩  Add ⟨file⟩ to the package db stack

  -hide-package ⟨pkg⟩    Hide package ⟨pkg⟩
-ignore-package ⟨pkg⟩    Ignore package ⟨pkg⟩
       -package ⟨pkg⟩    Expose package ⟨pkg⟩

-distrust-all-packages  Distrust all packages by default
    -hide-all-packages  Hide all packages by default
-no-auto-link-packages  Don't automatically link in the base and rts packages

   -trust ⟨pkg⟩          Expose package ⟨pkg⟩ and set it to be trusted
-distrust ⟨pkg⟩          Expose package ⟨pkg⟩ and set it to be distrusted
-fpackage-trust         Enable Safe Haskell trusted modules
  -package-id ⟨unit-id⟩  Expose package by id ⟨unit-id⟩
-this-unit-id ⟨unit-id⟩  Compile to be part of unit (i.e. package) ⟨unit-id⟩
-package-env ⟨file⟩|⟨name⟩    Use specified package env



## Verbosity options

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/using.html#verbosity-options

-fabstract-refinement-hole-fits
Toggles whether refinements where one or more of the holes are abstract 
are reported. default: off

-fdefer-diagnostics
Defer and group diagnostic messages by severity

`-fdiagnostics-color=⟨always|auto|never⟩`
Use colors in error messages

-fdiagnostics-show-caret
Whether to show snippets of original source code

`-ferror-spans`
Output full span in error messages

-fhide-source-paths
hide module source and object paths

-fkeep-going
Continue compilation as far as possible on errors


-fmax-refinement-hole-fits=⟨n⟩
Set the maximum number of refinement hole fits for typed holes to display 
in type error messages. default: 6

-fmax-relevant-binds=⟨n⟩
Set the maximum number of bindings to display 
in type error messages. default: 6

-fmax-valid-hole-fits=⟨n⟩
Set the maximum number of valid hole fits for typed holes to display 
in type error messages. default: 6

-fno-show-valid-hole-fits
Disables showing a list of valid hole fits for typed holes 
in type error messages.

-fno-sort-valid-hole-fits
Disables sorting of list of valid hole fits for typed holes 
in type error messages


-fprint-axiom-incomps
Display equation incompatibilities in closed type families

-fprint-equality-relations
Distinguish between equality relations when printing

`-fprint-expanded-synonyms`
In type errors, also print type-synonym-expanded types

`-fprint-explicit-coercions`
Print coercions in type

`-fprint-explicit-foralls`
Print explicit forall quantification in types

`-fprint-explicit-kinds`
Print explicit kind foralls and kind arguments in types

`-fprint-explicit-runtime-reps`
Print RuntimeRep vars in types which are runtime-representation polymorphic

-fprint-potential-instances
display all available instances in type error messages

-fprint-typechecker-elaboration
Print extra information from typechecker

`-fprint-unicode-syntax`
Use unicode syntax when printing expressions, types and kinds

`-freverse-errors`
Output errors in reverse order-fno-reverse-errors


-frefinement-level-hole-fits=⟨n⟩
Sets the level of refinement of the refinement hole fits, where level n means 
that hole fits of up to n holes will be considered. default: off

-fshow-docs-of-hole-fits
Toggles whether to show the doc of the valid hole fits in the output

-fshow-hole-constraints
Show constraints when reporting typed holes.

-fshow-hole-matches-of-hole-fits
Toggles whether to show the type of the additional holes in refinement hole fits

-fshow-provenance-of-hole-fits
Toggles whether to show the provenance of the valid hole fits in the output

-fshow-type-app-of-hole-fits
Toggles whether to show the type application of the valid hole fits in the output

-fshow-type-app-vars-of-hole-fits
Toggles whether to show what type each quantified var takes in a valid hole fit

-fshow-type-of-hole-fits
Toggles whether to show the type of the valid hole fits in the output.

-funclutter-valid-hole-fits
Unclutter the list of valid hole fits by not showing
provenance nor type applications of suggestions.

-Rghc-timing    Summarise timing stats for GHC (same as +RTS -tstderr)
-vverbose       mode (equivalent to -v3)
-v⟨n⟩set        verbosity level



## Warnings

-fdefer-out-of-scope-variables
Convert variable out of scope variables errors into warnings.
Implied by -fdefer-type-errors.
See also -Wdeferred-out-of-scope-variables

-fdefer-type-errors
Turn type errors into warnings, deferring the error until runtime.
Implies -fdefer-typed-holes and -fdefer-out-of-scope-variables.
See also -Wdeferred-type-errors

-fdefer-typed-holes
Convert typed hole errors into warnings, deferring the error until runtime.
Implied by -fdefer-type-errors.
See also -Wtyped-holes

-fenable-th-splice-warnings
Generate warnings for Template Haskell splice

`-fhelpful-errors`
Make suggestions for misspelled names

-fmax-pmcheck-models⟨n⟩
soft limit on the number of parallel models the pattern match
checker should check a pattern match clause against

-fshow-warning-groups
show which group an emitted warning belongs to

-fvia-C
use the C code generator

-Wenable
normal warnings-w

-wdisable
all warnings

-Wallenable
almost all warnings (details in Warnings and sanity-checking-w

-Wall-missed-specialisationswarn
when specialisation of any overloaded function fails.-Wno-all-missed-specialisations

-Wcompatenable
future compatibility warnings (details in Warnings and sanity-checking)-Wno-compat

-Wcompat-unqualified-importsReport
unqualified imports of core libraries which are expected to cause compatibility 
problems in future releases-Wno-compat-unqualified-imports

-Wcpp-undefwarn
on uses of the #if directive on undefined identifiers

-Wdeferred-out-of-scope-variablesReport
warnings when variable out-of-scope errors are deferred until runtime. 
See -fdefer-out-of-scope-variables-Wno-deferred-out-of-scope-variables

-Wdeferred-type-errorsReport
warnings when deferred type errors are enabled. This option is enabled by 
default. See -fdefer-type-errors-Wno-deferred-type-errors

-Wdeprecated-flagswarn
about uses of commandline flags that are deprecate-Wno-deprecated-flags

-Wdeprecationswarn
about uses of functions & types that have warnings or deprecated pragmas. 
Alias for -Wwarnings-deprecation-Wno-deprecations

-Wderiving-defaultswarn
about default deriving when using both DeriveAnyClass and 
GeneralizedNewtypeDeriving-Wno-deriving-defaults

-Wdodgy-exportswarn
about dodgy exports-Wno-dodgy-exports

-Wdodgy-foreign-importswarn
about dodgy foreign import-Wno-dodgy-foreign-import

-Wdodgy-importswarn
about dodgy imports-Wno-dodgy-imports

-Wduplicate-constraintswarn
when a constraint appears duplicated in a type signature-Wno-duplicate-constraints

-Wduplicate-exportswarn
when an entity is exported multiple time-Wno-duplicate-exports

-Wempty-enumerationswarn
about enumerations that are empt-Wno-empty-enumerations

-Werrormake
warnings fatal-Wwarn

-Weverythingenable
all warnings supported by GHC

-Whi-shadowing
deprecated) warn when a .hi file in the current directory shadows a 
library-Wno-hi-shadowing

-Widentitieswarn
about uses of Prelude numeric conversions that are probably the identity 
(and hence could be omitted)-Wno-identities

-Wimplicit-kind-varswarn
when kind variables are implicitly quantified over.-Wno-implicit-kind-vars

-Wimplicit-preludewarn
when the Prelude is implicitly importe-Wno-implicit-prelude

-Winaccessible-codewarn
about inaccessible cod-Wno-inaccessible-code

-Wincomplete-patternswarn
when a pattern match could fai-Wno-incomplete-patterns

-Wincomplete-record-updateswarn
when a record update could fai-Wno-incomplete-record-updates

-Wincomplete-uni-patternswarn
when a pattern match in a lambda expression or pattern binding could 
fail-Wno-incomplete-uni-patterns

-Winline-rule-shadowingWarn
if a rewrite RULE might fail to fire because the function might be inlined 
before the rule has a chance to fire. See How rules interact with INLINE/NOINLINE 
pragmas.-Wno-inline-rule-shadowing

-Wmissed-extra-shared-libWarn
when GHCi can’t load a shared lib-Wno-missed-extra-shared-lib

-Wmissed-specialisationswarn
when specialisation of an imported, overloaded function fails.
-Wno-missed-specialisations

-Wmissing-deriving-strategieswarn
when a deriving clause is missing a deriving strategy
-Wno-missing-deriving-strategies

-Wmissing-export-listswarn
when a module declaration does not explicitly list all 
exports-fnowarn-missing-export-lists

-Wmissing-exported-signatureswarn
about top-level functions without signatures, only if they are exported. 
takes precedence over -Wmissing-signatures-Wno-missing-exported-signatures

-Wmissing-exported-sigs
deprecated) warn about top-level functions without signatures, only if they are 
exported. takes precedence over -Wmissing-signatures-Wno-missing-exported-sigs

-Wmissing-fieldswarn
when fields of a record are uninitialise-Wno-missing-fields

-Wmissing-home-moduleswarn
when encountering a home module imported, but not listed on the command line. 
Useful for cabal to ensure GHC won’t pick up modules, not listed neither in 
exposed-modules, nor in other-modules-Wno-missing-home-modules

-Wmissing-import-listswarn
when an import declaration does not explicitly list all the names brought into 
scope-fnowarn-missing-import-lists

-Wmissing-local-signatureswarn
about polymorphic local bindings without signatures-Wno-missing-local-signatures

-Wmissing-local-sigs
deprecated) warn about polymorphic local bindings without 
signatures-Wno-missing-local-sigs

-Wmissing-methodswarn
when class methods are undefine-Wno-missing-methods

-Wmissing-monadfail-instancesWarn
when a failable pattern is used in a do-block that does not have a MonadFail 
instance-Wno-missing-monadfail-instances

-Wmissing-pattern-synonym-signatureswarn
when pattern synonyms do not have type signatures
-Wno-missing-pattern-synonym-signatures

-Wmissing-signatureswarn
about top-level functions without signature-Wno-missing-signatures

-Wmonomorphism-restrictionwarn
when the Monomorphism Restriction is applie-Wno-monomorphism-restriction

-Wname-shadowingwarn
when names are shadowed-Wno-name-shadowing

-Wno-compatDisables
all warnings enabled by -Wcompat.-Wcompat

-Wnoncanonical-monad-instances
warn when Applicative or Monad instances have noncanonical definitions of return, 
pure, (>>), or (*>). See flag description in Warnings and sanity-checking for more details-Wno-noncanonical-monad-instances

-Wnoncanonical-monadfail-instanceswarn
when Monad or MonadFail instances have noncanonical definitions of fail. 
See flag description in Warnings and sanity-checking for more details
-Wno-noncanonical-monadfail-instances

-Wnoncanonical-monoid-instanceswarn
when Semigroup or Monoid instances have noncanonical definitions of (<>) or 
mappend. See flag description in Warnings and sanity-checking for more details
-Wno-noncanonical-monoid-instances

-Worphanswarn
when the module contains orphan instance declarations or rewrite rules
-Wno-orphans

-Woverflowed-literalswarn
about literals that will overflow their typ-Wno-overflowed-literals

-Woverlapping-patternswarn
about overlapping pattern-Wno-overlapping-patterns

-Wpartial-fieldswarn
when defining a partial record field-Wno-partial-fields

-Wpartial-type-signatureswarn
about holes in partial type signatures when PartialTypeSignatures is enabled. Not applicable when PartialTypeSignatures is not enabled, in which case errors are generated for such holes-Wno-partial-type-signatures

-Wredundant-constraintsHave
the compiler warn about redundant constraints in type signatures.-Wno-redundant-constraints

-Wredundant-record-wildcardsWarn
about record wildcard matches when the wildcard binds no patterns.-Wno-redundant-record-wildcards

-Wsafewarn
if the module being compiled is regarded to be safe-Wno-safe

-Wsemigroupwarn
when a Monoid is not Semigroup, and on non- Semigroup definitions of (<>)?-Wno-semigroup

-Wsimplifiable-class-constraintsWarn
about class constraints in a type signature that can be simplified using a top-level instance declaration-Wno-simplifiable-class-constraints

-Wspace-after-bangwarn
for missing space before the second argument of an infix definition of (!) when BangPatterns are not enable-Wno-missing-space-after-bang

-Wstar-binderwarn
about binding the (*) type operator despite StarIsTyp-Wno-star-binder

-Wstar-is-typewarn
when * is used to mean Data.Kind.Typ-Wno-star-is-type

-Wtabswarn
if there are tabs in the source file-Wno-tabs

-Wtrustworthy-safewarn
if the module being compiled is marked as Trustworthy but it could instead be marked as Safe, a more informative bound-Wno-safe

-Wtype-defaultswarn
when defaulting happens-Wno-type-defaults

-Wtyped-holesReport
warnings when typed hole errors are deferred until runtime. See -fdefer-typed-holes.-Wno-typed-holes

-Wunbanged-strict-patternswarn
on pattern bind of unlifted variable that is neither bare nor banged-Wno-unbanged-strict-patterns

-Wunrecognised-pragmaswarn
about uses of pragmas that GHC doesn’t recognis-Wno-unrecognised-pragmas

-Wunrecognised-warning-flagsthrow
a warning when an unrecognised -W... flag is encountered on the command line.-Wno-unrecognised-warning-flags

-Wunsafewarn
if the module being compiled is regarded to be unsafe. See Safe Haskell-Wno-unsafe

-Wunsupported-calling-conventionswarn
about use of an unsupported calling convention-Wno-unsupported-calling-conventions

-Wunsupported-llvm-versionWarn
when using -fllvm with an unsupported version of LLVM.-Wno-monomorphism-restriction

-Wunticked-promoted-constructorswarn
if promoted constructors are not ticked-Wno-unticked-promoted-constructors

-Wunused-bindswarn
about bindings that are unused. Alias for -Wunused-top-binds, -Wunused-local-binds and -Wunused-pattern-bind-Wno-unused-binds

-Wunused-do-bindwarn
about do bindings that appear to throw away values of types other than ()-Wno-unused-do-bind

-Wunused-forallswarn
about type variables in user-written forall\s that are unused-Wno-unused-foralls

-Wunused-importswarn
about unnecessary imports-Wno-unused-imports

-Wunused-local-bindswarn
about local bindings that are unuse-Wno-unused-local-binds

-Wunused-matcheswarn
about variables in patterns that aren’t use-Wno-unused-matches

-Wunused-packageswarn
when package is requested on command line, but was never loaded.-Wno-unused-packages

-Wunused-pattern-bindswarn
about pattern match bindings that are unuse-Wno-unused-pattern-binds

-Wunused-record-wildcardsWarn
about record wildcard matches when none of the bound variables are used.-Wno-unused-record-wildcards

-Wunused-top-bindswarn
about top-level bindings that are unuse-Wno-unused-top-binds

-Wunused-type-patternswarn
about unused type variables which arise from patterns in in type family and data family instance-Wno-unused-type-patterns

-Wwarnmake
warnings non-fatal-Werror

-Wwarnings-deprecationswarn
about uses of functions & types that have warnings or deprecated pragmas-Wno-warnings-deprecations

-Wwrong-do-bindwarn
about do bindings that appear to throw away monadic values that you should have bound instead-Wno-wrong-do-bind
