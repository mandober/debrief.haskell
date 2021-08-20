# Haskell Language Extension Taxonomy
(Latest update: July 2021)

Caveat: It's just personal opinion, and was written to be a bit provocative
and encourage discussion . It is also something that is constantly evolving.
Some of the main criteria I used in constructing this taxonomy are age, how
widely used it us, and how well understood it is by the average Haskell
programmer.  These things will change over time.

Aso, this is focused on appropriateness for use in commercial production code
bases.  If you are not thinking about commercial use with a team of multiple
people, then the calculus completely changes.

Almost all extensions in this list can also be used with a "No" prefix.  The
notable ones for which "No" is most common are NoMonomorphismRestriction and
NoImplicitPrelude.

This is not an exhaustive list of language extensions. As a general rule of
thumb, extensions that are not mentioned here should be shied away from because
either they weren't commonly used enough to be mentioned here, or they are very
new. New language extenions should generally be avoided in production code. Ask
not, "How can I use this great new GHC feature in my code?"...but rather ask,
"How could I accomplish this in a clean way with the fewest language extensions?"

Here are some notable quotes on this topic from prominent haskellers.

> I try to stay with Haskell 2010 as far as I can. The one exception is MPTC and
> FD, which are sometimes too useful to avoid. Furthermore, I use extensions to
> Haskell 2010 that I regard as just syntactic sugar, e.g., TupleSections. What I
> try to avoid is extensions that are a semantic headache, e.g., GADTs.

> Don't get me wrong, I like cool features as much as anyone else (I suggested
> GADTs 10 years before GHC got them), but for production code I think it's best
> to stay very conservative and avoid too much cleverness.

-- [Lennart Augustsson](https://www.reddit.com/r/haskell/comments/2olrxn/what_is_an_intermediate_haskell_programmer/cmooh4p/),
author of multiple languages and Haskell compilers

> Stick to the basics. You get the vast majority of benefits. I haven't talked
> about GADTs. I haven't talked about any technology invented in the last 10
> years... You get most of the benefit just using newtypes, data, and functions.
> And certainly all the benefits of reuse and minimizing complexity were already
> there in '95 or earlier.

-- Don Stewart [Haskell in the
Large](https://skillsmatter.com/skillscasts/9098-haskell-in-the-large-the-day-to-day-practice-of-using-haskell-to-write-large-systems)
October 2016 (55:55)

Language Pragma History
https://ghc.haskell.org/trac/ghc/wiki/LanguagePragmaHistory

Another take on this topic
http://dev.stephendiehl.com/hask/#language-extensions

## Level 0 - Use at will

Tried and true extensions.  Very unlikely to cause problems.  You can use
these and rarely get any complaints.

- ApplicativeDo
- AutoDeriveTypeable       -- Automatic derivation of Typeable
- BangPatterns
- Cpp
- DeriveDataTypeable
- DeriveFoldable
- DeriveFunctor
- DeriveGeneric
- DeriveLift
- DeriveTraversable
- EmptyDataDecls
- ExistentialQuantification
- ExplicitForAll
- FlexibleContexts
- FlexibleInstances
- ForeignFunctionInterface
- GeneralizedNewtypeDeriving
- JavaScriptFFI
- KindSignatures
- LambdaCase
- MagicHash
- MonomorphismRestriction
- MultiParamTypeClasses
- OverloadedLists
- OverloadedStrings
- PackageImports
- PartialTypeSignatures
- RankNTypes
- RecordWildCards          -- This one is somewhat controversial
- ScopedTypeVariables
- StandaloneDeriving
- TupleSections
- TypeApplications
- UnboxedTuples
- ViewPatterns

## Level 1 - More complex, but well understood

More complexity than level 0, but on the whole quite well understood and
useful.  Pose a little bit more of an obstacle in terms of concepts you
need to understand to use them.  Implementations without level 1 are
still preferred if possible.

- AllowAmbiguousTypes
- DefaultSignatures
- DerivingStrategies
- FunctionalDependencies
- GADTs
- ImplicitPrelude          -- Alternate preludes meh
- QuasiQuotes
- RecursiveDo
- TemplateHaskell
- TypeFamilies
- TypeOperators
- TypeSynonymInstances
- UndecidableInstances

## Level 2 - Avoid unless you have a VERY good reason

Substantially newer and more complex concepts that are not in general production
ready. Don't use unless you have an alternate implementation for comparison that
doesn't use it, and can clearly articulate the value you are getting from using
the extension and have weighed it against the cost.

- Arrows                   -- Arrow-notation syntax.  Arrows suck :)
- ConstraintKinds
- DataKinds
- DeriveAnyClass
- OverlappingInstances
- PolyKinds
- RebindableSyntax
- Strict
- StrictData
- TypeFamilyDependencies
- TypeInType

## Banned

I don't think there are very many absolutes in software and this is no
exception. You should interpret this as "banned for the most part".

- ImpredicativeTypes
- IncoherentInstances
- UnicodeSyntax -- Admittedly opinionated, USA-centric, and pragmatically intended for maximum ease of use across large teams.

## Unknown / undecided

- AlternativeLayoutRule
- AlternativeLayoutRuleTransitional
- BinaryLiterals
- CApiFFI
- ConstrainedClassMethods
- DatatypeContexts
- DisambiguateRecordFields
- DoAndIfThenElse
- DuplicateRecordFields
- EmptyCase
- ExplicitNamespaces
- ExtendedDefaultRules     -- Use GHC's extended rules for defaulting
- GADTSyntax
- GHCForeignImportPrim
- ImplicitParams
- InstanceSigs
- InterruptibleFFI
- LiberalTypeSynonyms
- MonadComprehensions
- MonadFailDesugaring
- MonoLocalBinds
- MonoPatBinds
- MultiWayIf
- NPlusKPatterns
- NamedWildCards
- NegativeLiterals
- NondecreasingIndentation
- NullaryTypeClasses
- NumDecimals
- OverloadedLabels
- ParallelArrays           -- Syntactic support for parallel arrays
- ParallelListComp
- PatternGuards
- PatternSynonyms
- PostfixOperators
- RecordPuns
- RelaxedLayout
- RelaxedPolyRec           -- Deprecated
- RoleAnnotations
- StaticPointers
- TemplateHaskellQuotes    -- subset of TH supported by stage1, no splice
- TraditionalRecordSyntax
- TransformListComp
- UnboxedSums
- UndecidableSuperClasses
- UnliftedFFITypes

