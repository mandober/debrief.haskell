# Tabular overview of extensions

All language extensions in GHC 9.0.1   
https://downloads.haskell.org/ghc/latest/docs/html/users_guide/exts/table.html


Nr.|Extension                  | Groups | →  |  ← | Flags | Steals | Implied by <- | Implies ->
---|---------------------------|--------|----|----|--------------------------------------------
001|AllowAmbiguousTypes        | ty     |    |    |
002|ApplicativeDo              |        |    |    |
003|Arrows                     |        |    |    | `rec`, `proc`, `-<`, `>-`, `-<<`, `>>-`, `(|`, `|)`
004|BangPatterns               | sy     |    |    |
005|BinaryLiterals             | sy     |    |    |
006|BlockArguments             |        |    |    |
007|CApiFFI                    |        |    |    |
008|ConstrainedClassMethods    |        |    |    |
009|ConstraintKinds            |        |    |    |
---|---------------------------|--------|----|----|-----------------------------------------------------------------------
010|CPP                        |        |    |    | 
011|CUSKs                      |        |    |    |
012|DataKinds                  |        |    |    |
013|DatatypeContexts           |        |    |    |
014|DefaultSignatures          |        |    |    |
015|DeriveAnyClass             |        |    |    |
016|DeriveDataTypeable         |        |    |    |
017|DeriveFoldable             |        |    | 021|
018|DeriveFunctor              |        |    | 021|
019|DeriveGeneric              |        |    |    |
---|---------------------------|--------|---------------------------------------------------------------------------------
020|DeriveLift                 |        |
021|DeriveTraversable          |        | -> 017 018
022|DerivingStrategies         |        |
023|DerivingVia                |        | -> 022
024|DisambiguateRecordFields   |        | <- RecordWildCards
025|DuplicateRecordFields      |        |
026|EmptyCase                  |        |
027|EmptyDataDecls             |        |
028|EmptyDataDeriving          |        |
029|ExistentialQuantification  |        |
---|---------------------------|--------|---------------------------------------------------------------------------------
030|ExplicitForAll             |        | Implied by: ScopedTypeVariables, LiberalTypeSynonyms, RankNTypes, ExistentialQuantification
031|ExplicitNamespaces         |        | Implied by: TypeOperators, TypeFamilies
032|ExtendedDefaultRules       |        |
033|FlexibleContexts           |        | 
034|FlexibleInstances          |        | Implies TypeSynonymInstances
035|ForeignFunctionInterface   |        | 
036|FunctionalDependencies     |        | Implies MultiParamTypeClasses
037|GADTs                      |        | Implies GADTSyntax and MonoLocalBinds
038|GADTSyntax                 |        | 
039|GeneralisedNewtypeDeriving |        | 
---|---------------------------|--------|---------------------------------------------------------------------------------
040|GHCForeignImportPrim       |        | 
041|Haskell2010                |        | 
042|Haskell98                  |        | 
043|HexFloatLiterals           |        | 
044|ImplicitParams             |        | `?<varid>`
045|ImplicitPrelude            |        | Implied by RebindableSyntax
046|ImportQualifiedPost        |        | 
047|ImpredicativeTypes         |        | Implies RankNTypes
048|IncoherentInstances        |        | Implies OverlappingInstances
049|InstanceSigs               |        | 
---|---------------------------|--------|---------------------------------------------------------------------------------
050|InterruptibleFFI           |        | 
051|KindSignatures             |        | Implied by TypeFamilies and PolyKinds.                                                                                                              
052|LambdaCase                 |        | 
053|LexicalNegation            |        | synt | Use whitespace to determine whether the minus sign stands for negation or subtraction
054|LiberalTypeSynonyms        |        |
055|LinearTypes                |        | `a %1 -> b`, `a %m -> b`
056|MagicHash                  |        |
057|MonadComprehensions        |        |
058|MonadFailDesugaring        |        |
059|MonoLocalBinds             |        | Implied by: TypeFamilies and GADTs
---|---------------------------|--------|---------------------------------------------------------------------------------
060|MonomorphismRestriction    |        |
061|MultiParamTypeClasses      |        | Implied by FunctionalDependencies
062|MultiWayIf                 |        |
063|NamedFieldPuns             |        |
064|NamedWildCards             |        |
065|NegativeLiterals           |        |
066|NondecreasingIndentation   |        |
067|NPlusKPatterns             |        | Implied by Haskell98
068|NullaryTypeClasses         |        | Deprecated, does nothing
069|NumDecimals                |        |
---|---------------------------|--------|---------------------------------------------------------------------------------
070|NumericUnderscores         |        |
071|OverlappingInstances       |        |
072|OverloadedLabels           |        |
073|OverloadedLists            |        |
074|OverloadedStrings          |        |
075|PackageImports             |        |
076|ParallelListComp           |        |
077|PartialTypeSignatures      |        |
078|PatternGuards              |        | Implied by Haskell98
079|PatternSynonyms            |        |
---|---------------------------|--------|---------------------------------------------------------------------------------
080|PolyKinds                  |        | Implies KindSignatures
081|PostfixOperators           |        |
082|QualifiedDo                |        |
083|QuantifiedConstraints      |        |
084|QuasiQuotes                |        |
085|Rank2Types                 |        | Synonym for RankNTypes
086|RankNTypes                 |        | Implied by ImpredicativeTypes
087|RebindableSyntax           |        | Implies NoImplicitPrelude
088|RecordWildCards            |        | Implies DisambiguateRecordFields
089|RecursiveDo                |        |
---|---------------------------|--------|---------------------------------------------------------------------------------
090|RoleAnnotations            |        |
091|Safe                       |        |
092|ScopedTypeVariables        |        |
093|StandaloneDeriving         |        |
094|StandaloneKindSignatures   |        |
095|StarIsType                 |        |
096|StaticPointers             |        |
097|Strict                     |        |
098|StrictData                 |        |
099|TemplateHaskell            |        |
---|---------------------------|--------|---------------------------------------------------------------------------------
100|TemplateHaskellQuotes      |        |
101|TraditionalRecordSyntax    |        |
102|TransformListComp          |        |
103|Trustworthy                |        |
104|TupleSections              |        |
105|TypeApplications           |        |
106|TypeFamilies               |        | Implies ExplicitNamespaces, KindSignatures, MonoLocalBinds
107|TypeFamilyDependencies     |        |
108|TypeInType                 |        | Deprecated
109|TypeOperators              |        | Implies ExplicitNamespaces
---|---------------------------|--------|---------------------------------------------------------------------------------
110|TypeSynonymInstances       |        | Implied by FlexibleInstances
111|UnboxedSums                |        |
112|UnboxedTuples              |        |
113|UndecidableInstances       |        |
114|UndecidableSuperClasses    |        |
115|UnicodeSyntax              |        |
116|UnliftedFFITypes           |        |
117|UnliftedNewtypes           |        |
118|Unsafe                     |        |
119|ViewPatterns               |        |
---|---------------------------|--------|---------------------------------------------------------------------------------
