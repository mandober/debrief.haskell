# glasgow-exts

Although not recommended, the deprecated `-fglasgow-exts` flag enables a large swath of the extensions supported by GHC at once.

The flag `-fglasgow-exts` enables the following 32 extensions:
- ConstrainedClassMethods
- DeriveDataTypeable
- DeriveFoldable
- DeriveFunctor
- DeriveGeneric
- DeriveTraversable
- EmptyDataDecls
- ExistentialQuantification
- ExplicitNamespaces
- FlexibleContexts
- FlexibleInstances
- ForeignFunctionInterface
- FunctionalDependencies
- GeneralizedNewtypeDeriving
- ImplicitParams
- InterruptibleFFI
- KindSignatures
- LiberalTypeSynonyms
- MagicHash
- MultiParamTypeClasses
- ParallelListComp
- PatternGuards
- PostfixOperators
- RankNTypes
- RecursiveDo
- ScopedTypeVariables
- StandaloneDeriving
- TypeOperators
- TypeSynonymInstances
- UnboxedTuples
- UnicodeSyntax
- UnliftedFFITypes

Enabling these options is the only effect of -fglasgow-exts. We are trying to move away from this portmanteau flag, and towards enabling features individually.
