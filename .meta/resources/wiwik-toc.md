# What I wish I Knew :: TOC

- Haskell book: WIWIKWLH
- http://dev.stephendiehl.com/hask/
- https://github.com/nikcleju/wiwikwlh

* Basics
  GHC
  ghcup
  Package Managers
  Project Structure
  Cabal
  Cabal New-Build
  Local Packages
  Version Bounds
  Stack
  Hpack
  Base
  Prelude
  Modern Haskell
  Flags
  Hackage
  Stackage
  GHCi
  .ghci.conf
  Editor Integration
  Linux Packages
  Names
  Modules
  Functions
  Types
  Type Signatures
  Currying
  Algebraic Datatypes
  Lists
  Pattern Matching
  Guards
  Operators and Sections
  Tuples
  Where & Let Clauses
  Conditionals
  Function Composition
  List Comprehensions
  Comments
  Typeclasses
  Side Effects
  Records
  Pragmas
  Newtypes
  Bottoms
  Exhaustiveness
  Debugger
  Stack Traces
  Printf Tracing
  Type Inference
  Type Holes
  Deferred Type Errors
  Name Conventions
  ghcid
  HLint
  Docker Images
  Continuous Integration
  Ormolu
  Haddock
  Unsafe Functions
* Monads
  Eightfold Path to Monad Satori
  Monad Myths
  Monad Methods
  Monad Laws
  Do Notation
  Maybe Monad
  List Monad
  IO Monad
  What's the point?
  Reader Monad
  Writer Monad
  State Monad
  Why are monads confusing?
  Monad Transformers
  mtl / transformers
  Transformers
  Basics
  mtl
  ReaderT
  Newtype Deriving
  Efficiency
  Monad Morphisms
  Effect Systems
  Polysemy
  Fused Effects
* Language Extensions
  Philosophy
  Classes
  Extension Dependencies
  The Benign
  The Advanced
  The Lowlevel
  The Dangerous
  NoMonomorphismRestriction
  ExtendedDefaultRules
  Safe Haskell
  PartialTypeSignatures
  RecursiveDo
  ApplicativeDo
  PatternGuards
  ViewPatterns
  TupleSections
  Postfix Operators
  MultiWayIf
  EmptyCase
  LambdaCase
  NumDecimals
  PackageImports
  RecordWildCards
  NamedFieldPuns
  PatternSynonyms
  DeriveFunctor
  DeriveFoldable
  DeriveTraversable
  DeriveGeneric
  DeriveAnyClass
  DuplicateRecordFields
  OverloadedLabels
  CPP
  TypeApplications
  DerivingVia
  DerivingStrategies
  Historical Extensions
* Type Class Extensions
  Standard Hierarchy
  Instance Search
  Orphan Instances
  Minimal Annotations
  TypeSynonymInstances
  FlexibleInstances
  FlexibleContexts
  OverlappingInstances
  IncoherentInstances
  Laziness
  Strictness
  Seq and WHNF
  Thunks
  BangPatterns
  StrictData
  Strict
  Deepseq
  Irrefutable Patterns
  The Debate
* Prelude
  What to Avoid?
  What Should be in Prelude
  Custom Preludes
  Preludes
  Protolude
  Partial Functions
  Replacing Partiality
  Boolean Blindness
  Foldable / Traversable
* Strings
  String
  String Conversions
  OverloadedStrings
  Text
  Text.Builder
  ByteString
  Printf
  Overloaded Lists
  Regex
  Escaping Text
  String Splitting
  * Applicatives
  Alternative
  Arrows
  Bifunctors
  Polyvariadic Functions
* Error Handling
  Either Monad
  ExceptT
  Control.Exception
  Exceptions
  Spoon
* Advanced Monads
  Function Monad
  RWS Monad
  Cont
  MonadPlus
  MonadFail
  MonadFix
  ST Monad
  Free Monads
  Indexed Monads
  Lifted Base
* Quantification
  Universal Quantification
  Free Theorems
  Type Systems
  Rank-N Types
  Existential Quantification
  Impredicative Types
  Scoped Type Variables
* GADTs
  Kind Signatures
  Void
  Phantom Types
  Typelevel Operations
* Interpreters
  HOAS
  PHOAS
  Final Interpreters
  Finally Tagless
  Datatypes
  F-Algebras
  Recursion Schemes & The Morphism Zoo
  Hint and Mueval
* Testing
  QuickCheck
  SmallCheck
  QuickSpec
  Tasty
  Silently
* Type Families
  MultiParam Typeclasses
  Type Families
  Injectivity
  Roles
  NonEmpty
  Manual Proofs
  Constraint Kinds
  TypeFamilyDependencies
* Promotion
  Higher Kinded Types
  Kind Polymorphism
  Data Kinds
  Size-Indexed Vectors
  Typelevel Numbers
  Typelevel Strings
  Custom Errors
  Type Equality
  Proxies
  Promoted Syntax
  Singleton Types
  Closed Type Families
  Kind Indexed Type Families
  HLists
  Typelevel Dictionaries
  Advanced Proofs
  Liquid Haskell
* Generics
  Generic
  Generic Deriving
  Typeable
  Dynamic Types
  Data
  Uniplate
* Mathematics
  Numeric Tower
  GMP Integers
  Complex Numbers
  Decimal & Scientific Types
  Polynomial Arithmetic
  Combinatorics
  Number Theory
  Stochastic Calculus
  Differential Equations
  Statistics & Probability
  Constructive Reals
  SAT Solvers
  SMT Solvers
* Data Structures
  Map
  Tree
  Set
  Vector
  Mutable Vectors
  Unordered Containers
  Hashtables
  Graphs
  Graph Theory
  DList
  Sequence
* FFI
  Pure Functions
  Storable Arrays
  Function Pointers
  hsc2hs
* Concurrency
  Sparks
  Threads
  IORef
  MVars
  TVar
  Chans
  Semaphores
  Threadscope
  Strategies
  STM
  Monad Par
  Async
* Parsing
  Parsec
  Custom Lexer
  Simple Parsing
  Megaparsec
  Attoparsec
  Configurator
  Optparse Applicative
  Happy & Alex
* Streaming
  Lazy IO
  Pipes
  ZeroMQ
  Conduits
* Cryptography
  SHA Hashing
  Password Hashing
  Curve25519 Diffie-Hellman
  Ed25519 EdDSA
  Secure Memory Handling
  AES Encryption
  Galois Fields
  Elliptic Curves
  Pairing Cryptography
  zkSNARKs
* Dates and Times
  time
  ISO8601
* Data Formats
  JSON
  Yaml
  CSV
* Network & Web Programming
  Frameworks
  HTTP Requests
  Req
  Blaze
  Lucid
  Hastache
  Warp
  Scotty
  Servant
  Databases
  Postgres
  Sqlite
  Redis
  Acid State
  Selda
* GHC
  Compiler Design
  GHC API
  DynFlags
  Package Databases
  HIE Bios
  Abstract Syntax Tree
  Parser
  Outputable
  Datatypes
  Core
  Inliner
  Primops
  SIMD Intrinsics
  Rewrite Rules
  Boot Libraries
  Dictionaries
  Specialization
  Static Compilation
  Unboxed Types
  IO/ST
  ghc-heap-view
  STG
  Worker/Wrapper
  Z-Encoding
  Cmm
  Inline CMM
  Optimisation
  Interface Files
  Runtime System
* Profiling
  Criterion
  EKG
  RTS Profiling
* Compilers
  Unbound
  Unbound Generics
  Pretty Printers
  prettyprinter
  pretty-simple
  Haskeline
  Repline
  LLVM
  Template Haskell
  Metaprogramming
  Quasiquotation
  language-c-quote
  GPU Kernels
* Template Haskell
  Antiquotation
  Templated Type Families
  Templated Type Classes
  Multiline Strings
  Path Files
  Categories
  Do I need to Learn Category Theory?
  Abstract Algebra
* Categories
  Isomorphisms
  Duality
  Functors
  Natural Transformations
  Kleisli Category
  Monoidal Categories
  Further Resources
