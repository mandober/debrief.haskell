# Haskell TOPICS

* Syntax I
  values
  expressions
  types
  kinds
  namespaces
  layout
  comments

* Values
  - variables
  - data constructors
  - literals
    - numeric literals
    - characters literals
    - string literals
  - operators

* Variables
  - term variable
    - identifier, name, label
    - type
    - quantifier
  - type variable
    - identifier, name, label
    - kind
    - rank
    - quantifier
  - kind variable
    - identifier, name, label
    - quantifier

* Expressions
  - currying
  - lambdas
  - operator applications
  - sections
  - conditionals
  - lists
  - tuples
  - unit expression
  - parenthesized expression
  - arithmetic sequence
  - list comprehension
  - `let` expression
  - `case` Expressions
  - `do` Expressions
  - Datatypes with Field Labels
    - Field Selection
    - Construction Using Field Labels
    - Updates Using Field Labels
  - Expression Type-Signatures
  - Pattern Matching
    - Patterns
    - Informal Semantics of Pattern Matching
    - Formal Semantics of Pattern Matching
    - as-pattern
    - top-level pattern binding

  - `where` clause

* Binding
  - binding names
    - binding values
    - binding literals
    - binding lambdas
    - binding named     functions ≡ binding lambdas (rewrite)
    - binding recursive functions ≡ binding lambdas + fix (rewrite)
    - binding mutually recursive named functions (?)
  - binding properties
    - name (alpha or symbolic)
    - type
    - immutable (can only bind a symbol once in a give scope)
    - order-independent (wrt source code location)
    - recursive (bound symbol is in scope within its own definition)
    - lazy
    - bound symbols still called variables (since function args can vary)
  - bindings level
    - top-level
    - local
    - nested
      - overshadowing
  - binding patterns
    - refutable
    - irrefutable



* Recursion
  - tail-calls
    - accumulators
  - mutual recursion
  - let recursion
  - polymorphic recursion



------------------------------------------------------------------------------
fixity
declaration
data type declaration
data type definition
data type instance

------------------------------------------------------------------------------
* Type classes
  class declaration
  class instantiation
  class contexts
  class assertion


* Type class deriving
  Derived Instances

* Modules

* Input/Output

* Foreign Function Interface
  - Foreign Declarations
  - External Entities
  - Marshalling

------------------------------------------------------------------------------

* Type system
  data types
  kinds

------------------------------------------------------------------------------

* Packages
  pipes
  conduit

* Tools
  hlint
  britany

* Tooling
  HLS
  stack
  cabal

------------------------------------------------------------------------------

* Haskell language
  Haskell language
  Haskell standard
  Haskell implementations
  Haskell compilers
  Haskell GHC flavor

* GHC
  GHC compiler
  GHC extensions
  GHC language extensions


* GHC Pragmas
  - Inlining
  - Specialization
  - RULES
  - UN/PACK
  - OVERLAP, OVERLAPPING, OVERLAPPABLE
  - Language extensions

* Internals
  desugaring
  Core language
  STG machine

------------------------------------------------------------------------------

* Theoretical background
  math
  logic
  type theory
  category theory

------------------------------------------------------------------------------
