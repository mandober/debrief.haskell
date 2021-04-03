# GHC Language Features

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/lang.html

## TOC

9. GHC Language Features

9.1. Language options

9.2. Unboxed types and primitive operations
  9.2.1. Unboxed types
  9.2.2. Unboxed type kinds
  9.2.3. Unboxed tuples
  9.2.4. Unboxed sums
  9.2.5. Unlifted Newtypes

9.3. Syntactic extensions
  9.3.1. Unicode syntax
  9.3.2. The magic hash
  9.3.3. Negative literals
  9.3.4. Fractional looking integer literals
  9.3.5. Binary integer literals
  9.3.6. Hexadecimal floating point literals
  9.3.7. Numeric underscores
  9.3.8. Pattern guards
  9.3.9. View patterns
  9.3.10. n+k patterns
  9.3.11. The recursive do-notation
    9.3.11.1. Recursive binding groups
    9.3.11.2. The mdo notation
  9.3.12. Applicative do-notation
    9.3.12.1. Strict patterns
    9.3.12.2. Things to watch out for
  9.3.13. Parallel List Comprehensions
  9.3.14. Generalised (SQL-like) List Comprehensions
  9.3.15. Monad comprehensions
  9.3.16. New monadic failure desugaring mechanism
  9.3.17. Rebindable syntax and the implicit Prelude import
    9.3.17.1. Things unaffected by RebindableSyntax
  9.3.18. Postfix operators
  9.3.19. Tuple sections
  9.3.20. Lambda-case
  9.3.21. Empty case alternatives
  9.3.22. Multi-way if-expressions
  9.3.23. Local Fixity Declarations
  9.3.24. Import and export extensions
    9.3.24.1. Hiding things the imported module doesn't export
    9.3.24.2. Package-qualified imports
    9.3.24.3. Safe imports
    9.3.24.4. Explicit namespaces in import/export
    9.3.24.5. Writing qualified in postpositive position
  9.3.25. More liberal syntax for function arguments
    9.3.25.1. Changes to the grammar
  9.3.26. Summary of stolen syntax

9.4. Extensions to data types and type synonyms
  9.4.1. Data types with no constructors
  9.4.2. Data type contexts
  9.4.3. Infix type constructors, classes, and type variables
  9.4.4. Type operators
  9.4.5. Liberalised type synonyms
  9.4.6. Existentially quantified data constructors
  9.4.6.1. Why existential?
  9.4.6.2. Existentials and type classes
  9.4.6.3. Record Constructors
  9.4.6.4. Restrictions
  9.4.7. Declaring data types with explicit constructor signatures
  9.4.8. Generalised Algebraic Data Types (GADTs)

9.5. Extensions to the record system
  9.5.1. Traditional record syntax
  9.5.2. Record field disambiguation
  9.5.3. Duplicate record fields
  9.5.3.1. Selector functions
  9.5.3.2. Record updates
  9.5.3.3. Import and export of record fields
  9.5.4. Record puns
  9.5.5. Record wildcards
  9.5.6. Record field selector polymorphism
  9.5.6.1. Solving HasField constraints
  9.5.6.2. Virtual record fields

9.6. Extensions to the "deriving" mechanism
  9.6.1. Deriving instances for empty data types
  9.6.2. Inferred context for deriving clauses
  9.6.3. Stand-alone deriving declarations
  9.6.4. Deriving instances of extra classes (Data, etc.)
  9.6.4.1. Deriving Functor instances
  9.6.4.2. Deriving Foldable instances
  9.6.4.3. Deriving Traversable instances
  9.6.4.4. Deriving Data instances
  9.6.4.5. Deriving Typeable instances
  9.6.4.6. Deriving Lift instances
  9.6.5. Generalised derived instances for newtypes
  9.6.5.1. Generalising the deriving clause
  9.6.5.2. A more precise specification
  9.6.5.3. Associated type families
  9.6.6. Deriving any other class
  9.6.7. Deriving strategies
  9.6.7.1. Default deriving strategy
  9.6.8. Deriving via

9.7. Pattern synonyms
  9.7.1. Record Pattern Synonyms
  9.7.2. Syntax and scoping of pattern synonyms
  9.7.3. Import and export of pattern synonyms
  9.7.4. Typing of pattern synonyms
  9.7.5. Matching of pattern synonyms

9.8. Class and instances declarations
  9.8.1. Class declarations
  9.8.1.1. Multi-parameter type classes
  9.8.1.2. The superclasses of a class declaration
  9.8.1.3. Constrained class method types
  9.8.1.4. Default method signatures
  9.8.1.5. Nullary type classes
  9.8.2. Functional dependencies
  9.8.2.1. Rules for functional dependencies
  9.8.2.2. Background on functional dependencies
  9.8.2.2.1. An attempt to use constructor classes
  9.8.2.2.2. Adding functional dependencies
  9.8.3. Instance declarations
  9.8.3.1. Instance resolution
  9.8.3.2. Relaxed rules for the instance head
  9.8.3.3. Relaxed rules for instance contexts
  9.8.3.4. Instance termination rules
  9.8.3.5. Undecidable instances
  9.8.3.6. Overlapping instances
  9.8.3.7. Instance signatures: type signatures in instance declarations
  9.8.4. Overloaded string literals
  9.8.5. Overloaded labels
  9.8.6. Overloaded lists
  9.8.6.1. The IsList class
  9.8.6.2. Rebindable syntax
  9.8.6.3. Defaulting
  9.8.6.4. Speculation about the future
  9.8.7. Undecidable (or recursive) superclasses

9.9. Type families
  9.9.1. Data families
  9.9.1.1. Data family declarations
  9.9.1.2. Data instance declarations
  9.9.1.3. Overlap of data instances
  9.9.2. Synonym families
  9.9.2.1. Type family declarations
  9.9.2.2. Type instance declarations
  9.9.2.3. Closed type families
  9.9.2.4. Type family examples
  9.9.2.5. Compatibility and apartness of type family equations
  9.9.2.6. Decidability of type synonym instances
  9.9.3. Wildcards on the LHS of data and type family instances
  9.9.4. Associated data and type families
  9.9.4.1. Associated instances
  9.9.4.2. Associated type synonym defaults
  9.9.4.3. Scoping of class parameters
  9.9.4.4. Instance contexts and associated type and data instances
  9.9.5. Import and export
  9.9.5.1. Examples
  9.9.5.2. Instances
  9.9.6. Type families and instance declarations
  9.9.7. Injective type families
  9.9.7.1. Syntax of injectivity annotation
  9.9.7.2. Verifying the injectivity annotation against type family equations

9.10. Datatype promotion
  9.10.1. Motivation
  9.10.2. Overview
  9.10.3. Distinguishing between types and constructors
  9.10.4. Promoted list and tuple types
  9.10.5. Promoting existential data constructors

9.11. Kind polymorphism
  9.11.1. Overview of kind polymorphism
  9.11.2. Overview of Type-in-Type
  9.11.3. Principles of kind inference
  9.11.4. Inferring the order of variables in a type/class declaration
  9.11.5. Complete user-supplied kind signatures and polymorphic recursion
  9.11.6. Standalone kind signatures and polymorphic recursion
  9.11.7. Standalone kind signatures and declaration headers
  9.11.8. Kind inference in closed type families
  9.11.9. Kind inference in class instance declarations
  9.11.10. Kind inference in type signatures
  9.11.11. Explicit kind quantification
  9.11.12. Implicit quantification in type synonyms and type family instances
  9.11.13. Kind-indexed GADTs
  9.11.14. Higher-rank kinds
  9.11.15. Constraints in kinds
  9.11.16. The kind Type
  9.11.17. Inferring dependency in datatype declarations
  9.11.18. Inferring dependency in user-written foralls
  9.11.19. Kind defaulting without PolyKinds
  9.11.20. Pretty-printing in the presence of kind polymorphism

9.12. Levity polymorphism
  9.12.1. No levity-polymorphic variables or arguments
  9.12.2. Levity-polymorphic bottoms
  9.12.3. Printing levity-polymorphic types

9.13. Type-Level Literals
  9.13.1. Runtime Values for Type-Level Literals
  9.13.2. Computing With Type-Level Naturals

9.14. Equality constraints, Coercible, and the kind Constraint
  9.14.1. Equality constraints
  9.14.2. Heterogeneous equality
  9.14.3. Unlifted heterogeneous equality
  9.14.4. The Coercible constraint
  9.14.5. The Constraint kind

9.15. Quantified constraints
  9.15.1. Motivation
  9.15.2. Syntax changes
  9.15.3. Typing changes
  9.15.4. Superclasses
  9.15.5. Overlap
  9.15.6. Instance lookup
  9.15.7. Termination
  9.15.8. Coherence

9.16. Extensions to type signatures
  9.16.1. Explicit universal quantification (forall)
  9.16.2. The context of a type signature
  9.16.3. Ambiguous types and the ambiguity check
  9.16.4. Explicitly-kinded quantification

9.17. Lexically scoped type variables
  9.17.1. Overview
  9.17.2. Declaration type signatures
  9.17.3. Expression type signatures
  9.17.4. Pattern type signatures
  9.17.5. Class and instance declarations

9.18. Bindings and generalisation
  9.18.1. Switching off the dreaded Monomorphism Restriction
  9.18.2. Let-generalisation

9.19. Visible type application
  9.19.1. Inferred vs. specified type variables
  9.19.2. Ordering of specified variables

9.20. Implicit parameters
  9.20.1. Implicit-parameter type constraints
  9.20.2. Implicit-parameter bindings
  9.20.3. Implicit parameters and polymorphic recursion
  9.20.4. Implicit parameters and monomorphism

9.21. Arbitrary-rank polymorphism
  9.21.1. Examples
  9.21.2. Type inference
  9.21.3. Implicit quantification

9.22. Impredicative polymorphism

9.23. Typed Holes
  9.23.1. Valid Hole Fits
  9.23.1.1. Refinement Hole Fits
  9.23.1.2. Sorting Valid Hole Fits

9.24. Partial Type Signatures
  9.24.1. Syntax
  9.24.1.1. Type Wildcards
  9.24.1.2. Named Wildcards
  9.24.1.3. Extra-Constraints Wildcard
  9.24.2. Where can they occur?

9.25. Custom compile-time errors

9.26. Deferring type errors to runtime
  9.26.1. Enabling deferring of type errors
  9.26.2. Deferred type errors in GHCi
  9.26.3. Limitations of deferred type errors

9.27. Template Haskell
  9.27.1. Syntax
  9.27.2. Using Template Haskell
  9.27.3. Viewing Template Haskell generated code
  9.27.4. A Template Haskell Worked Example
  9.27.5. Using Template Haskell with Profiling
  9.27.6. Template Haskell Quasi-quotation

9.28. Arrow notation
  9.28.1. do-notation for commands
  9.28.2. Conditional commands
  9.28.3. Defining your own control structures
  9.28.4. Primitive constructs
  9.28.5. Differences with the paper
  9.28.6. Portability

9.29. Bang patterns and Strict Haskell
  9.29.1. Bang patterns
  9.29.2. Strict-by-default data types
  9.29.3. Strict-by-default pattern bindings
  9.29.4. Modularity
  9.29.5. Dynamic semantics of bang patterns

9.30. Assertions

9.31. Static pointers
  9.31.1. Using static pointers
  9.31.2. Static semantics of static pointers

9.32. Pragmas
  9.32.1. LANGUAGE pragma
  9.32.2. OPTIONS_GHC pragma
  9.32.3. INCLUDE pragma
  9.32.4. WARNING and DEPRECATED pragmas
  9.32.5. MINIMAL pragma
  9.32.6. INLINE and NOINLINE pragmas
  9.32.6.1. INLINE pragma
  9.32.6.2. INLINABLE pragma
  9.32.6.3. NOINLINE pragma
  9.32.6.4. CONLIKE modifier
  9.32.6.5. Phase control
  9.32.7. LINE pragma
  9.32.8. COLUMN pragma
  9.32.9. RULES pragma
  9.32.10. SPECIALIZE pragma
  9.32.10.1. SPECIALIZE INLINE
  9.32.10.2. SPECIALIZE for imported functions
  9.32.11. SPECIALIZE instance pragma
  9.32.12. UNPACK pragma
  9.32.13. NOUNPACK pragma
  9.32.14. SOURCE pragma
  9.32.15. COMPLETE pragmas
  9.32.16. OVERLAPPING, OVERLAPPABLE, OVERLAPS, and INCOHERENT pragmas

9.33. Rewrite rules
  9.33.1. Syntax
  9.33.2. Semantics
  9.33.3. How rules interact with INLINE/NOINLINE pragmas
  9.33.4. How rules interact with CONLIKE pragmas
  9.33.5. How rules interact with class methods
  9.33.6. List fusion
  9.33.7. Specialisation
  9.33.8. Controlling what's going on in rewrite rules

9.34. Special built-in functions

9.35. Generic classes

9.36. Generic programming
  9.36.1. Deriving representations
  9.36.2. Writing generic functions
  9.36.3. Unlifted representation types
  9.36.4. Generic defaults
  9.36.5. More information

9.37. Roles
  9.37.1. Nominal, Representational, and Phantom
  9.37.2. Role inference
  9.37.3. Role annotations

9.38. HasCallStack
  9.38.1. Compared with other sources of stack traces

9.39. Concurrent and Parallel Haskell
  9.39.1. Concurrent Haskell
  9.39.2. Software Transactional Memory
  9.39.3. Parallel Haskell
  9.39.4. Annotating pure code for parallelism

9.40. Safe Haskell
  9.40.1. Uses of Safe Haskell
  9.40.1.1. Strict type-safety (good style)
  9.40.1.2. Building secure systems (restricted IO Monads)
  9.40.2. Safe Language
  9.40.2.1. Safe Overlapping Instances
  9.40.3. Safe Imports
  9.40.4. Trust and Safe Haskell Modes
  9.40.4.1. Trust check (-fpackage-trust disabled)
  9.40.4.2. Trust check (-fpackage-trust enabled)
  9.40.4.3. Example
  9.40.4.4. Trustworthy Requirements
  9.40.4.5. Package Trust
  9.40.5. Safe Haskell Inference
  9.40.6. Safe Haskell Flag Summary
  9.40.7. Safe Compilation
