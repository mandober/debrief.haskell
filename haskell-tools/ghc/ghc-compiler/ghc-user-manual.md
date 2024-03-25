# GHC User's manual

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/


1. The Glasgow Haskell Compiler

2. Introduction to GHC
  2.1. Obtaining GHC
  2.2. Meta-information: Web sites, mailing lists, etc.
  2.3. Reporting bugs in GHC
  2.4. GHC version numbering policy

3. Release notes for version 8.10.1
  3.1. Highlights
  3.2. Full details
    3.2.1. Language
    3.2.2. Compiler
    3.2.3. GHC API
    3.2.4. GHCi
    3.2.5. Runtime system
    3.2.6. Template Haskell
    3.2.7. ghc-prim library
    3.2.8. ghc library
    3.2.9. base library
    3.2.10. Build system
  3.3. Included libraries

4. **Using GHCi**
  4.1. Introduction to GHCi
  4.2. Loading source files
  4.2.1. Modules vs. filenames
  4.2.2. Making changes and recompilation
  4.3. Loading compiled code
  4.4. Interactive evaluation at the prompt
  4.4.1. I/O actions at the prompt
  4.4.2. Using do notation at the prompt
  4.4.3. Multiline input
  4.4.4. Type, class and other declarations
  4.4.5. What's really in scope at the prompt?
  4.4.5.1. The effect of :load on what is in scope
  4.4.5.2. Controlling what is in scope with import
  4.4.5.3. Controlling what is in scope with the :module command
  4.4.5.4. Qualified names
  4.4.5.5. :module and :load
  4.4.6. The :main and :run commands
  4.4.7. The it variable
  4.4.8. Type defaulting in GHCi
  4.4.8.1. Interactive classes
  4.4.8.2. Extended rules around default declarations
  4.4.9. Using a custom interactive printing function
  4.4.10. Stack Traces in GHCi
  4.5. The GHCi Debugger
  4.5.1. Breakpoints and inspecting variables
  4.5.1.1. Setting breakpoints
  4.5.1.2. Managing breakpoints
  4.5.2. Single-stepping
  4.5.3. Nested breakpoints
  4.5.4. The _result variable
  4.5.5. Tracing and history
  4.5.6. Debugging exceptions
  4.5.7. Example: inspecting functions
  4.5.8. Limitations
  4.6. Invoking GHCi
  4.6.1. Packages
  4.6.2. Extra libraries
  4.7. GHCi commands
  4.8. The :set and :seti commands
  4.8.1. GHCi options
  4.8.2. Setting GHC command-line options in GHCi
  4.8.3. Setting options for interactive evaluation only
  4.9. The .ghci and .haskeline files
  4.9.1. The .ghci files
  4.9.2. The .haskeline file
  4.10. Compiling to object code inside GHCi
  4.11. Running the interpreter in a separate process
  4.12. Running the interpreter on a different host
  4.13. FAQ and Things To Watch Out For

5. Using runghc
  5.1. Usage
  5.2. runghc flags
  5.3. GHC Flags

6. **Using GHC**
  6.1. Using GHC
  6.1.1. Getting started: compiling programs
  6.1.2. Options overview
  6.1.2.1. Command-line arguments
  6.1.2.2. Command line options in source files
  6.1.2.3. Setting options in GHCi
  6.1.3. Dynamic and Mode options
  6.1.4. Meaningful file suffixes
  6.1.5. Modes of operation
  6.1.5.1. Using ghc --make
  6.1.5.2. Expression evaluation mode
  6.1.5.3. Batch compiler mode
  6.1.6. Verbosity options
  6.1.7. Platform-specific Flags
  6.1.8. Miscellaneous flags
  6.1.8.1. Other environment variables
  6.2. Warnings and sanity-checking
  6.3. Optimisation (code improvement)
  6.3.1. -O*: convenient "packages" of optimisation flags.
  6.3.2. -f*: platform-independent flags
  6.4. Using Concurrent Haskell
  6.5. Using SMP parallelism
  6.5.1. Compile-time options for SMP parallelism
  6.5.2. RTS options for SMP parallelism
  6.5.3. Hints for using SMP parallelism
  6.6. Flag reference
  6.6.1. Verbosity options
  6.6.2. Alternative modes of operation
  6.6.3. Which phases to run
  6.6.4. Redirecting output
  6.6.5. Keeping intermediate files
  6.6.6. Temporary files
  6.6.7. Finding imports
  6.6.8. Interface file options
  6.6.9. Recompilation checking
  6.6.10. Interactive-mode options
  6.6.11. Packages
  6.6.12. Language options
  6.6.13. Warnings
  6.6.14. Optimisation levels
  6.6.15. Individual optimisations
  6.6.16. Profiling options
  6.6.17. Program coverage options
  6.6.18. C pre-processor options
  6.6.19. Code generation options
  6.6.20. Linking options
  6.6.21. Plugin options
  6.6.22. Replacing phases
  6.6.23. Forcing options to particular phases
  6.6.24. Platform-specific options
  6.6.25. Compiler debugging options
  6.6.26. Miscellaneous compiler options
  6.7. Running a compiled program
  6.7.1. Setting RTS options
  6.7.1.1. Setting RTS options on the command line
  6.7.1.2. Setting RTS options at compile time
  6.7.1.3. Setting RTS options with the GHCRTS environment variable
  6.7.1.4. "Hooks" to change RTS behaviour
  6.7.2. Miscellaneous RTS options
  6.7.3. RTS options to control the garbage collector
  6.7.4. RTS options to produce runtime statistics
  6.7.5. RTS options for concurrency and parallelism
  6.7.6. RTS options for profiling
  6.7.7. Tracing
  6.7.8. RTS options for hackers, debuggers, and over-interested souls
  6.7.9. Getting information about the RTS
  6.8. Filenames and separate compilation
  6.8.1. Haskell source files
  6.8.2. Output files
  6.8.3. The search path
  6.8.4. Redirecting the compilation output(s)
  6.8.5. Keeping Intermediate Files
  6.8.6. Redirecting temporary files
  6.8.7. Other options related to interface files
  6.8.8. Options related to extended interface files
  6.8.9. The recompilation checker
  6.8.10. How to compile mutually recursive modules
  6.8.11. Module signatures
  6.8.12. Using make
  6.8.13. Dependency generation
  6.8.14. Orphan modules and instance declarations
  6.9. Packages
  6.9.1. Using Packages
  6.9.2. The main package
  6.9.3. Consequences of packages for the Haskell language
  6.9.4. Thinning and renaming modules
  6.9.5. Package Databases
  6.9.5.1. The GHC_PACKAGE_PATH environment variable
  6.9.5.2. Package environments
  6.9.6. Installed package IDs, dependencies, and broken packages
  6.9.7. Package management (the ghc-pkg command)
  6.9.8. Building a package from Haskell source
  6.9.9. InstalledPackageInfo: a package specification
  6.10. GHC Backends
  6.10.1. Native Code Generator (-fasm)
  6.10.2. LLVM Code Generator (-fllvm)
  6.10.3. C Code Generator (-fvia-C)
  6.10.4. Unregisterised compilation
  6.11. Options related to a particular phase
  6.11.1. Replacing the program for one or more phases
  6.11.2. Forcing options to a particular phase
  6.11.3. Options affecting the C pre-processor
  6.11.3.1. Standard CPP macros
  6.11.3.2. CPP and string gaps
  6.11.4. Options affecting a Haskell pre-processor
  6.11.5. Options affecting code generation
  6.11.6. Options affecting linking
  6.12. Using shared libraries
  6.12.1. Building programs that use shared libraries
  6.12.2. Shared libraries for Haskell packages
  6.12.3. Shared libraries that export a C API
  6.12.4. Finding shared libraries at runtime
  6.12.4.1. Unix
  6.12.4.2. Mac OS X
  6.13. Debugging the compiler
  6.13.1. Dumping out compiler intermediate structures
  6.13.1.1. Front-end
  6.13.1.2. Type-checking and renaming
  6.13.1.3. Core representation and simplification
  6.13.1.4. STG representation
  6.13.1.5. C-\- representation
  6.13.1.6. LLVM code generator
  6.13.1.7. Native code generator
  6.13.1.8. Miscellaneous backend dumps
  6.13.2. Formatting dumps
  6.13.3. Suppressing unwanted information
  6.13.4. Checking for consistency
  6.13.5. Checking for determinism
  6.13.6. Other

7. Profiling
  7.1. Cost centres and cost-centre stacks
  7.1.1. Inserting cost centres by hand
  7.1.2. Rules for attributing costs
  7.2. Compiler options for profiling
  7.3. Time and allocation profiling
  7.3.1. JSON profile format
  7.4. Profiling memory usage
  7.4.1. RTS options for heap profiling
  7.4.2. Retainer Profiling
  7.4.2.1. Hints for using retainer profiling
  7.4.3. Biographical Profiling
  7.4.4. Actual memory residency
  7.5. hp2ps - Rendering heap profiles to PostScript
  7.5.1. Manipulating the hp file
  7.5.2. Zooming in on regions of your profile
  7.5.3. Viewing the heap profile of a running program
  7.5.4. Viewing a heap profile in real time
  7.6. Profiling Parallel and Concurrent Programs
  7.7. Observing Code Coverage
  7.7.1. A small example: Reciprocation
  7.7.2. Options for instrumenting code for coverage
  7.7.3. The hpc toolkit
  7.7.3.1. hpc report
  7.7.3.2. hpc markup
  7.7.3.3. hpc sum
  7.7.3.4. hpc combine
  7.7.3.5. hpc map
  7.7.3.6. hpc overlay and hpc draft
  7.7.4. Caveats and Shortcomings of Haskell Program Coverage
  7.8. Using "ticky-ticky" profiling (for implementors)

8. Advice on: sooner, faster, smaller, thriftier
  8.1. Sooner: producing a program more quickly
  8.2. Faster: producing a program that runs quicker
  8.3. Smaller: producing a program that is smaller
  8.4. Thriftier: producing a program that gobbles less heap space

9. **GHC Language Features**
  9.01. Language options
  9.02. Unboxed types and primitive operations
    9.2.1. Unboxed types
    9.2.2. Unboxed type kinds
    9.2.3. Unboxed tuples
    9.2.4. Unboxed sums
    9.2.5. Unlifted Newtypes
  9.03. Syntactic extensions
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
  9.04. Extensions to data types and type synonyms
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
  9.05. Extensions to the record system
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
  9.06. Extensions to the "deriving" mechanism
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
  9.07. Pattern synonyms
    9.7.1. Record Pattern Synonyms
    9.7.2. Syntax and scoping of pattern synonyms
    9.7.3. Import and export of pattern synonyms
    9.7.4. Typing of pattern synonyms
    9.7.5. Matching of pattern synonyms
  9.08. Class and instances declarations
    9.8.1. Class declarations
      9.8.1.1. Multi-parameter type classes
      9.8.1.2. The superclasses of a class declaration
      9.8.1.3. Constrained class method types
      9.8.1.4. Default method signatures
      9.8.1.5. Nullary type classes
    9.8.2. Functional dependencies
      9.8.2.1. Rules for functional dependencies
      9.8.2.2. Background on functional dependencies
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
      9.8.6.1. The `IsList` class
      9.8.6.2. Rebindable syntax
      9.8.6.3. Defaulting
      9.8.6.4. Speculation about the future
    9.8.7. Undecidable (or recursive) superclasses
  9.09. Type families
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

10. Foreign function interface (FFI)
  10.1. GHC differences to the FFI Chapter
  10.1.1. Guaranteed call safety
  10.2. GHC extensions to the FFI Chapter
  10.2.1. Unlifted FFI Types
  10.2.2. Newtype wrapping of the IO monad
  10.2.3. Explicit "forall"s in foreign types
  10.2.4. Primitive imports
  10.2.5. Interruptible foreign calls
  10.2.6. The CAPI calling convention
  10.2.7. hs_thread_done()
  10.2.8. Freeing many stable pointers efficiently
  10.3. Using the FFI with GHC
  10.3.1. Using foreign export and foreign import ccall "wrapper" with GHC
  10.3.1.1. Using your own main()
  10.3.1.2. Making a Haskell library that can be called from foreign code
  10.3.2. Using header files
  10.3.3. Memory Allocation
  10.3.4. Multi-threading and the FFI
  10.3.4.1. Foreign imports and multi-threading
  10.3.4.2. The relationship between Haskell threads and OS threads
  10.3.4.3. Foreign exports and multi-threading
  10.3.4.4. On the use of hs_exit()
  10.3.4.5. Waking up Haskell threads from C
  10.3.5. Floating point and the FFI
  10.3.6. Pinned Byte Arrays

11. Extending and using GHC as a Library
  11.1. Source annotations
  11.1.1. Annotating values
  11.1.2. Annotating types
  11.1.3. Annotating modules
  11.2. Using GHC as a Library
  11.3. Compiler Plugins
  11.3.1. Using compiler plugins
  11.3.2. Writing compiler plugins
  11.3.3. Core plugins in more detail
  11.3.3.1. Manipulating bindings
  11.3.3.2. Using Annotations
  11.3.4. Typechecker plugins
  11.3.4.1. Constraint solving with plugins
  11.3.5. Source plugins
  11.3.5.1. Parsed representation
  11.3.5.2. Type checked representation
  11.3.5.3. Evaluated code
  11.3.5.4. Interface files
  11.3.5.5. Source plugin example
  11.3.6. Hole fit plugins
  11.3.6.1. Stateful hole fit plugins
  11.3.6.2. Hole fit plugin example
  11.3.7. Controlling Recompilation
  11.3.8. Frontend plugins
  11.3.9. DynFlags plugins

12. What to do when something goes wrong
  12.1. When the compiler "does the wrong thing"
  12.2. When your program "does the wrong thing"

13. Debugging compiled programs
  13.1. Tutorial
  13.2. Requesting a stack trace from Haskell code
  13.3. Requesting a stack trace with SIGQUIT
  13.4. Implementor's notes: DWARF annotations
  13.4.1. Debugging information entities
  13.4.1.1. DW_TAG_ghc_src_note
  13.5. Further Reading

14. Other Haskell utility programs
  14.1. "Yacc for Haskell": happy
  14.2. Writing Haskell interfaces to C code: hsc2hs
  14.2.1. command line syntax
  14.2.2. Input syntax
  14.2.3. Custom constructs
  14.2.4. Cross-compilation

15. Running GHC on Win32 systems
  15.1. Starting GHC on Windows platforms
  15.2. Running GHCi on Windows
  15.3. Interacting with the terminal
  15.4. Differences in library behaviour
  15.5. File paths under Windows
  15.6. Using GHC (and other GHC-compiled executables) with Cygwin
  15.6.1. Background
  15.6.2. The problem
  15.6.3. Things to do
  15.7. Building and using Win32 DLLs
  15.7.1. Creating a DLL
  15.7.2. Making DLLs to be called from other languages
  15.7.2.1. Using from VBA
  15.7.2.2. Using from C++

16. Known bugs and infelicities
  16.1. Haskell standards vs. Glasgow Haskell: language non-compliance
  16.1.1. Divergence from Haskell 98 and Haskell 2010
  16.1.1.1. Lexical syntax
  16.1.1.2. Context-free syntax
  16.1.1.3. Expressions and patterns
  16.1.1.4. Declarations and bindings
  16.1.1.5. Typechecking of recursive binding groups
  16.1.1.6. Default Module headers with -main-is
  16.1.1.7. Module system and interface files
  16.1.1.8. Numbers, basic types, and built-in classes
  16.1.1.9. In Prelude support
  16.1.1.10. The Foreign Function Interface
  16.1.1.11. Operator sections
  16.1.2. GHC's interpretation of undefined behaviour in Haskell 98 and Haskell 2010
  16.2. Known bugs or infelicities
  16.2.1. Bugs in GHC
  16.2.2. Bugs in GHCi (the interactive GHC)

17. Eventlog encodings
  17.1. Heap profiler event log output
  17.1.1. Metadata event types
  17.1.1.1. Beginning of sample stream
  17.1.1.2. Cost centre definitions
  17.1.2. Sample event types
  17.1.2.1. Cost-centre break-down
  17.1.2.2. String break-down
  17.2. Time profiler event log output
  17.2.1. Profile begin event
  17.2.2. Tick sample event

18. Care and feeding of your GHC User's Guide
  18.1. Basics
  18.1.1. Headings
  18.1.2. Formatting code
  18.1.2.1. Haskell
  18.1.2.2. Other languages
  18.1.3. Links
  18.1.3.1. Within the User's Guide
  18.1.3.2. To GHC resources
  18.1.3.3. To external resources
  18.1.3.4. To core library Haddock documentation
  18.1.3.5. Math
  18.1.4. Index entries
  18.2. Citations
  18.3. Admonitions
  18.4. Documenting command-line options and GHCi commands
  18.4.1. Command-line options
  18.4.2. GHCi commands
  18.5. Style Conventions
  18.6. ReST reference materials
