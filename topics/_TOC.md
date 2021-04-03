# Topics



Lazy Evaluation
Declaring Values
Overloaded Literals
Strings
Floating Numeral
Integer Numeral
List Literals
Foldable
An instance of Foldable for a binary tree
Counting the elements of a Foldable structure
Folding a structure in reverse
Flattening a Foldable structure into a list
Performing a side-effect for each element of a Foldable structure
Flattening a Foldable structure into a Monoid
Checking if a Foldable structure is empty

Traversable
Definition of Traversable
Traversing a structure in reverse
An instance of Traversable for a binary tree
Traversable structures as shapes with contents
Instantiating Functor and Foldable for a Traversable structure
Transforming a Traversable structure with the aid of an accumulating parameter
Transposing a list of lists

Lens
Lenses for records
Manipulating tuples with Lens
Lens and Prism
Stateful Lenses
Lenses compose
Writing a lens without Template Haskell
Fields with makeFields
Classy Lenses
Traversals

QuickCheck
Declaring a property
Randomly generating data for custom types
Using implication (==>) to check properties with preconditions
Checking a single property
Checking all the properties in a file
Limiting the size of test data

Common GHC Language Extensions
RankNTypes
OverloadedStrings
BinaryLiterals
ExistentialQuantification
LambdaCase
FunctionalDependencies
FlexibleInstances
GADTs
TupleSections
OverloadedLists
MultiParamTypeClasses
UnicodeSyntax
PatternSynonyms
ScopedTypeVariables
RecordWildCards
Free Monads
Free monads split monadic computations into data structures and interpreters
The Freer monad
How do foldFree and iterM work?
Free Monads are like fixed points
Type Classes
Eq
Monoid
Ord
Num
Maybe and the Functor Class
Type class inheritance: Ord type class
IO
Getting the 'a' "out of" 'IO a'
IO defines your program's `main` action
Checking for end-of-file conditions
Reading all contents of standard input into a string
Role and Purpose of IO
Writing to stdout
Reading words from an entire file
Reading a line from standard input
Reading from `stdin`
Parsing and constructing an object from standard input
Reading from file handles
Record Syntax
Basic Syntax
Defining a data type with field labels
RecordWildCards
Copying Records while Changing Field Values
Records with newtype
Partial Application
Sections
Partially Applied Adding Function
Returning a Partially Applied Function 
Monoid
An instance of Monoid for lists
Collapsing a list of Monoids into a single value
Numeric Monoids
An instance of Monoid for ()
Category Theory
Category theory as a system for organizing abstraction
Haskell types as a category
Definition of a Category
Coproduct of types in Hask
Product of types in Hask
Haskell Applicative in terms of Category Theory
Lists
List basics
Processing lists
Ranges
List Literals
List Concatenation
Accessing elements in lists
Basic Functions on Lists
Transforming with `map`
Filtering with `filter`
foldr
Zipping and Unzipping Lists
foldl
Sorting Algorithms
Insertion Sort
Permutation Sort
Merge Sort
Quicksort
Bubble sort
Selection sort
Type Families
Datatype Families
Type Synonym Families
Injectivity
Monads
Definition of Monad
No general way to extract value from a monadic computation
Monad as a Subclass of Applicative
The Maybe monad
IO monad
List Monad
do-notation
Stack
Profiling with Stack
Structure
Build and Run a Stack Project
Viewing dependencies
Stack install
Installing Stack
Creating a simple project
Stackage Packages and changing the LTS (resolver) version
Generalized Algebraic Data Types
Basic Usage
Recursion Schemes
Fixed points
Primitive recursion
Primitive corecursion
Folding up a structure one layer at a time
Unfolding a structure one layer at a time
Unfolding and then folding, fused
Data.Text
Text Literals
Checking if a Text is a substring of another Text
Stripping whitespace
Indexing Text
Splitting Text Values
Encoding and Decoding Text
Using GHCi
Breakpoints with GHCi
Quitting GHCi
Reloading a already loaded file
Starting GHCi
Changing the GHCi default prompt
The GHCi configuration file
Loading a file
Multi-line statements
Strictness
Bang Patterns
Lazy patterns
Normal forms
Strict fields
Syntax in Functions
Pattern Matching
Using where and guards
Guards
Functor
Class Definition of Functor and Laws
Replacing all elements of a Functor with a single value
Common instances of Functor
Deriving Functor
Polynomial functors
Functors in Category Theory
Testing with Tasty
SmallCheck, QuickCheck and HUnit
Creating Custom Data Types
Creating a data type with value constructor parameters 
Creating a data type with type parameters
Creating a simple data type
Custom data type with record parameters
Reactive-banana
Injecting external events into the library
Event type
Actuating EventNetworks
Behavior type
Optimization
Compiling your Program for Profiling
Cost Centers
Concurrency
Spawning Threads with `forkIO`
Communicating between Threads with `MVar`
Atomic Blocks with Software Transactional Memory
Function composition
Right-to-left composition
Composition with binary function
Left-to-right composition
Databases
Postgres
Data.Aeson - JSON in Haskell
Smart Encoding and Decoding using Generics
A quick way to generate a Data.Aeson.Value
Optional Fields
Higher-order functions
Basics of Higher Order Functions
Lambda Expressions
Currying
Containers - Data.Map
Importing the Module
Monoid instance
Constructing
Checking If Empty
Finding Values
Inserting Elements
Deleting Elements
Fixity declarations
Associativity
Binding precedence
Example declarations
Web Development
Servant
Yesod
Vectors
The Data.Vector Module
Filtering a Vector
Mapping (`map`) and Reducing (`fold`) a Vector
Working on Multiple Vectors
Cabal
Working with sandboxes
Install packages
Type algebra
Addition and multiplication
Functions
Natural numbers in type algebra
Recursive types
Derivatives
Arrows
Function compositions with multiple channels
Typed holes
Syntax of typed holes
Semantics of typed holes
Using typed holes to define a class instance
Rewrite rules (GHC)
Using rewrite rules on overloaded functions
Date and Time
Finding Today's Date
Adding, Subtracting and Comparing Days
List Comprehensions
Basic List Comprehensions
Do Notation
Patterns in Generator Expressions
Guards
Parallel Comprehensions
Local Bindings
Nested Generators
Streaming IO
Streaming IO
Google Protocol Buers
Creating, building and using a simple .proto file
Template Haskell & QuasiQuotes
Syntax of Template Haskell and Quasiquotes
The Q type
An n-arity curry
Phantom types
Use Case for Phantom Types: Currencies
Modules
Defining Your Own Module
Exporting Constructors
Importing Specific Members of a Module
Hiding Imports
Qualifying Imports
Hierarchical module names
Tuples (Pairs, Triples, ...)
Extract tuple components
Strictness of matching a tuple
Construct tuple values
Write tuple types
Pattern Match on Tuples
Apply a binary function to a tuple (uncurrying)
Apply a tuple function to two arguments (currying)
Swap pair components
Graphics with Gloss
Installing Gloss
Getting something on the screen
State Monad
Numbering the nodes of a tree with a counter
Pipes
Producers
Connecting Pipes
Pipes
Running Pipes with runEect
Consumers
The Proxy monad transformer
Combining Pipes and Network communication
Infix operators
Prelude
Finding information about infix operators
Custom operators
Parallelism
The Eval Monad
rpar
rseq
Parsing HTML with taggy-lens and lens
Filtering elements from the tree
Extract the text contents from a div with a particular id
Foreign Function Interface
Calling C from Haskell
Passing Haskell functions as callbacks to C code
Gtk3
Hello World in Gtk
Monad Transformers
A monadic counter
Bifunctor
Definition of Bifunctor
Common instances of Bifunctor
first and second
Proxies
Using Proxy
The "polymorphic proxy" idiom
Proxy is like ()
Applicative Functor
Alternative definition
Common instances of Applicative 
Common monads as free monads
Free Empty ~~ Identity
Free Identity ~~ (Nat,) ~~ Writer Nat
Free Maybe ~~ MaybeT (Writer Nat)
Free (Writer w) ~~ Writer [w]
Free (Const c) ~~ Either c
Free (Reader x) ~~ Reader (Stream x)
Common functors as the base of cofree comonads
Cofree Empty ~~ Empty
Cofree (Const c) ~~ Writer c
Cofree Identity ~~ Stream
Cofree Maybe ~~ NonEmpty
Cofree (Writer w) ~~ WriterT w Stream
Cofree (Either e) ~~ NonEmptyT (Writer e)
Cofree (Reader x) ~~ Moore x
Arithmetic
Basic examples
`Could not deduce (Fractional Int) ...`
Function examples
Role
Nominal Role
Representational Role
Phantom Role
Arbitrary-rank polymorphism with RankNTypes
RankNTypes
GHCJS
Running "Hello World!" with Node.js
XML
Encoding a record using the `xml` library
Reader / ReaderT
Simple demonstration
Function call syntax
Partial application - Part 1
Partial application - Part 2
Parentheses in a basic function call
Parentheses in embedded function calls
Logging
Logging with hslogger
Attoparsec
Combinators
Bitmap - Parsing Binary Data
zipWithM
Calculatings sales prices
Profunctor
(->) Profunctor
Type Application
Avoiding type annotations
Type applications in other languages 
Order of parameters
Interaction with ambiguous types
