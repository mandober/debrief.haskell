# Prelude

https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.12.0.0/Prelude.html

The Prelude: a standard module.

The Prelude is imported by default into all Haskell modules unless either:
- there is an explicit import statement for it, or 
- the `NoImplicitPrelude` extension is enabled

To start ghci withou the prelude: `ghci -XNoImplicitPrelude`

## Prelude Contents

* Prelude
  * Standard types, classes and related functions
    * Basic data types
      - Tuples
    * Basic type classes
    * Numbers
      - Numeric types
      - Numeric type classes
      - Numeric functions
    * Semigroups and Monoids
    * Monads and functors
    * Folds and traversals
    * Miscellaneous functions
  * List operations
    * Special folds
    * Building lists
      - Scans
      - Infinite lists
    * Sublists
    * Searching lists
    * Zipping and unzipping lists
    * Functions on strings
  * Converting to and from String
    - Converting to String
    - Converting from String
  * Basic Input and output
    * Simple I/O operations
      - Output functions
      - Input functions
      - Files
    * Exception handling in the I/O monad
