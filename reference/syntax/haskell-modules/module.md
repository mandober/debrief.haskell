# Module

- module
- module header
- namespace
- exports
  - individual functions, classes
  - recursive export `(..)` of all type ctors
- imports
  - qualified
  - aliased
  - re-exports
- module sources
  - internal
    - `Prelude`
    - `Data.*`
    - `GHC.*`
  - external, 3rd party
  - user-authored
- module scope
  - top scope
  - what can appear in top scope


# Module

- `Main`: uniquely named module that every program contains
- `main`: uniquely named function that every module contains
- which must always have the type: `main :: IO ()`
- Indentation is relevant: same indentation level - same block
- braces and semicolons are optional
- `putStrLn` prints a string arg (and a newline) to stdout (stdIO)
- `getLine` reads in a string from stdin (stdIO)
- `do` introduces a sequence of IO ops


Every program contains exactly one module named `Main`, which contains exactly one function named `main`. The `main` function always has the type `IO ()`, although, as usual, the type need not be explicitly annotated.

The `main` function is usually associated with the `do` keyword that introduces a sequence of IO operations. Curly braces and semicolons optional, but indentation does matters. All expressions (e.g. IO actions within the `do` block) that have the same indentation level are assumed to belong to the same block.


## Modules

- every file contains a module
- the name of file and the name of containing module has to match
- program is a set of modules
- module is a set of fns, types, classes available under one name
- primary module and the entry point into a program is `Main`
- `Main` imports 3rd party, internal, user-defined modules
- `Main` defines function `main :: IO ()`, which is the first function to run


```hs
-- filename: Mod.hs
module Mod
    ( f
    , ty(..)
    , g
    ) where
```


---
http://learnyouahaskell.com/modules
