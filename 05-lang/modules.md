# Modules

<!-- TOC -->

- [Making modules](#making-modules)
- [Importing modules](#importing-modules)
- [Import all from a module](#import-all-from-a-module)
- [Disable loading the Prelude](#disable-loading-the-prelude)
- [Selective import](#selective-import)

<!-- /TOC -->

* Haskell module is a collection of related functions, types and typeclasses.
* Haskell program consists of a set of modules.
* Modules serve the purpose of controlling the namespaces
* Modules are declarations that contain other declarations:
    - data and type declarations
    - class and instance declarations
    - type signatures
    - function definitions
    - etc.
* namespace is a set of objects (functions, type classes)
  organized and contained so that they may be referenced by name, 
  without interference from other objects with the same name.
* namespace of modules is flat: declarations may appear in any order
  save for module header and import declarations)

A module always begins with a header with the keyword `module` and a module name, which may be followed by a list of entities that will be exported from that module.

```hs
module ModuleName
    ( func1
    , func2
    , func3
    ) where
```

A module's name should match the file name that contains the module, respecting the letter case.

The header is followed by a list of imports which can be empty if the module doesn't have any deps:

```hs
module ModuleName where
```


## Making modules

Refactoring the project by creating the new file and editing Main.hs

`/src/Hello.hs`

```hs
module Hello where
sayHello :: IO ()
sayHello = putStrLn "Hello from Haskell!"
```

`/app/Main.hs`

```hs
module Main where
import Hello
main :: IO ()
main = sayHello
```

Now rebuild and run the program: 
> cabal build


## Importing modules

Importing modules brings more functions into the current module's scope (beyond those available in the standard Prelude).

Imported modules are top-level declarations. The entities imported as part of those declarations, like other top-level declarations, have global scope throughout the module, but they can be shadowed by local bindings.

The effect of multiple import declarations is cumulative, but the ordering of import declarations is irrelevant. An entity is in scope for the entire module if it is imported by any of the import declarations.

## Import all from a module
In ghci, to import everything a module exports:
> import Data.Bool

## Disable loading the Prelude
Use `-XNoImplicitPrelude` pragma (language extension) when starting GHCi to turn Prelude off. It can be also done rightinthe source file. *how?*
> ghci -XNoImplicitPrelude

## Selective import
Selective import from Data.Bool, only import bool:
> import Data.Bool (bool)

Normally, in Prelude, `not` is in scope but `bool` isn't. By turning off Prelude loading, the standard functions are out of scope; then by importing only `bool`, we no longer have the standard `not` function in scope.

You can import one or more functions from a module or library. The import declarations have to be at the beginning of a module. Putting `import Data.Char (toUpper)` in the import declarations of a module will ensure that `toUpper` is in scope for your project, but not any of the other entities contained in `Data.Char`.

```hs
module Main where
import Data.Char (toUpper)
```

## Qualified imports
Using the qualified imports we can make the imported names more explicit. That way you can know where something you imported came from.

Use `qualified` keyword when importing a module.

Sometimes you’ll have stuff with the same name imported from two different modules, qualifying your imports is a common way of dealing with this. Here’s an example of how you might use a qualified import:

> import qualified Data.Bool
> :t bool
<interactive>:1:1:
Not in scope: ‘bool’
Perhaps you meant ‘Data.Bool.bool’ (imported from Data.Bool)
> :t Data.Bool.bool
Data.Bool.bool :: a -> a -> Data.Bool.Bool -> a
> :t Data.Bool.not
Data.Bool.not :: Data.Bool.Bool -> Data.Bool.Bool

When the import is qualified, like for `Data.Bool`, everything from Data.Bool is in scope but only when accessed with the full `Data.Bool` namespace.

## Import aliasing
To alias an import use the `as` keyword. We can alias modules upon qualified import so we don't have to type out the full namespace:
> import qualified Data.Bool as B
> :t B.bool
B.bool :: a -> a -> B.Bool -> a


## Example: import

```hs
module Example where

-- unqualified total
import Data.Bits

-- qualified total
import qualified Safe

-- unqualified selective
import Control.Monad (forever, when)

-- qualified total aliased
import qualified Control.Concurrent as CC


import qualified Data.ByteString.Char8      as B
import qualified Data.Locator               as DL
import qualified Data.Time.Clock.POSIX      as PSX
import qualified Control.Concurrent.MVar    as MV
import qualified Filesystem                 as FS
import qualified Filesystem.Path.CurrentOS  as FPC
import qualified Network.Info               as NI
import Database.Blacktip.Types
import Data.Bits.Bitwise (fromListBE)
import Data.List.Split (chunksOf)
import Control.Exception (mask, try)
import System.IO.Unsafe (unsafePerformIO)
```
