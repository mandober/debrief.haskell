# Modules

<!-- TOC -->

- [Importing modules](#importing-modules)
- [Qualified imports](#qualified-imports)
- [Import aliasing](#import-aliasing)
- [Example: import](#example-import)

<!-- /TOC -->




## Importing modules

- importing modules brings items into the current module's scope
- imported modules are top-level declarations with global scope
- imported entities in top/global scope can be shadowed by locals
- multiple declaration importing is cumulative
- the ordering of import declarations is irrelevant
- an entity is in scope for whole module if imported by any import declaration
- `-XNoImplicitPrelude` pragma disables implicit Prelude import



In ghci, to **import everything** a module exports:

    import Data.Bool


**Selective import** from Data.Bool, only import bool:

    import Data.Bool (bool)


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
