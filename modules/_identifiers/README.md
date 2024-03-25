# Haskell :: Names per module

## Content

- [Data.Aeason](./Data.Aeason.md)
- [Data.Char](./Data.Char.md)
- [Data.List](./Data.List.md)
- [Data.Map](./Data.Map.md)
- [Data.String](./Data.String.md)
- [Data.Vector](./Data.Vector.md)


## Importing

- Import each module `qualified`
- and aliase it `as` a short name, to act as a prefix
- e.g. `import Data.Set qualified as S`
- for `Data` modules, import the type ctor directly to avoid having to write
  the prefix in the types, `import Data.Set (Set)`
- import any infix symbolic operator directly to avoid prefixing it
  rather then (x `S.\\` y) do `import Data.Set ((\\))` then (x \\ y)

```hs
import Data.Set qualified as S

import Data.Set (Set)
import Data.Set ((\\))

_ = xs `S.\\` ys

import Data.Set (Set, (\\))
```


## Imports

```hs
import Prelude        qualified as P

import Data.List      qualified as L
import Data.Set       qualified as S
import Data.Map       qualified as M
import Data.Map.Lazy  qualified as ML
import Data.IntMap    qualified as MZ
import Data.Array     qualified as A
import Data.Vector    qualified as V

import Data.Char      qualified as Ch
import Data.String    qualified as St

import Data.Aeason    qualified as Ae
```



## Getting the lists

Lists of names exported from a module.
A way to get these lists is to use 
the usual practice when importing `Data` modules 
that often export functions with the same name 
[why_not_just_ad_hoc_them_into_classes?]
Import, esp Data, modules qualified 
and aliased to a short name. 
then ask ghci for complitions


```hs
-- import the type ctor directly - to avoid writing S.Set in types
import Data.Set (Set)
-- import the rest under an alias - write functions as S.lines
import Data.Set qualified as S

import Data.Char   qualified as DC
import Data.String qualified as DS


-- hide list-related names from Prelude
import Prelude hiding
  (break, cycle, drop, dropWhile, filter, foldl, foldr, head, init, iterate, last, length, map, repeat, reverse, scanl, scanl1, scanr, scanr1, span, splitAt, tail, take, takeWhile, unzip, zip, zipWith, (!!))

-- import list ctor directly
import Data.List ([])
-- import list-related names as L
import Data.List qualified as L
```
