# Hiding things the imported module does not export

Technically, in [Haskell 2010][hs2010] this is illegal:

```hs
module A (f) where
  f = True

--- in the file B.hs
module B where

import A hiding (g)    -- but 'A' does not export nor define 'g'

g = f
```

The `import A hiding (g)` line in the `B` module is technically an error because the module `A` not only doesn't export, but it doesn't even define `g`.

However, GHC will allow it, in the interests of supporting backward compatibility (plus, it's sorta falls under the "vacuously true").

For example, if it happens that a newer version of `A` exports `g`, you would want `B` to work in either case.

The warning flag `-Wdodgy-imports`, which is *off by default*, will warn you if you hide something that the imported module doesn't export.


[hs2010]: http://www.haskell.org/onlinereport/haskell2010/haskellch5.html#x11-1020005.3.1
