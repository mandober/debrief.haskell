# Stream-related hackages

`Stream`
https://hackage.haskell.org/package/Stream
This package implements functions, analogous to those from `Data.List`, to create and manipulate infinite lists, `data Stream a = Cons a (Stream a)`. It provides alternative definitions for those Prelude functions that make sense for such streams (this package has almost nothing to do with the work on Stream Fusion by Duncan Coutts, Roman Leshchinskiy, and Don Stewart).
