# Stream-related hackages

* `infinite-list`
- https://hackage.haskell.org/package/infinite-list-0.1
- Modern lightweight library for infinite lists with fusion:
- API similar to Data.List.
- No non-boot dependencies.
- Top performance, driven by fusion.
- Avoid dangerous instances like Foldable.
- Use NonEmpty where applicable.
- Use Word for indices.
- Be lazy, but not too lazy.

* `Data.Stream.Infinite` from `streams` package:
- Large dependency footprint, e. g., adjunctions.
- Provides dangerous instances such as Foldable.
- No fusion framework.
- https://hackage.haskell.org/package/streams/docs/Data-Stream-Infinite.html
- https://hackage.haskell.org/package/streams

* `Data.Stream` from `Stream` package:
- No fusion framework.
- No repository or issue tracker.
- https://hackage.haskell.org/package/Stream/docs/Data-Stream.html
- https://hackage.haskell.org/package/Stream
This package implements functions, analogous to those from `Data.List`, to create and manipulate infinite lists, `data Stream a = Cons a (Stream a)`. It provides alternative definitions for those Prelude functions that make sense for such streams (this package has almost nothing to do with the work on Stream Fusion by Duncan Coutts, Roman Leshchinskiy, and Don Stewart).

* `GHC.Data.List.Infinite` in GHC source tree:
- Limited API, only to cater for GHC internals.
- Not available as a separate package outside of GHC.
- https://gitlab.haskell.org/ghc/ghc/-/blob/080fffa1015bcc0cff8ab4ad1eeb507fb7a13383/compiler/GHC/Data/List/Infinite.hs
