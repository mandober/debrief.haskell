---
downloaded:       2022-01-01
page-url:         https://wiki.haskell.org/New_monads
page-title:       New monads - HaskellWiki
article-title:    New monads - HaskellWiki
---
# New monads - HaskellWiki

Remember to add a [ [ Category:Code ] ] tag to any new sub-pages.
Remember to add a \[ \[ Category:Code \] \] tag to any new sub-pages.

## MonadBase

It seems that the liftIO function from MonadIO can be generalized to access whatever the base of a transformer stack happens to be. So there is no need for a liftSTM, liftST, etc.

View [New monads/MonadBase][1].

## MonadLib

[MonadLib][2] is written by Iavor S. Diatchki.

It is a new version of the mtl package with base monads: Id, and Lift, and transformers ReaderT, WriterT, StateT, ExceptionT, ChoiceT, and ContT.

It also defines BaseM which is like MonadBase above.

## MonadRandom

A simple monad transformer to allow computations in the transformed monad to generate random values.

View [New monads/MonadRandom][3].

### MonadRandomSplittable

A refinement of MonadRandom to integrate RandomGen's split function.

View at [New monads/MonadRandomSplittable][4]

## MaybeT

The Maybe monad deserves a transformer, just like the other classic monads.

View [New monads/MaybeT][5].

## MonadSupply

Here is a simple monad/monad transformer for computations which consume values from a (finite or infinite) supply. Note that due to pattern matching, running out of supply in a non-MonadZero monad will cause an error.

View [New monads/MonadSupply][6].

## MonadUndo

Here is a modified state monad transformer for keeping track of undo/redo states automatically.

View [New monads/MonadUndo][7].

## MonadUnique

This is a simple (trivial) monad transformer for supplying unique integer values to an algorithm.

View [New monads/MonadUnique][8].

## MonadSTO

Here's an extension of the ST monad in which the references are ordered and showable (they list their creation index).

View [New monads/MonadSTO][9].

## MonadNondet

There is a [MonadNondet][10] that when compiled with optimizations outperforms List.

## Stateful nondeterminism

There is a [Stateful nondeterminism][11] monad for if you want to do nondeterministic computation with local states for each of your threads and a global state shared by all your threads.

## MonadAdvSTM

Here is an extension of STM to easy interaction with IO after committing or retrying. Inspired by Simon P-J.

View [New monads/MonadAdvSTM][12].

## TimedStateT

A monad transformer which combines State, Reader, and Error functionality to give the effect of a StateT monad which checks clock-time and stops the current computation if a period is exceeded.

darcs get [http://www.mapcar.org/haskell/TimedStateT/][13]

Haddocks: [http://www.mapcar.org/haskell/TimedStateT/dist/doc/html/][14]

## MonadExit

The Exit monad provides [short-circuiting][15] for complex program flow logic.

If you are using CPS (either explicitly, or in a CPS-based monad such as MonadCont or LogicT) only for this purpose, the Exit monad will likely simplify your program considerably.

**Note:** Now that a restriction on the Left type has been removed, the standard `Either` type can be used for this purpose. No separate Exit monad is needed anymore. For a monad transformer, use the version of EitherT defined in the [either][16] package.

View [MonadExit][17].

## MonadSplit

Represents the class of monads such that

l \== (msplit l \>>= \\(x,xs) \-> return x \`mplus\` xs)

In English, msplit is a counterpart to "mplus".

Using this, you can redefine many of the functions which previously depended on lists: foldM, scanM, inits, tails, and some derived functions.

Note: A more general form of this monad, [Data.Foldable][18], is now part of the [standard libraries][19].

View [New monads/MonadSplit][20].

## Lazy and Strict variants

This section contains monads that have interesting Strict or Lazy properties.

### LazyWriterT

This came up on the mailing list: Why is WriterT never lazy? The answer is it does not use lazy patterns with "~". So here is a more useful [New monads/LazyWriterT][21] that add two "~" to the definition of (>>=) and renames WriterT to LazyWriterT.

### Strict RWS

This was contribute by John Meacham on on the haskell-cafe mailing list. [New monads/UnboxedRWS][22] is an strict variant of RWS.

[1]: https://wiki.haskell.org/New_monads/MonadBase "New monads/MonadBase"
[2]: https://wiki.haskell.org/MonadLib "MonadLib"
[3]: https://wiki.haskell.org/New_monads/MonadRandom "New monads/MonadRandom"
[4]: https://wiki.haskell.org/New_monads/MonadRandomSplittable "New monads/MonadRandomSplittable"
[5]: https://wiki.haskell.org/New_monads/MaybeT "New monads/MaybeT"
[6]: https://wiki.haskell.org/New_monads/MonadSupply "New monads/MonadSupply"
[7]: https://wiki.haskell.org/New_monads/MonadUndo "New monads/MonadUndo"
[8]: https://wiki.haskell.org/New_monads/MonadUnique "New monads/MonadUnique"
[9]: https://wiki.haskell.org/New_monads/MonadSTO "New monads/MonadSTO"
[10]: https://wiki.haskell.org/Sudoku#Monadic_Non-Deterministic_Solver "Sudoku"
[11]: https://wiki.haskell.org/Stateful_nondeterminism "Stateful nondeterminism"
[12]: https://wiki.haskell.org/New_monads/MonadAdvSTM "New monads/MonadAdvSTM"
[13]: http://www.mapcar.org/haskell/TimedStateT/
[14]: http://www.mapcar.org/haskell/TimedStateT/dist/doc/html/
[15]: https://wiki.haskell.org/Short-circuiting "Short-circuiting"
[16]: http://hackage.haskell.org/package/either
[17]: https://wiki.haskell.org/New_monads/MonadExit "New monads/MonadExit"
[18]: http://haskell.org/ghc/docs/latest/html/libraries/base/Data-Foldable.html
[19]: http://haskell.org/ghc/docs/latest/html/libraries/
[20]: https://wiki.haskell.org/New_monads/MonadSplit "New monads/MonadSplit"
[21]: https://wiki.haskell.org/New_monads/LazyWriterT "New monads/LazyWriterT"
[22]: https://wiki.haskell.org/New_monads/UnboxedRWS "New monads/UnboxedRWS"
