---
downloaded:       2022-01-01
page-url:         https://wiki.haskell.org/Monad_Transformers
page-title:       Monad Transformers - HaskellWiki
article-title:    Monad Transformers - HaskellWiki
---
# Monad Transformers - HaskellWiki

There are currently several packages that implement similar interfaces to monad transformers (besides an additional package with a similar goal but different API named MonadLib):
There are currently several packages that implement similar interfaces to [monad transformers][1] (besides an additional package with a similar goal but different API named [MonadLib][2]):

-   [transformers][3]: provides the classes `MonadTrans` and `MonadIO`, as well as concrete monad transformers such as `StateT`. The monad `State s a` is only a type synonym for `StateT s Identity a`. Thus both `State` and `StateT` can be accessed by the same methods like `put` and `get`. However, this only works if `StateT` is the top-most transformer in a monad transformer stack. This package is Haskell 98 and thus can be also used with [JHC][4].
-   [mtl][5] (Monad Transformer Library) comes in two versions:
    
    -   version 1 was the first implementation, containing the classes `MonadTrans` and `MonadIO`, concrete monad transformers such as `StateT` and [multi-parameter type classes][6] with [functional dependencies][7] such as `MonadState`. Monads like `State` and their transformer counterparts like `StateT` are distinct types and can be accessed uniformly only through a type class abstraction like `MonadState`. This version is now obsolete.
    -   version 2 re-exports the classes and monad transformers of the transformers package, and adds [multi-parameter type classes][8] with [functional dependencies][9] such as `MonadState`.
    
    Version 2 of the MTL has some small [incompatibilities][10] relative to version 1. See "[Upgrading from MTL 1 to MTL 2][11]" for instructions on how to make code written for version 1 work with version 2.
    

Because of the functional dependencies, MTL can currently (2010-03) only used in [Hugs][12] and [GHC][13]. MTL was the first implementation.

-   [monads-fd][14]: this was the prototype of the new mtl implementation. It is now obsolete, and simply re-exports mtl version 2.
-   [monads-tf][15]: Provides a different abstraction using [type families][16]. Unfortunately the module names of `mtl` and `monads-tf` clash, so you can currently not import both packages in one package.

## How can I use MTL and transformers together?

MTL and transformers use different module names, but share common classes, type constructors and functions, so they are fully compatible.

## Shall I use MTL or transformers?

Transformers is Haskell 98 and thus more portable, and doesn't tie you to functional dependencies. But because it lacks the monad classes, you'll have to lift operations to the composite monad yourself ([examples][17]).

## How to move from MTL to transformers?

Many package using `MTL` can be ported to `transformers` with only slight modifications. Modules require the `Trans` infix, e.g. `import Control.Monad.State ...` must be replaced by `import Control.Monad.Trans.State ...`. Since `State` is only a type synonym, there is no longer a constructor named `State`. For constructing you must use the function `state` and instead of matching patterns you must call `runState`.

## See also

-   [Monad Transformers Explained][18]
-   [Monad Transformers Step by Step][19]
-   [All About Monads][20]
-   [http://www.haskell.org/pipermail/libraries/2009-March/011415.html][21]
-   [http://www.haskell.org/pipermail/libraries/2009-December/012914.html][22]
-   [http://www.haskell.org/pipermail/haskell-cafe/2010-January/071842.html][23]
-   [http://www.mail-archive.com/debian-haskell@lists.debian.org/msg01241.html][24]

[1]: https://wiki.haskell.org/Monad_Transformers_Explained "Monad Transformers Explained"
[2]: https://wiki.haskell.org/MonadLib "MonadLib"
[3]: http://hackage.haskell.org/package/transformers
[4]: https://wiki.haskell.org/JHC "JHC"
[5]: http://hackage.haskell.org/package/mtl
[6]: https://wiki.haskell.org/Multi-parameter_type_class "Multi-parameter type class"
[7]: https://wiki.haskell.org/Functional_dependencies "Functional dependencies"
[8]: https://wiki.haskell.org/Multi-parameter_type_class "Multi-parameter type class"
[9]: https://wiki.haskell.org/Functional_dependencies "Functional dependencies"
[10]: https://wiki.haskell.org/Incompatibilities_between_MTL_1_and_MTL_2 "Incompatibilities between MTL 1 and MTL 2"
[11]: https://wiki.haskell.org/Upgrading_from_MTL_1_to_MTL_2 "Upgrading from MTL 1 to MTL 2"
[12]: https://wiki.haskell.org/Hugs "Hugs"
[13]: https://wiki.haskell.org/GHC "GHC"
[14]: http://hackage.haskell.org/package/monads-fd
[15]: http://hackage.haskell.org/package/monads-tf
[16]: https://wiki.haskell.org/Type_families "Type families"
[17]: http://hackage.haskell.org/packages/archive/transformers/latest/doc/html/Control-Monad-Trans-Class.html#g:2
[18]: https://wiki.haskell.org/Monad_Transformers_Explained "Monad Transformers Explained"
[19]: https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf
[20]: https://wiki.haskell.org/All_About_Monads "All About Monads"
[21]: http://www.haskell.org/pipermail/libraries/2009-March/011415.html
[22]: http://www.haskell.org/pipermail/libraries/2009-December/012914.html
[23]: http://www.haskell.org/pipermail/haskell-cafe/2010-January/071842.html
[24]: http://www.mail-archive.com/debian-haskell@lists.debian.org/msg01241.html
