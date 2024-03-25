# Haskell :: Monad transformers :: stdlib packages

* Monad_Transformers @HaskellWiki
- https://wiki.haskell.org/Monad_Transformers
- https://wiki.haskell.org/Monad_Transformers_Explained
- https://wiki.haskell.org/Upgrading_from_MTL_1_to_MTL_2
- https://wiki.haskell.org/Multi-parameter_type_class

- https://page.mi.fu-berlin.de/scravy/realworldhaskell/materialien/monad-transformers-step-by-step.pdf

- https://wiki.haskell.org/All_About_Monads

- http://www.haskell.org/pipermail/libraries/2009-March/011415.html
- http://www.haskell.org/pipermail/libraries/2009-December/012914.html
- http://www.haskell.org/pipermail/haskell-cafe/2010-January/071842.html

- http://www.mail-archive.com/debian-haskell@lists.debian.org/msg01241.html


* `mtl` package
  - https://hackage.haskell.org/package/mtl
  - http://github.com/haskell/mtl
  - git@github.com:haskell/mtl.git
  - MTL (monad transformers library) is a collection of monad classes, extending the 'transformers' package, using functional dependencies for generic lifting of monadic actions.
  - modules:
    - Control
      - Monad
        - Control.Monad.Accum
        - Control.Monad.Cont
          - Control.Monad.Cont.Class
        - Error
          - Control.Monad.Error.Class
        - Control.Monad.Except
        - Control.Monad.Identity
        - Control.Monad.RWS
          - Control.Monad.RWS.CPS
          - Control.Monad.RWS.Class
          - Control.Monad.RWS.Lazy
          - Control.Monad.RWS.Strict
        - Control.Monad.Reader
          - Control.Monad.Reader.Class
        - Control.Monad.Select
        - Control.Monad.State
          - Control.Monad.State.Class
          - Control.Monad.State.Lazy
          - Control.Monad.State.Strict
        - Control.Monad.Trans
        - Control.Monad.Writer
          - Control.Monad.Writer.CPS
          - Control.Monad.Writer.Class
          - Control.Monad.Writer.Lazy
          - Control.Monad.Writer.Strict

* `transformers` package
  - https://hackage.haskell.org/package/transformers
  - http://hub.darcs.net/ross/transformers
  - https://hackage.haskell.org/package/transformers/docs/Control-Monad-Trans-Writer-CPS.html
  - https://hackage.haskell.org/package/transformers-0.6.1.1/docs/Control-Monad-Trans-Writer-CPS.html
  - mtl is a portable library of functor and monad transformers, inspired by the paper *Functional Programming with Overloading and Higher-Order Polymorphism* by Mark P. Jones, 1995 (http://web.cecs.pdx.edu/~mpj/pubs/springschool.html).
  - The package contains the monad transformer class (in `Control.Monad.Trans.Class`), and concrete functor and monad transformers, each with associated operations and functions to lift ops associated with other transformers.
  - The package can be used on its own in portable Haskell code, in which case operations need to be manually lifted through transformer stacks (in `Control.Monad.Trans.Class`). Alternatively, it can be used with the non-portable monad classes in the *mtl* or *monads-tf* packages, which automatically lift operations introduced by monad transformers through other transformers.
  - https://hackage.haskell.org/package/transformers-0.6.1.1/docs/Control-Monad-Trans-Class.html
  - modules:
    - Control
      - Applicative
        - Control.Applicative.Backwards
        - Control.Applicative.Lift
      - Monad
        - IO
          - Control.Monad.IO.Class
        - Control.Monad.Signatures
      - Trans
        - Control.Monad.Trans.Accum
        - Control.Monad.Trans.Class
        - Control.Monad.Trans.Cont
        - Control.Monad.Trans.Except
        - Control.Monad.Trans.Identity
        - Control.Monad.Trans.Maybe
        - Control.Monad.Trans.RWS
          - Control.Monad.Trans.RWS.CPS
          - Control.Monad.Trans.RWS.Lazy
          - Control.Monad.Trans.RWS.Strict
        - Control.Monad.Trans.Reader
        - Control.Monad.Trans.Select
        - Control.Monad.Trans.State
          - Control.Monad.Trans.State.Lazy
          - Control.Monad.Trans.State.Strict
        - Control.Monad.Trans.Writer
          - Control.Monad.Trans.Writer.CPS
          - Control.Monad.Trans.Writer.Lazy
          - Control.Monad.Trans.Writer.Strict
    - Data
      - Functor
        - Data.Functor.Identity
        - Data.Functor.Constant
        - Data.Functor.Compose
        - Data.Functor.Reverse
        - Data.Functor.Sum
        - Data.Functor.Product
        - Data.Functor.Classes

- `monads-tf` package
  - https://hackage.haskell.org/package/monads-tf
  - https://github.com/typeclasses/monads-tf
  - Monad classes using type families, with instances for various monad transformers.
  - modules:
    - Control
      - Monad
        - Control.Monad.Cont
          - Control.Monad.Cont.Class
        - Control.Monad.Except
          - Control.Monad.Except.Class
        - Control.Monad.Identity
        - Control.Monad.RWS
          - Control.Monad.RWS.Class
          - Control.Monad.RWS.Lazy
          - Control.Monad.RWS.Strict
        - Control.Monad.Reader
          - Control.Monad.Reader.Class
        - Control.Monad.State
          - Control.Monad.State.Class
          - Control.Monad.State.Lazy
          - Control.Monad.State.Strict
        - Control.Monad.Trans
        - Control.Monad.Writer
          - Control.Monad.Writer.Class
          - Control.Monad.Writer.Lazy
          - Control.Monad.Writer.Strict

- `monads-fd` package
  - This was the prototype of the new `mtl` implementation. It is now obsolete, and simply re-exports `mtl-v2`.
