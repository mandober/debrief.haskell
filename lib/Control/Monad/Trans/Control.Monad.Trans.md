# Control.Monad.Trans

- package: `transformers-0.5.6.2`
- hackage: https://hackage.haskell.org/package/transformers-0.5.6.2

This package contains:
- the monad transformer class in `Control.Monad.Trans.Class`
- concrete functor and monad transformers, each with associated operations and functions to `lift` operations associated with other transformers.

The package can be used on its own in portable Haskell code, in which case *operations need to be manually lifted through transformer stacks* (see `Control.Monad.Trans.Class` for some examples). Alternatively, it can be used with the non-portable monad classes in the `mtl` or `monads-tf` packages, which *automatically lift operations* introduced by monad transformers through other transformers.

Modules:
* Control
  * Applicative
    - Control.Applicative.Backwards
    - Control.Applicative.Lift
  * Monad
    * IO
      - Control.Monad.IO.Class
      - Control.Monad.Signatures

- Control.Monad.Trans.Class
- Control.Monad.Trans.Accum
- Control.Monad.Trans.Cont
- Control.Monad.Trans.Control
- Control.Monad.Trans.Error
- Control.Monad.Trans.Except
- Control.Monad.Trans.Identity
- Control.Monad.Trans.List
- Control.Monad.Trans.Maybe
- Control.Monad.Trans.Reader
- Control.Monad.Trans.RWS
  - Control.Monad.Trans.RWS.CPS
  - Control.Monad.Trans.RWS.Lazy
  - Control.Monad.Trans.RWS.Strict
- Control.Monad.Trans.Select
- Control.Monad.Trans.State
  - Control.Monad.Trans.State.Lazy
  - Control.Monad.Trans.State.Strict
- Control.Monad.Trans.Writer
  - Control.Monad.Trans.Writer.CPS
  - Control.Monad.Trans.Writer.Lazy
  - Control.Monad.Trans.Writer.Strict
