# Index of monadic type classes

Monadic type classes are classes with `Monad` class as their superclass, and whose name is prefixed with `Monad`. For example, the class `MonadFail` is in the module `Control.Monad.Fail` (so drop 'Control' and the dot to get the class name: Control.Monad.Fail ~~> Monad.Fail ~~> MonadFail).

Monadic *type classes*
- `Monad`       Class of monads
- `MonadPlus`   Class of monad that also support choice and failure
- `MonadTrans`  Class of monad transformers
- `MonadBase`   Class of monads that lift transformer stack computations
- `MonadIO`     Class of monads that are based on IO
- `MonadZip`    Class of monadic zippers (used for monad comprehensions) 
- `MonadFix`    Class of monads with fixpoint, knot-tying, semantics
- `MonadRWS`    Class of monads that encompass Reader, Writer and State
- `MonadReader` Class of monads with access to a readonly shared environment
- `MonadWriter` Class of monads which can log output
- `MonadState`  Class of monads which model mutable state
- `MonadCont`   Computations which can be interrupted and resumed
- `MonadFail`   Class of monads that can fail a patter match
- `MonadError`  Class of monads with error handling
- `MonadExcept` ? Class of monads with error handling
- `MonadCatch`  Class of monads that allow catching exceptions
- `MonadThrow`  Class for monads that can throw extensible exceptions
- `MonadMask`   Class for monads which can mask asynchronous exceptions


The `MonadMask` type class groups monads which provide for the ability to account for all possible exit points from a computation, and to mask asynchronous exceptions. Continuation-based monads are invalid instances of this class.


Control.Monad.IO.Class
Control.Monad.Cont.Class
Control.Monad.Random.Class
Control.Monad.Reader.Class
Control.Monad.Writer.Class
Control.Monad.State.Class
Control.Monad.RWS.Class
Control.Monad.Trans.Class


* Used to find all `Monad*` classes
https://hackage-search.serokell.io/?q=%5Eclass%5Cs%2BMonad%5Cw%2B%5Cs%2B
