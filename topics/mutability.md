# Mutability

Mutable State in Haskell (Jakub Arnold, 2014)
https://blog.jakuba.net/2014-07-20-mutable-state-in-haskell/

Haskell is a purely functional language, which means there are no side-effects and all variables are immutable. While variables are indeed immutable, there are ways to construct mutable references where we can change what the reference points to.

Generally, mutability has to be observed (by another part of the code) for it to break referential transparency. If it goes by unobserved, none's the wiser.

Dealing with mutable state:
* IORef
* STRef in the ST monad
* MVar
* TVar in Software Transactional Memory (STM)
