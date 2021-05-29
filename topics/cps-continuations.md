# Continuations

- continuations use existential types, which are denoted in Haskell by reusing the `forall` keyword in a nested scope, enabled by the `RankNTypes` GHC pragma
- rank-N types are about the interplay of toggling the control between the caller and the callee.
- rank of a type is the depth at which the `forall` keyword appears



## Continuation Monad

The isomorphism between the type `a` and `forall r. (a -> r) -> r` is witnessed by the following pair of functions that convert between them:

```hs
-- a to the k
cont :: forall a. (a -> (forall r. (a -> r) -> r))
cont a = \k -> k a

-- k to the a
runCont :: forall a. ((forall r. (a -> r) -> r) -> a)
runCont f = let k = id in f k
```

(un)intuitively, this says that having a return value (`a`) is just as good as applying a callback to it (?) The `forall r. (a -> r) -> r` is in CP style i.e. continuation-passing style (CPS).

Isomorphisms are transitive: `(t1 ≅ t2) ∧ (t2 ≅ t3) ==> t1 ≅ t3`, so these three are isomorphic between themselves:

`∀a.a`    ≅    `∀a. Identity a`    ≅    `∀a. (∀r. (a -> r) -> r)`

Since we know that `Identity a` is a Monad and that isomorphisms preserve classes, we expect CPS also forms a Monad.

```hs
-- | This one repr both, `a` and `Identity a` functor - they are
-- exactly the same if we disregard the obligatory data ctor noise
newtype Identity a = Identity { runIdentity :: a }

newtype Cont a = Cont { runCont :: forall r. (a -> r) -> r }
```








# Continuations

* Continuations have the form `(a -> r) -> r`, where `a` is the output from the continuation and `r` is the final result after the callback is applied.

`forall r. (a -> r) -> r`


* Since isomorphisms are transitive: `Identity a ≅ a ≅ forall r. (a -> r) -> r`,and since `Identity a` is a monad, and isomorphisms preserve classes, we expect that continuation is also a monad.

`Identity a ≅ a ≅ forall r. (a -> r) -> r`

`a ≅ forall r. (a -> r) -> r`


```hs
-- continuation signature: forall r. (a -> r) -> r

type Cont a = forall r. (a -> r) -> r

newtype Cont a = Cont { unCont :: forall r. (a -> r) -> r }

-- continuations
twoC   = \k -> k 2
helloC = \k -> k "hello"

-- id is often used as a cont
twoC   id -- 2
helloC id -- "hello"

-- create cont taking function
mkcont f = \k -> k f
mkcont f k = k f

-- create func that take continuations
twoC'   = mkcont 2
helloC' = mkcont "hello"

-- pair as func
fpair a b k = k a b
mkProj k p = p k
p1 = fpair 1 2
car = mkProj const p1
cdr = mkProj (flip const) p1


-- bind takes a cont and a function which is provided the value of it
-- and returns a new continuation as a result:
inC `bind` fn = \out -> inC (\inCval -> (fn inCval) out)

-- a simple example that doubles the value handed back from twoC:
fourC = twoC `bind` \two -> ret (two*2)

-- a more complex example combining the results of two continuations:
twoHelloC = twoC `bind` \two ->
              helloC `bind` \hello ->
                ret $ (show two)++hello
The result of running these new continuations is:

fourC id == 4
twoHelloC id == "2hello"
```










## References

https://jsdw.me/posts/haskell-cont-monad/
https://wiki.haskell.org/Continuation
https://en.wikipedia.org/wiki/Continuation
http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/
http://logic.cs.tsukuba.ac.jp/cw2011/tutorial.html
http://okmij.org/ftp/continuations/#tutorial
