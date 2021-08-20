# Kind system

* The meaning of the term "kind" is somewhere between "type of a type", and a mecnaism to specify arity of type constructors.

* The kind `Type` or `*` is the kind of any saturated type ctor; saturnated type ctors include fully applied type ctors, and nullary type ctors.
* The kind `#` is the kind of unlifted primitives.
* The kind `Constraint` is the kind that applies to everything on the LHS of the context fat arrow, including typeclass constraints.
* The kind `TYPE` is the most general kind that includes other kinds.


* A kind system is essentially a STLC "one level up". It has a base primitive kind/type `*`, and kind functions `* -> *`.
* Kind polymorphism uses a kind variable, `κ`, and kind functions, `κ -> κ`, also `forall κ. κ -> κ`, that eventually reach the base kind, `κ -> *`.
* `κ := * | κ -> κ` where `κ` is a kind variable

* Standalone kind signatures: 
`type Kleisli :: forall k. (k -> *) -> * -> k -> *`

* Syntactically, it is natural to consider polymorphic types to be type constructors, thus non-polymorphic types to be nullary type constructors. But all nullary constructors, thus all monomorphic types, have the same, simplest kind `*`.

* The kind signature `(* -> *) -> *` is impossible because a type/kind function, the `(* -> *)` part, cannot saturate a type ctor. It is similar to (a function with) a type signature `f :: (a -> a) -> a`, i.e. `f` function can accept a function argument, `(a -> a)`, but it cannot return an `a`. In fact, kind signatures, unlike type signatures, must eventually reach the base kind `Type`, so they all eventually end in `*` (however, a function can return a function, it is not required to eventually return a value of some base type).

```hs
-- monomorphic (takes a funarg and arg):
f :: (Int -> Int) -> Int -> Int
K :: (* -> *) -> * -> *

-- monomorphic and flipped
f :: Int -> (Int -> Int) -> Int
K :: * -> (* -> *) -> *         -- cannot flip a kind function!



k :: (* -> *) -> * -> *
f :: (a -> a) -> a -> a



-- more generic
f :: (a -> b) -> a -> b
k :: (κ -> *) -> κ -> *

-- more generic with flipped args
f :: (a -> b) -> a -> b
k :: (κ -> *) -> κ -> *


-- impossible kind signatures
k :: (* -> *) -> *         -- impossible!
k :: (κ -> *) -> *         -- impossible!
k :: (* -> κ) -> κ         -- impossible!
-- are possible type signature
f :: (a -> a) -> a
f :: (τ -> a) -> a
k :: (a -> τ) -> τ
```
