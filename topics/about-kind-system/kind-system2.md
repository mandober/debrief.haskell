# Kind system


- saturated type ctors include fully applied type ctors and nullary type ctors

- In Haskell 98, the kind system was essentially a STLC "one level up": it has the base kind, `*`, and kind functions, `* -> *`. Later, it got complicated. 
- `κ := * | κ -> κ` where `κ` is a kind variable

Kind polymorphism uses
- kind variable, `κ`
- kind functions, `κ -> κ`
- quantifiers, `forall κ. κ -> κ`
that eventually need to reach the base kind, `κ -> Type`


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
