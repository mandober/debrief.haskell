# Function type

All types, including function types, match the signature `a`, but only functions match the signature `a -> b`.

Only one (sensible) function matches the `a -> a` signature.


```hs
-- most general type
f :: a

-- concrete unparameterized types of functions
f :: Int -> Int
f :: Maybe Char -> Maybe Int

f :: a -> a
f :: a -> b

f :: Int -> a
f :: a -> Int

f :: Functor f => f a -> f b
f :: (Foldable t, Functor f) => t f a -> t f b

-- other types with contained fns
x :: [Int -> Int]
x :: (Int -> Char, Int -> Bool)
x :: Maybe (Int -> Int)
x :: Either e (a -> b)
```


The most general type, that only functions match, is `(a -> b)`.

Therein is a function type ctor `(->)`, with fixity `(infixr -1)`
and no representable data ctors.

GHC.Prim defines it as: `data (->) (a :: TYPE q) (b :: TYPE r)`

Like other type ctors it can be partially applied, yielding `((->) r)`.

This partially applied type of functions is exactly the type we'll use to make functions instances of the Functor, Applicative and Monads.

Functions (fn types) are functors/applicatives/monads in their input type, `r`, which is fixed; they are used as ((->) r). Their output type, `a` is flexible.

(r -> a) ~~> ((r ->) a) ~~> ((->) r a)
