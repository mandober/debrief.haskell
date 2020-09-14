# Types

Explicit types are always denoted with the first letter in capital case. 

The most general type in Haskell is `a`. All other types match this signature since `a` can stand for any type at all: `Maybe b`, `(b, c)`, `Either e [b -> b]`, `s -> (b, s)`, etc. More precisely, the type parameter `a` can stand for any *concrete type*; it cannot stand for (partially applied) type ctors such as `Maybe`, `Either` or `Either e`, `Map k`, etc. It only matches the fully *saturated* types.

All types match the signature `a`, but only the primitive types do not have any other, more-specific but not completely concrete, signature. That is, an integer, such as 42, either matches its own concrete type `Int` or a general signature expressed as a type parameter, like `a`. A character matches either its own `Char` or `a`.

However, more complex, that is, compound types, have more specific signatures. These are algebraic types that have a type ctor and at least one data ctor. They are parameterized by one or more (concrete) types. So, for example, a type like `Maybe Int` matches the most general signature `a`, but it also matches the more specific signature `Maybe a`. Its most specific signature is itself, `Maybe Int`.

The types like `Maybe Int` or `Maybe Char` are fully specified types. They are concrete types, lacking parameterization. Parameterization can be introduced when it's desirable to match (accept) all Maybe types, whether the inner type is: Maybe Int, Maybe (), Maybe whatever. So, a function that wants to state that it accepts any "maybe" type, can declare a formal parameter of `Maybe a` type.

```hs
-- most general type
x :: a

x :: Int
x :: Char
x :: ()
x :: Int -> Int

x :: Maybe Int
x :: [Int]
x :: (Int, Char)

x :: a
x :: Maybe a
x :: Functor f => f a
x :: (Foldable t, Functor f) => t f a
```



All types of functions also match the `a` signature, but only functions will match the signature `a -> b`.




for everything, including functions, is `a`.
The most general type for functions is (r -> a).

Therein is a function type ctor (->), with fixity (infixr -1)
and no representable data ctors.

GHC.Prim defines it as: data (->) (a :: TYPE q) (b :: TYPE r)

Like other type ctors it can be partially applied, yielding ((->) r).

This partially applied type of functions is exactly the type we'll use to make functions instances of the Functor, Applicative and Monads.

Functions (fn types) are functors/applicatives/monads in their input type, `r`, which is fixed; they are used as ((->) r). Their output type, `a` is flexible.

(r -> a) ~~> ((r ->) a) ~~> ((->) r a)
