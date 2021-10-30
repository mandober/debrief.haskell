# Type refinement

> Refinement Types = Types + Predicates

Haskell supports a limited form of type refinement through GADTs:

```hs
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

type Some :: Type -> Type
data Some a where
  SomeInt   :: Int  -> Some Int
  SomeChar  :: Char -> Some Char
  Something :: a    -> Some a

unSome :: Some a -> a
unSome (SomeInt   n) = n + 3
unSome (SomeChar  c) = Data.Char.toLower c
unSome (Something a) = a
```

With ADTs, the return type of all data ctors is always fixed - it is the type being declared. GADTs are generalized ADTs in that they allow their data ctors to completely specify their signatures. The "completely" part mostly means that each data ctor can have a different return type (in a way, up to a subtype). This is realized through the declaration of the GADTs itself, more precisely, through its type parameter.

In the example above, the type `Some a` has 3 data ctors, each with a full explicit signature, from which it can be seen that they all return `Some a`, that is, some variant of that type; they can freely instantiate the type param `a` at different types. The `Anything` data ctor doesn't take advantage of that, but the other two ctors do.

However, the magic happens when we pattern-match a value of the `Some` type: within a branch that successfully matched, GHC understands that the polymorphic type (type var `a`) can be refined to a more specific type and that is exactly what happens in the `SomeInt` and `SomeChar` cases.

Even though the `unSome` function is polymorphic, GHC understands that, in the first equation, if the arg is matched against the pattern `(SomeInt x)` pattern, then in the RHS of this, the pattern-bound variable, `x`, must be an `Int`, i.e. `a ~ Int`, so it allows `x` to be treated as `Int` (and permits addition to it).

> This is called *type refinement*.

The similar thing happens in the second equation, where `x ~ Char`. The most interesting thing is that the return type of `unSome` is declared as `a`, which usually means that `unSome` can return any type, but nevertheless, that type must be the same in all of its equations; however, in this case, the return type can vary because each equation can instantiate the return type `a` at a distinct concrete type.

> Depending on the pattern match, the return type of `unSome` is either `Int` or `Char` or `a`.




## Ref

Refinement type
https://en.wikipedia.org/wiki/Refinement_type

Dependent types vs refinement types
https://cs.stackexchange.com/questions/21728/dependent-types-vs-refinement-types

Integrating Refinement And Dependent Types: A Fellowship Report
https://www.tweag.io/blog/2021-02-05-refinement-types/

Programming with Refinement Types - An Introduction to LiquidHaskell
https://ucsd-progsys.github.io/liquidhaskell-tutorial/
http://ucsd-progsys.github.io/lh-workshop/

Difference between Dependent type, refinement type and Hoare Logic
https://cs.stackexchange.com/questions/54957/difference-between-dependent-type-refinement-type-and-hoare-logic?noredirect=1&lq=1
