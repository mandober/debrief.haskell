# Making a Show with reflection

> A blog about functional programming

Posted on October 21, 2017

`Show` is one of the most used type classes in Haskell[1](#fn1). Being able to derive its instances is certainly convenient, but it becomes a mess once we start playing with higher-kinded types, such as `Fix :: (Type -> Type) -> Type`.

Derived instance
----------------

    newtype Fix f = Fix (f (Fix f))
      deriving Show

The compiler complains:

    <interactive>:4:39: error:
        • No instance for (Show (f (Fix f)))
            arising from the first field of ‘Fix’ (type ‘f (Fix f)’)
          Possible fix:
            use a standalone 'deriving instance' declaration,
              so you can specify the instance context yourself
        • When deriving the instance for (Show (Fix f))

We can write (with sufficiently many extensions) the instance in the way suggested by GHC:

    newtype Fix f = Fix (f (Fix f))
    
    deriving instance Show (f (Fix f)) => Show (Fix f)

What is this constraint, `Show (f (Fix f))`? To show a value `Fix x :: Fix f`, the derived instance needs to show `x :: f (Fix f)`, hence the constraint. But that still looks bad. Essentially, that `Show` instance for `Fix` is leaking an implementation detail, the type to which `f` is being applied.

A more modular solution would be a way to show `f a` parameterized by a way to show `a`. Something like `forall a. Show a => Show (f a)`. I’m looking forward to seeing Quantified class constraints to write that. But what can we do in the meantime?

Handwritten instance
--------------------

The currently standard solution uses a separate type class for unary type constructors, `Show1`. The constraint `Show1 f` tells us that if we can show `a`, then we can show `f a`.

    showsPrec1 :: (Show1 f, Show a) => Int -> f a -> ShowS

Unfortunately, GHC’s deriving mechanism doesn’t know how to use `Show1` constraints, so we need to write the instance by hand to use `showsPrec1` instead of `showsPrec` in the right places. That’s exactly what the \*recursion-schemes\* library does for `Fix`.

This can be automated to some extent, but not entirely. `Show1` expresses the constraint `forall a. Show a => Show (f a)`, but sometimes the type parameter of `f` is expected to be phantom, so we would just require `forall a. Show (f a)`. It can be obtained as yet another type class, call it `Show1'`, with the overloaded helper:

    showsPrec1' :: Show1' f => Int -> f a -> ShowS

We don’t want to write `Show` instances by hand, but an automated deriving mechanism has no way to know whether we want to use `Show1 f` or `Show1' f` when it encounters a field of type `f a`. Thus, we need the ability to specify how each individual constructor field should be shown, whether it is using `Show`, `Show1`, `Show1'`, or yet another method.

Looking back at GHC’s derived instance, it calls `showsPrec` for every field; that isn’t quite what we want. But `showsPrec` is overloaded:

    showsPrec :: Show a => Int -> a -> ShowS

Basically, `Show a` can also be seen as a type of dictionaries, and `showsPrec` is a function of it.

    showsPrec :: Show a -> Int -> a -> ShowS

Instead of taking the dictionary by explicit function application, `showsPrec` requires it implicitly by emitting a type class constraint. Constraints are solved during type checking, and then overloaded methods are desugared to regular functions.

It turns out that it is possible for us to catch those constraints and pass our own dictionaries. The magic can be found in the \*reflection-extras\* library, though its age makes it a bit clunky to use nowadays.

Semiautomatic deriving
----------------------

For demonstration purposes, I whipped up a post-GHC-8-friendly prototype, with the following combinator (which can be generalized to other classes):

    using :: forall a t. RShow a -> (Show a => t) -> t

The first parameter is a record `RShow a` containing a method to be put in a local `Show` instance, used by the second parameter.

Via TH or Generics, we can obtain a generic implementation of `showsPrec`, which generates a `Show (f (Fix f))` constraint, like the GHC-derived instance does. Some TH scripts can be found in the \*deriving-compat\* library. I know of no equivalent library using Generics.[2](#fn2)

    showsPrecFix :: Show (f (Fix f)) => Int -> Fix f -> ShowS
    showsPrecFix = $(makeShow 'Fix)  -- from deriving-compat

Then we can solve that constraint using `Show1 f` and `Show (Fix f)`.

    instance Show1 f => Show (Fix f) where
      showsPrec =
        using
          RShow { rshowsPrec = showsPrec1 @f @(Fix f) }
          showsPrecFix

One inconvenient of this approach is that the \*reflection\* tricks behind the scenes rely on implementation details of type classes in GHC. It is unclear to me how stable these details can be assumed to be, although they seem to have been around for a good while.

Another issue is that _reflection_ breaks the core principle of _canonicity_ around which type classes are designed. Would a separate system of local instances make sense?

* * *

1.  Citation needed.[↩︎](#fnref1)
    
2.  In case you’re wondering whether \*generic-deriving\* does the job, it doesn’t derive `Show`, but a custom and entirely separate `GShow` class.[↩︎](#fnref2)


[Source](https://blog.poisson.chat/posts/2017-10-21-making-a-show.html)