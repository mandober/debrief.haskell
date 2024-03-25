# Varience



## Varience of fucntors

First, a note about our friend, the `Functor` class. You can think of `Functor f` as an assertion that `a` never appears in the "negative position". This is an esoteric term for this idea: Notice that in the following datatypes the `a` appears to act as a "result" variable.

```hs
newtype IO a = IO (World -> (World, a))
newtype Identity a = Identity a
newtype List a = List (forall r. r -> (a -> List a -> r) -> r)
```

In each of these examples, `a` appears in a positive position. In some sense the `a` for each type represents the "result" of a function. It might help to think of `a` in the second example as `() -> a`. And it might help to remember that third example is equivalent to `data List a = Nil | Cons a (List a)`. In callbacks like `a -> List a -> r`, the `a` appears in the negative position, but the callback itself is in the negative position so negative and negative multiply to be positive.

This scheme for signing the parameters of a function is elaborated in this wonderful blog post:
https://www.schoolofhaskell.com/user/commercial/content/covariance-contravariance

Now note that each of these types admit a Functor. That is no mistake! Functors are meant to model the idea of categorical covariant functors, which "preserve the order of the arrows" i.e. `f a -> f b` as opposed to `f b -> f a`. In Haskell, types where `a` never appears in a negative position always admit Functor. We say these types are *covariant* on `a`.

To put it another way, one could validly rename the `Functor` class to be `Covariant`. They are one and the same idea.

The reason this idea is worded so strangely with the word "never" is that `a` can appear in both a positive and negative location, in which case we say the type is *invariant* on `a`. `a` can also never appear (such as a phantom type), in which case we say the type is *bivariant* (both covariant and contravariant) on `a`.

So for types where `a` never appears in the positive position, we say the type is **contravariant** in `a`. Every such type `Foo a` will admit an instance `Contravariant Foo`. Here are some examples, taken from the `contravariant` package:

```hs
data Void a                   -- (a is phantom)
data Unit a = Unit            -- (a is phantom)
newtype Const c a = Const c   -- (a is phantom)

newtype WriteOnlyStateVariable a = WriteOnlyStateVariable (a -> IO ())
newtype Predicate a = Predicate (a -> Bool)
newtype Equivalence a = Equivalence (a -> a -> Bool)
```

In these examples `a` is either *bivariant* or merely *contravariant*. `a` either never appears or is negative (in these contrived examples `a` always appears before an arrow so determining this is extra-simple). As a result, each of these types admit an instance `Contravariant`.

A more intuitive exercise would be to squint at these types (which exhibit contravariance) and then squint at the types above (which exhibit covariance) and see if you can intuit a difference in the semantic meaning of `a`. Maybe that is helpful, or maybe it is just still abstruse sleight of hand.

When might these be practically useful?

Let us for example want to partition a list of cookies by what kind of chips they have. We have a `chipEquality :: Chip -> Chip -> Bool`. To obtain a `Cookie -> Cookie -> Bool`, we simply evaluate `runEquivalence . contramap cookie2chip . Equivalence $ chipEquality`.

Pretty verbose! But solving the problem of newtype-induced verbosity will have to be another question...

---

The example I always have in head when speaking about those is functions - and then an example of `f` would be `type F a = forall r. r -> a` (which means the first argument is arbitrary but fixed `r`), or in other words all functions with a common input. As always the instance for (covariant) Functor is just `fmap ψ φ = ψ . φ`.

Where the (contravariant) Functor is all functions with a common result - `type G a = forall r. a -> r`, here the Contravariant instance would be `cmap ψ φ = φ . ψ`.

```hs
type F a = forall r. r -> a
type G a = forall r. a -> r
```



## Ref

* What is a contravariant functor?
https://stackoverflow.com/questions/38034077/what-is-a-contravariant-functor

* 24 Days of Hackage: contravariant
https://ocharles.org.uk/blog/guest-posts/2013-12-21-24-days-of-hackage-contravariant.html

* Covariance, contravariance, and positive and negative positions
https://www.schoolofhaskell.com/user/commercial/content/covariance-contravariance

* I love profunctors. They're so easy - Liyang Hu, 2013
https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/profunctors

* Talk: Fun with Profunctors
https://www.youtube.com/watch?v=OJtGECfksds

https://stackoverflow.com/questions/53854853/why-is-there-a-distinction-between-co-and-contravariant-functors-in-haskell-but

https://typeclasses.com/profunctors

https://medium.com/@drboolean/monoidal-contravariant-functors-are-actually-useful-1032211045c4


http://hackage.haskell.org/package/contravariant-1.4.1/docs/Data-Functor-Contravariant.html
