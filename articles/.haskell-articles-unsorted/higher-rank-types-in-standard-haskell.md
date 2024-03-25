# Higher-rank types in Standard Haskell

> A blog about functional programming

Posted on March 25, 2019

The other day on the IRC channel `#haskell` there was question about writing functions parameterized by polymorphic functions, and the natural solution was to use `RankNTypes`. That got me wondering, was that the simplest possible solution? Of course, implicit in that question is another: what does it mean to be simple? I certainly don’t intend to answer this question, but pondering it will take you to fun places. That time, I got to think about this puzzle: how to write higher-rank polymorphic functions, without using `RankNTypes` or any other language extension?

While you think about this, and to not spoil the answer right away, let me write a few words about higher-rank types.

Polymorphism and higher-rank types
----------------------------------

A _polymorphic function_ is one which can be given many types. For example, the `(+)` operator is polymorphic, because it can take the following types:

    (+) :: Integer -> Integer -> Integer
    (+) :: Int -> Int -> Int
    (+) :: Double -> Double -> Double

One of the earliest mentions[1](#fn1) of the word polymorphism appears in lectures by Christopher Strachey in 1967. One can find a paper dating from 2000 with the gist of those lectures floating around:

> (…) ambiguous symbols (such as `+`) which mean different things according to the types of their operands. We call ambiguous operators of this sort _polymorphic_ as they have several forms depending on their arguments.
> 
> – Christopher Strachey, in _Fundamental Concepts in Programming Languages_ ([PDF](https://www.cs.cmu.edu/~crary/819-f09/Strachey67.pdf)).

There are different forms of polymorphism[2](#fn2): every programming language has its own approach. The above paper discusses a distinction between “ad-hoc” vs “parametric” polymorphism, among other concepts.

In Haskell as described by the standard (98 or 2010), you can easily define “simple” polymorphic functions, but, to this day, an extension is required to define functions whose arguments are themselves polymorphic: `RankNTypes`.

For example, the following function takes a permutation function as an argument, and applies it to two lists:

    coolThing :: (???) -> ([Integer], [String])
    coolThing perm = (perm [0, 1], perm ["Higher", "Rank", "Polymorphism"])

What is the type of `perm`? It is being applied to a list of `Integer`, so it should have type `[Integer] -> [Integer]`, and also to a list of `String`, so it should have type `[String] -> [String]`. Thus `perm` must be polymorphic. The `RankNTypes` extension makes this legal:

    {-# LANGUAGE RankNTypes #-}
    
    coolThing :: (forall a. [a] -> [a]) -> ([Integer], [String])
    coolThing perm = (perm [0, 1], perm ["Higher", "Rank", "Polymorphism"])

The word “rank” in the name of the extension refers to the following classification:

*   monomorphic functions (i.e., non-polymorphic) have rank 0;
*   “simple” polymorphic functions have rank 1 (for example, polymorphic functions definable in standard Haskell, and the `perm` parameter);
*   polymorphic functions parameterized by functions of rank 1 or less have rank 2 (for example, `coolThing`);
*   polymorphic functions parameterized by functions of rank 2 or less have rank 3, etc.

It’s not often useful to distinguish ranks 2 and above, so we just say “higher-rank”.

Rather than “higher-rank polymorphism”, some also say “higher-order polymorphism”. The term “higher-order” applies to a lot of things, and seems quite appropriate here as well, so I’m not sure why “higher-rank” was picked over “higher-order” when talking about polymorphism, but one benefit of using a different term might be that “higher-order polymorphism” could be confused with “higher-order” (functions parameterized by functions) and simple “polymorphism” separately.

Higher-rank polymorphism without `RankNTypes`
---------------------------------------------

The first thing to note is that this problem is highly open to interpretation, that’s part of the fun. It’s definitely possible that you would come up with a totally different answer.

### Higher-rank polymorphism with class

Extensions and imports for this Literate Haskell file

    {-# LANGUAGE ExplicitForAll #-}
    {-
      `ExplicitForAll` does not add any actual power to the type system.
      It gives us the luxury of explicit `forall` quantifiers, which is
      especially useful to make class definitions less confusing.
      But I would use `forall` even if `RankNTypes` were the only way
      to enable this.
    -}
    
    module HigherRankWithClass where

Standard Haskell doesn’t have many features, it is basically a lambda calculus with data types and type classes. Of course, that’s only how I see things in this context. Other people may see a heap of crazy things, depending on their familiarity with Haskell and what they consider constitutes “the language”.

Anyway, if you think about it in the right way[3](#fn3), you quickly zero in on type classes as another mechanism for passing polymorphic functions around.

The most common example of this is the `Functor` type class. Its method `fmap` is polymorphic (with parameters `a` and `b`).

    class Functor f where
      fmap :: forall a b. (a -> b) -> (f a -> f b)

A `Functor f` constraint can be seen as a way to carry a polymorphic function `forall a b. (a -> b) -> (f a -> f b)`.

    void :: Functor f => f a -> f ()
    void = fmap (\_ -> ())
    
    -- similar to this rank 2 function
    
    void :: (forall a b. (a -> b) -> (f a -> f b)) -> f a -> f ()
    void f = f (\_ -> ())

Another interesting one is the `Traversable` class, because the `traverse` method itself has a type class constraint:

    class Traversable t where
      traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

So functions parameterized by `Traversable` constraints are really rank 3 functions.

    sequence :: (Traversable t, Applicative f) => t (f a) -> f (t a)
    sequence = traverse id

How do we encode the higher-rank `coolThing` I showed earlier with type classes? The main idea is that type classes are a way to attach values to types, and you can always make up arbitrary types to attach arbitrary things to.[4](#fn4)

We thus create a type class `Permutation` to associate functions of type `forall a. [a] -> [a]` (which might not be permutations in the official, combinatorial sense, but that’s close enough) to types `p`, which may possibly contain some runtime data (or one could also use `Proxy p -> [a] -> [a]` if that’s not an issue):

    class Permutation p where
      perm :: forall a. p -> [a] -> [a]

We can define a few permutations.

    -- Identity
    data Id = Id
    instance Permutation Id where
      perm _ = id
    
    -- Reverse
    data Rev = Rev
    instance Permutation Rev where
      perm _ = reverse
    
    -- Rotation
    -- N.B. Degenerates to the identity permutation on short lists.
    data Rot = Rot Int
    instance Permutation Rot where
      perm (Rot n) xs = zs ++ ys where
        (ys, zs) = splitAt n xs

Funny thing, we can even define an operator to compose permutations.

    data Com p q = p :>>> q
    instance (Permutation p, Permutation q) => Permutation (Com p q) where
      perm (p :>>> q) = perm q . perm p

Here’s the classy `coolThing`, which reifies the representation `p` of a polymorphic permutation function using `perm`.

    coolThing :: Permutation p => p -> ([Integer], [String])
    coolThing p = (perm p [0, 1], perm p ["Higher", "Rank", "Polymorphism"])

Some examples to show off.

    examples :: IO ()
    examples = do
      print (coolThing Id)
      print (coolThing Rev)
      print (coolThing (Rot 2 :>>> Rev))
      print (coolThing (Rev :>>> Rot 1))
    
    -- ([0,1],["Higher","Rank","Polymorphism"])
    -- ([1,0],["Polymorphism","Rank","Higher"])
    -- ([1,0],["Rank","Higher","Polymorphism"])
    -- ([0,1],["Rank","Higher","Polymorphism"])

* * *

1.  Hedging my bets. I don’t actually know whether it’s _the_ earliest because I’m too lazy to research it.[↩︎](#fnref1)
    
2.  Since its etymology is the Ancient Greek words for “many” (πολύς) and “shape” (μορφή), one could say here that polymorphism is… polymorphic. _badum tss_[↩︎](#fnref2)
    
3.  My way is of course the right one.[↩︎](#fnref3)
    
4.  This is a reductionist view of type classes for the sake of this exercise. From the point of view of programming language design, there are richer discussions about type classes to be had with questions like “should classes have laws?”, “how powerful should instance resolution be?”, or “what about orphan instances?”.[↩︎](#fnref4)


[Source](https://blog.poisson.chat/posts/2019-03-25-higher-rank-types.html)