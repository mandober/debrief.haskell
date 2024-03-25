# The Less Travelled Monad Tutorial: Understanding Kinds

> This is part 1 of a monad tutorial (but as we will see, it's more than your average monad tutorial). If you already have a strong grasp of t...

This is part 1 of a monad tutorial (but as we will see, it's more than your average monad tutorial). If you already have a strong grasp of types, kinds, monads, and monad transformers, and type signatures like `newtype RST r s m a = RST { runRST :: r -> s -> m (a, s) }` don't make your eyes glaze over, then reading this won't change your life. If you don't, then maybe it will.

More seriously, when I was learning Haskell I got the impression that some topics were "more advanced" and should wait until later. Now, a few years in, I feel that understanding some of these topics earlier would have significantly sped up the learning process for me. If there are other people out there whose brains work somewhat like mine, then maybe they will be able to benefit from this tutorial. I can't say that everything I say here will be new, but I haven't seen these concepts organized in this way before.

This tutorial is not for absolute beginners. It assumes a basic knowledge of Haskell including the basics of data types, type signatures, and type classes. If you've been programming Haskell for a little bit, but are getting stuck on monads or monad transformers, then you might find some help here.

    data Distance = Dist Double
    data Mass = Mass Double

This code defines data types called Distance and Mass. The name on the left side of the equals sign is called a **type constructor** (or sometimes shortened to just **type**). The Haskell compiler automatically creates functions from the names just to the right of the equals sign. These functions are called **data constructors** because they construct the types Distance and Mass. Since they are functions, they are also first-class values, which means they have types as seen in the following ghci session.

    $ ghci ltmt.hs
    GHCi, version 7.0.3: http://www.haskell.org/ghc/  :? for helpghci> :t Dist
    Dist :: Double -> Distance
    ghci> :t Mass
    Mass :: Double -> Mass
    ghci> :t Distance
    <interactive>:1:1: Not in scope: data constructor `Distance'

We see here that Dist and Mass are functions that return the types Distance and Mass respectively. Frequently you'll encounter code where the type and the constructor have the same name (as we have here with Mass). Distance, however, illustrates that these are really two separate entities. Distance is the type and Dist is the constructor. Types don't have types, so the ":t" command fails for Distance.

Now, we need to pause for a moment and think about the meaning of these things. What is the Distance type? Well, when we look at the constructor, we can see that a value of type Distance contains a single Double. The constructor function doesn't actually do anything to the Double value in the process of constructing the Distance value. All it does is create a new context for thinking about a Double, specifically the context of a Double that we intend to represent a distance quantity. (Well, that's not completely true, but for the purposes of this tutorial we'll ignore those details.) Let me repeat that. **A type is just a context.** This probably seems so obvious that you're starting to wonder about me. But I'm saying it because I think that keeping it in mind will help when talking about monads later.

Now let's look at another data declaration.

    data Pair a = MkPair a a

This one is more interesting. The type constructor Pair takes an argument. The argument is some other type a and that is used in some part of the data constructor. When we look to the right side, we see that the data constructor is called MkPair and it constructs a value of type "Pair a" from two values of type "a".

    MkPair :: a -> a -> Pair a

The same thing we said for the above types Distance and Mass applies here. The type constructor `Pair` represents a context. It's a context representing a pair of values. The type `Pair Int` represents a pair of Ints. The type `Pair String` represents a pair of strings. And on and on for whatever concrete type we use in the place of `a`.

Again, this is all very straightforward. But there is a significant distinction between the two type constructors Pair and Distance. Pair requires a type parameter, while Distance does not. This brings us to the topic of kinds. (Most people postpone this topic until later, but it's not hard to understand and I think it helps to clarify things later.) You know those analogy questions they use on standardized tests? Here's a completed one for you:

    values : types    ::   types : kinds

Just as we categorize **values** by **type**, we categorize **type constructors** by **kind**. GHCi lets us look up a type constructor's kind with the ":k" command.

    ghci> :k Distance
    Distance :: *
    ghci> :k Mass
    Mass :: *
    ghci> :k Dist
    <interactive>:1:1: Not in scope: type constructor or class `Dist'
    ghci> :k Pair
    Pair :: * -> *
    ghci> :k Pair Mass
    Pair Mass :: *

In English we would say "Distance has kind \*", and "Pair has kind \* -> \*". Kind signatures look similar to type signatures because they are. When we use Mass as Pair's first type argument, the result has kind \*. The Haskell report defines kind signatures with the following two rules.

*   The symbol \* represents the kind of all nullary type constructors (constructors that don't take any parameters).
*   If k1 and k2 are kinds, then k1->k2 is the kind of types that take one parameter that is a type of kind k1 and return a type of kind k2.

As an exercise, see if you can work out the kind signatures for the following type constructors. You can check your work with GHCi.

    data Tuple a b = Tuple a b
    data HardA a = HardA (a Int)
    data HardB a b c = HardB (a b) (c a Int)

Before reading further, I suggest attempting to figure out the kind signatures for HardA and HardB because they involve a key pattern that will come up later.

* * *

Welcome back. The first example is just a simple extension of what we've already seen. The type constructor Tuple has two arguments, so it's kind signature is `Tuple :: * -> * -> *`. Also if you try :t you'll see that the data constructor's type signature is `Tuple :: a -> b -> Tuple a b`.

In the case of the last two types, it may be a little less obvious. But they build on each other in fairly small, manageable steps. For HardA, in the part to the left of the equals sign we see that there is one type parameter 'a'. From this, we can deduce that HardA's kind signature is something of the form `? -> *`, but we don't know exactly what to put at the question mark. On the right side, all the individual arguments to the data constructor must have kind \*. If `(a Int) :: *`, then the type 'a' must be a type constructor that takes one parameter. That is, it has kind `* -> *`, which is what we must substitute for the question mark. Therefore, we get the final kind signature `HardA :: (* -> *) -> *`.

HardB is a very contrived and much more complex case that exercises all the above simple principles. From `HardB a b c` we see that HardB has three type parameters, so it's kind signature has the form `HardB :: a -> b -> c -> *`. On the right side the `(a b)` tells us that `b :: *` and `a :: * -> *`. The second part `(c a Int)` means that c is a type constructor with two parameters where its first parameter is a, which has the type signature we described above. So this gives us `c :: (* -> *) -> * -> *`. Now, substituting all these in, we get `HardB :: (* -> *) -> * -> ((* -> *) -> * -> *) -> *`.

The point of all this is to show that when you see juxtaposition of type constructors (something of the form `(a b)` in a type signature), it is telling you that the context a is a non-nullary type constructor and b is its first parameter.

Continue on to [Part 2 of the Less Travelled Monad Tutorial](http://softwaresimply.blogspot.com/2012/04/ltmt-part-2-monads.html)


[Source](http://softwaresimply.blogspot.com/2012/04/less-travelled-monad-tutorial-part-1.html)