# Recursion Schemes


* Recursion comes with a lot of boilerplate.
* In the early 1990s, the seminal paper "Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire" by Erik Meijer, M.M. Fokkinga, and Ross Paterson, introduced a little-known technique known as recursion schemes.
* *Recursion-schemes* technique makes recursion generic, removing much of the boilerplate while separating business logic from recursive traversal of a DS.
* It inspired many different schemes, each for some kind of recursive traversal
* Catamorphism is a specific recursion schema corresponding to the `foldr`.

There are many recursion and corecursion schemes for diff types of un/folds
- *catamorphism* is generalization of `fold`
- *anamorphism*  is generalization of `unfold`
- *hylomorphism* is `unfold` followed by `fold`
- *metamorphism* is `fold` followed by `unfold`


## Recursion Schemes for Idiots

https://stackoverflow.com/questions/6941904/recursion-schemes-for-dummies

They're presented in a more rigorous form usually, to make the connection to category theory clearer. The denser form lets us distinguish *data* (the necessarily *finite product* of an initial algebra) and *codata* (the possibly *infinite product* of a final coalgebra). This distinction lets us guarantee that a fold is never called on an infinite list.

The other reason for the way that catamorphisms and anamorphisms are generally presented is that by operating over *F-algebras* and *F-coalgebras* (generated from functors), we can write them once and for all, rather than once over a list, once over a binary tree, etc.

A *metamorphism* (Gibbons) is like an inside-out *hylo*, a `fold` followed by an `unfold`, so it can be used to tear down a stream and build up a new one, with a potentially different structure.

Except perhaps for *histomorphisms*, I don't think the rest of the zoo is necessarily something you'd want to think with directly most of the time. If you grok *hylo* and *meta*, you can express nearly anything in terms of them alone. Typically the other morphisms are *more restrictive*, not less (but therefore give you more properties "for free").

E. Kmett posted a nice "field guide" to the various schemes in the literature: http://comonad.com/reader/2009/recursion-schemes/



## What is a recursion scheme?

https://hackage.haskell.org/package/recursion-schemes

Many recursive functions share the same structure, e.g. pattern-match on the input and, depending on the data constructor, either recur on a smaller input or terminate the recursion with the base case. Another one: start with a seed value, use it to produce the first element of an infinite list, and recur on a modified seed in order to produce the rest of the list. Such a structure is called a recursion scheme.

Each recursion scheme has a unique namel, such as
- *fold* or *catamorphism*
- *unfold* or *anamorphism*

The idea is that a recursive function may be broken into two parts:
- the part which is the same in all the recursive functions which follow a given recursion scheme, and
- the part which is different in each function.

Our implementation performs the recursive, common part, and takes as input a function which performs the non-recursive, unique part.

Many recursion schemes can be implemented in terms of each other. So if you notice that the non-recursive functions you provide themselves seem to share a common pattern, you might be accidentally reimplementing an existing recursion scheme which already has those common parts builtin; or maybe you have stumbled upon a new recursion scheme which does not yet have a name, and which you may want to implement yourself.

One way to implement such a custom recursion scheme is to combine the features of existing recursion schemes. For example, a *paramorphism* gives the non-recursive function access to the original sub-trees, a *zygomorphism* gives that function access to auxiliary results computed from those sub-trees, and so the combined *zygomorphic paramorphism* gives that function access to both the original sub-trees and the auxiliary results.

In order to construct such combinations, most of our recursion schemes come in a generalized variant, e.g. `gzygo`, and in a *distributive law transformer* variant, e.g. `distZygoT`.

Just like monad transformers, distributive law transformers can be combined into stacks, and like monad transformers, the order in which you combine the layers determines how the layers interact with each other.

Apply a generalized recursion scheme to a stack of distributive laws in order to obtain a recursion scheme which has both the features of the generalized recursion scheme and those of the distributive laws.


Example:

```hs
-- | Base functor of []
data ListF a b = Nil | Cons a b

instance Functor (ListF a) where
  fmap _ Nil        = Nil
  fmap f (Cons a b) = Cons a (f b)
```




## Practical Recursion Schemes

https://jtobin.io/practical-recursion-schemes

Recursion schemes are elegant and useful patterns for expressing general computation. In particular, they allow you to "factor out recursion" of whatever semantics you may be trying to express when interpreting programs, keeping your interpreters concise, your concerns separated, and your code more maintainable.

In particular, to feel comfortable using recursion-schemes, there are a few key patterns worth understanding:
* Factoring recursion out of your data types using *pattern functors* and a *fixed-point wrapper*.
* Using `Foldable` and `Unfoldable` classes, plus navigating the `Base` type family.
* How to use some of the more common recursion schemes for everyday tasks.



## FP with Bananas, Lenses, Envelopes and Barbed Wire
(paper by Erik Meijer, Maarten Fokkinga, Ross Paterson)

- Bananas     : ⦇ ⦈             (| a |)
- Lenses      : ⟬ ⟭  ⦅ ⦆         [( a )] 【　】
- Envelopes   : ⦃ ⦄ ⦗ ⦘         
- Barbed Wire : ⦓ ⦔ ⦕ ⦖

The gist of the paper is that all explicit recursion can be factored out into a few core combinators. As such, the reasoning is that we should instead learn these combinators or recursion schemes, rather than doing our own ad-hoc recursion whenever we need it.

> Catamorphisms over Lists

Catamorphisms refer to a fold over a data structure.

Specialization of a catamorphism over lists:

Let `base :: b` and `step :: a -> b -> b`, 
then a *list catamorphism* `fold :: [a] -> b` 
is a function, fold, of the following form:

```hs
-- base is a base case, a default value (sometimes an accumulator)
def :: b
-- step is a rec case, takes an el and acc so it's right-biased (foldr)
step :: a -> b -> b

fold :: [a] -> b
fold    [ ] = def
fold (x:xs) = step x (fold xs)
```


This function should look familiar when specialized to `foldr` on lists:

```hs
foldr :: (Foldable f) => (a -> b -> b) -> b -> f a -> b
foldr @[] ::             (a -> b -> b) -> b -> [a] -> b

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f z []     = z
foldl f z (x:xs) = foldl f (f z x) xs
```

We can view foldr as taking the values `step :: a -> b -> b` and `def :: b`, and then giving back a function that takes an [a] and computes some `b`. For example, we can write a few of the common prelude functions over lists as catamorphisms of this form.

```hs
length :: [a] -> Int
length = foldr (\x z -> z + 1) 0

filter :: forall a. (a -> Bool) -> [a] -> [a]
filter p = foldr step []
  where
    step :: a -> [a] -> [a]
    step a as = if p a
                   then a : as
                   else as
```

When written this way - Meijer et al. are quick to point out - the so-called "fusion" law is easily seen:

```hs
f . foldr step default = foldr step' default'
  where
    step' a b = step a (f b)
    default'  = f default
```

which intuitively says that you can "fuse" a catamorphism with a subsequent composition into a single catamorphism.









## The data type of lists

We shall illustrate the recursion patterns of interest by means of the specific data type of cons-lists. Modern FPLs allow the definition of cons-lists over some type `A` as:

`A* ::= Nil | Cons (A:A*)`

**Catamorphisms** are functions of the type `A* -> B`
- the recursive structure of this definition is employed when writing functions of the type `A* -> B` that destruct a list
- "cata" comes from the greek preposition `κατα` (downwards), e.g."catastrophe"

**Anamorphisms** are functions of the type `B -> A*`
- they generate a list of type `A*` from a seed type `B`.
- "ana" comes from the greek preposition `ανα` (upwards), e.g. "anabolism"

**Hylomorphisms** are functions of the type `A -> B`
- their call-tree has the shape of a cons-list
- "hylo" comes from the Aristotelian philosophy that form and matter are one, `υλoϛ` meaning dust or matter


## Catamorphisms

Let `base :: b` and `(:) :: a -> b -> b`, 
then a list catamorphism `h :: [a] -> b` 
is a function of the following form:

```hs
base :: b
step :: a -> b -> b

b ~ [a]

([]) :: b
(:)  :: a -> b -> b

h :: [a] -> b
h []     = base
h (x:xs) = step x (h xs)
```

In the notation of Bird & Wadler one would write `h = foldr b (:)`. We write catamorphisms by wrapping the relevant constituents between so called *banana brackets*, h = ⦇b,:⦈

(2)     h = ⦅ b, : ⦆








## Refs

Review: Bananas, Lenses, Envelopes and Barbed Wire
https://reasonablypolymorphic.com/blog/recursion-schemes/index.html

Origami programming - Jeremy Gibbons
http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/origami.pdf

Jeremy Gibbons - publications
http://www.cs.ox.ac.uk/people/publications/date/Jeremy.Gibbons.html
