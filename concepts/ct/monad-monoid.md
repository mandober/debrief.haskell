# Haskell and CT

https://www.reddit.com/r/math/comments/ap25mr/a_monad_is_a_monoid_in_the_category_of/

https://wiki.haskell.org/Hask


>A monad is a monoid in the category of endofunctors.

## What is a category

A [category](https://en.wikipedia.org/wiki/Category_(mathematics)) is simply a collection of 'objects' (or 'points') with 'morphisms' (or 'arrows') between them, satisfying two very simple rules:

You can compose an arrow f: A -> B with an arrow g: B -> C to get a new arrow g . f: A -> C, and this composition is associative (i.e. h . (g . f) = (h . g) . f).

For every object A there exists an identity arrow id_A: A -> A, such that for every arrow f: A -> B we have id_B . f = f . id_A = f.

The classical example in mathematics is the category Set, whose objects are sets, and whose arrows are functions between these sets. In the world of Haskell, we have the category Hask, whose objects are Haskell types and whose arrows are functions between these types. So, for example, Float and Int are objects, and round:: Float -> Int is an arrow.

## What is a functor

In category theory a functor is a map between two categories. So if C and D are categories, then a functor F: C -> D will map objects in C to objects in D, and arrows in C to arrows in D. It does this in a 'nice' way. That is:

The starts and ends of arrows are mapped nicely: if f is an arrow in C from object A to B, then F(f) is an arrow in D from F(A) to F(B).

Identities are preserved: F(id_A) = id_F(A).

Composition is preserved: F(g . f) = F(g) . F(f).

Note that this concept is a bit more general than the concept of functors in Haskell (see below).

## What is an endofunctor

An endofunctor is simply a functor from a category to itself. So in the above, assume that C = D. Note, that doesn't mean that the endofunctor F doesn't do anything. Just like a function from the real numbers to the real numbers might still change the numbers, the functor F might still change the objects and arrows that are fed through it in some way.

## What is a Haskell Functor

What is known as a Functor in Haskell is actually an endofunctor on the category Hask. Recall that Hask has types as its objects and functions between these types as its arrows. So an endofunctor on Hask will map a type to some other type, and will also map functions to functions in some nice way.

This is the definition of Functor in Haskell:

```hs
{- | The 'Functor' class is used for types that can be mapped over.
Instances of 'Functor' should satisfy the following laws:

> fmap id  ==  id
> fmap (f . g)  ==  fmap f . fmap g
-}
class Functor f where
  fmap:: (a -> b) -> f a -> f b
```

See the symmetry with the mathematical definition of functors above? The type constructor f fulfills the role of the functor's action on types (objects), while fmap fulfills the role of the functor's action on functions (arrows).

The two classical examples of Functors in Haskell are lists ([]) and Maybe.

## List is a Functor

List is a type constructor that, given some type a will give you a new type [a], namely lists consisting of values of type a. This is the type (object) mapping part. The function mapping part is fmap (also known simply as map). Given some function f:: a -> b it will give you a function fmap f:: [a] -> [b], namely the function that applies f to every element of a list of values of type a. You can see that fmap id is indeed the identity: doing nothing to every element of a list is the same as doing nothing to the list. The other law, fmap (f . g) = fmap f . fmap g, is also easy to see: doing g and then f to every element of a list, is the same as first doing g to every element of a list and then doing f to every element of the resulting list.

## Maybe is a Functor

Maybe is a type constructor that given some type a will give you a new type Maybe a. The values of this type are Just x for any x of type a, and Nothing. The function mapping part fmap will take some function f:: a -> b and give you a new function fmap f:: Maybe a -> Maybe b. It will take Just x to Just (f x), and Nothing to Nothing. Can you prove the laws?

## What is the category of endofunctors

Ok, now things are going to get a bit more tricky. First we need to talk about natural transformations, which are basically arrows between functors. Then we'll use this to build a category of endofunctors, and finally we'll look at examples in Haskell.

## What are natural transformations

It turns out that if you have categories C and D and functors F, G: C -> D between them, you can sometimes find so called 'natural transformations' between the functors F and G. A natural transformation t: F => G is a family of arrows in D that satisfies two requirements:

The arrows t go from the results of F to the results of G. More precisly, for every object A in C we have an arrow t_A: F(A) -> G(A) in D. This is the 'component' of t at A.

For every arrow f in C, applying F(f) first and then t is the same as applying t first and then G(f). This is the 'natural' part of a 'natural transformation'. More precisely, for every f: X -> Y in C we have t_Y . F(f) = G(f) . t_X. Graphically, it means that going around this diagram in either direction does the same thing ('the diagram commutes').

Of course there's a special case where F and G are endofunctors (so C = D). There's also nothing stopping us from setting F = G, so then we're looking for natural transformations from a functor to itself. Just as with endofunctors these natural transformations may still do something.

## The category of endofunctors

Now, for some category C we have a bunch of endofunctors on C and we have natural transformations between these endofunctors. We can make this into a category! So let's introduce `Endo(C)`, the category whose objects are endofunctors on C, and whose arrows are natural transformations between these endofunctors. You can check that composition of natural transformations is indeed associative, and that there is an identity natural transformation from every endofunctor to itself. But that's not super relevant here.

## The category of endofunctors in Haskell

What does the category `Endo(Hask)` look like?

The objects of the `Endo(Hask)` category are endofunctors on `Hask`, which are Haskell functors. The arrows are transformations from one Haskell functor to another.

If `f` and `g` are functors, then we are looking for some set of functions `t`, such that for every type `a` we have a function `t :: f a -> g a`.

Let's pick `f = Maybe` and `g = List`. 
So we are looking for a set of functions `t :: Maybe a -> List a`.

Here's one example:

```hs
maybeToList :: Maybe a -> List a
maybeToList (Just x) = [x]
maybeToList Nothing  = []
```

`maybeToList` is a NT from the endofunctor `Maybe` to the endofunctor `List`. We also need to chack whether it is natural. We take an arbitrary function `f : a -> b`. If `maybeToList` is natural, it must satisfy the *naturality square*:

>`maybeToList . fmap f` = `map f . maybeToList`

(to distinguish between them, List uses map, Maybe uses fmap).

We can confirm it does using equational reasoning:

```hs
(maybeToList . fmap f) Just x   =  maybeToList (Just (f x))  =  [f x]
(map f .  maybeToList) Just x   =  map f [x]                 =  [f x]

(maybeToList . fmap f) Nothing  =  maybeToList Nothing       =  []
(map f .  maybeToList) Nothing  =  map f []                  =  []
```

The naturality condition does indeed hold. So `maybeToList` is a natural transformation from the endofunctor `Maybe` to the endofunctor `List`.

The inverse NT, from `List` to `Maybe`, is also possible, but like a constant functor, it discards a lot of information (it only keeps the head element):

```hs
listToMaybe :: List a -> Maybe a
listToMaybe  []    = Nothing
listToMaybe (x:xs) = Just x
```

Another interesting NT is `concat :: [[a]] -> [a]`. It is a NT `List² => List`. The naturality condition is `concat . map (map f) = map f . concat`.

```hs
concat :: List (List a) -> List a
concat  []      = []
concat (xs:xss) = xs ++ concat xss
```

Its inverse, `List => List²`, is also possible in two different ways:
- [a,b,c] ⟼ [[a,b,c]]
- [a,b,c] ⟼ [[a],[b],[c]]
- in both cases, `[]` maps to `[[]]`

This suggest that a NT `List => List³` is possible as well:
- [a,b,c] ⟼ [[a],[b],[c]] ⟼ [[[a],[b],[c]]]
- []      ⟼ [[]]          ⟼ [[[]]]



## Monoids

We look at the classical concept of a monoid in set theory. We also consider an example of a monoid in Haskell. Then we try to generalize the set-theoretical concept of a monoid to category theory. However, we are still missing a crucial categorical ingredient, *monoidal category*. After going over it, we are finally ready to consider the concept of a monoid in a category.

## Monoid in set theory

In classical set theory, a monoid is a set `M` equipped with an associative binary operation `• : M × M -> M` satisfying the following axioms:
- Associativity: ∀abc ∈ M. (a • b) • c = a • (b • c)
- Unit: ∃!e ∈ M. ∀a ∈ M. e • a = a • e = a

An example of a monoid is the set of all finite strings over some alphabet with string concatenation as the monoid operation.

This gives us an example in Haskell: for every type `a`, the type `List a` is a monoid with `++` (concatenation) as the monoid operation, and the empty list is the identity element.

Note: the type [a] also includes infinite lists, for which concatenation is not well defined. Because of this, lists in Haskell are not technically monoids, nor are they monads. We ignore this problem in what follows.

## Monoids in category theory

Category theorists are alergic to sets. If they see a definition that includes the word 'set', they start thinking how to rewrite it without mentioning sets. In other words, they try to generalize the definition such that it also holds in categories other than `Set`.

To express *monoids* categorically:
- set `M` is represented by an arbitrary object `M` in some category C
- binary operation `•` is represented with the arrow `μ : M × M -> M`
- identity element is represented by the morphism `η : 1 -> M`

The arrows `μ` and `η` should clearly end at `M`, but where should they start? We need some way to construct a product object, similar to the Cartesian product for sets. There are several ways to do it, but the one most useful for monoids is the concept of a *tensor product* in a *monoidal category*.

## Monoidal categories

The goal is to define an operation `⊗`, called tensor product, that allows us to combine two objects `A` and `B`, in some category C, into a new object `A ⊗ B`, in that same category (hmmm?).

The natural way to map objects across categories is with functors. But in this case, we would have to map two objects into one object (in the same category?). We need a functor that can take two arguments.

The typical way of solving this is to introduce a product space that encodes both arguments as one. We need an object `(A, B)` in *product category* `C×C`.

At this point you may feel tired: before we can define monoids we need to define products of objects, but to do that we need to define monoidal categories, but to do that we need to define products of categories. Unfortunatelly, that's what's up.

For categories C and D the *product category* `C×D` is the category whose
- objects are pairs `(A, B)` of objects `A ∈ C` and `B ∈ D`
- arrows are pairs `f,g: (A,B) → (X,Y)` of arrows `f:A→X ∈ C` and `g:B→Y ∈ D`
- identity arrows as well: `∀(A,B). (idᴬ, idᴮ)`
- composition `(f,g) ∘ (k, l) = (f ∘ k, g ∘ l)` is associative


Now that we have a product category, the machinery of functors is available again, and we can start thinking about a functor `⊗ : C×C → C` that combines objects and arrows.


>What properties would we want this functor to posses?

* We certainly want it to be associative in some way, such that 

`A ⊗ (B ⊗ C)` ≅ `(A ⊗ B) ⊗ C`

This can be made precise by saying there is a NT whose components are isomorphisms:

`α : A ⊗ (B ⊗ C) -> (A ⊗ B) ⊗ C`

A NT whose components are isomorphisms is called a *natural isomorphism*.

* Similarly, we want there to be some identity element for `⊗`, call it `I`, such that `I ⊗ A` and `A ⊗ I` to be 'similar' to `A`, 

`I ⊗ A ≅ A ≅ A ⊗ I`

This can be made precise by saying there are natural isomorphisms

`λ : I ⊗ A -> A` and `ρ : A ⊗ I -> A`

There are also *coherence conditions* these NTs need to satisfy.

- `α : M ⊗ (M ⊗ M) -> (M ⊗ M) ⊗ M`
- `λ : I ⊗ M -> M`
- `ρ : M ⊗ I -> M`




Compaing this to monoids in set theory, there are some similarities:
- instead of elements of a set, we have objects in a category
- instead of a binary operation `•` we have the functor `⊗`
- instead of an identity element we have a unit object
- instead of equations involving elements of a set, we have NTs between functors built using `⊗`.

This correspondence is why the term 'monoid' is reused, and why this concept is called a monoidal category.

If we want to make the monoidal structure of a category C explicit, we will refer to it as the *monoidal category* `(C, ⊗, I)`.

~~The tensor product, ⊗, could be thought of as being either the product or coproduct, i.e. it unifies the two.~~


## Monoids in a monoidal category

Above we defined the concept analogous to monoids. However, it's not quite what we're looking for; a monoidal category is a category with some additional properties. What we want is an object in a category satisfing certain properties; namely, a monoid in a monoidal category.

https://en.wikipedia.org/wiki/Monoid_(category_theory)


We start with the definition of a monoid, and then swap out set-theoretic concepts for category-theoretic ones.

A monoid in a monoidal category `(C, ⊗, I)` is an object `M` with 
- arrow `μ : M ⊗ M -> M`, called multiplication, and
- arrow `η : I -> M`, called unit

satisfying the two monoid axioms:

- Associativity: if we have an object `M ⊗ (M ⊗ M) ≅ (M ⊗ M) ⊗ M`, it doesn't matter if we first multiply the left side or first multiply the right side. This is made exact with a commutative diagram involving `μ` and the natural isomorphism `α`, called the *associator*.

- Unit - identity of multiplication: if we have an object `I ⊗ M ≅ M`, then 'forgetting' the `I` using the *left unitor* `λ` is the same as first mapping to `M ⊗ M` using `η` and then to `M` using `μ`. The symmetric equation must also hold for `M ⊗ I` using the *right unitor* `ρ`.


>So how about a monoid in the category of endofunctors?

Ok, now that we know what a monoid in a (monoidal) category is, we can start thinking about a monoid in the category of endofunctors.

As we found out above, we first need to find a monoidal structure in the category of endofunctors. Turns out that's pretty easy: composition of endofunctors satisfies all the properties of a tensor product! The tensor product identity `I` is simply the identity functor `I`. It takes a little bit of work to show that the natural transformations behave the right way, but we skip over that here.

A monoid in the category of endofunctors is some endofunctor `M`, a natural transformation `μ : M ∘ M -> M` (multiplication), and a natural transformation `η : id -> M` (unit), that satisfy the monoid axioms.

How can we make sense of this definition? Well, here's one way that will carry over quite nicely to Haskell: we can think of our endofunctors as introducing some additional structure on the objects they're applied to. 

The action of the endofunctors on arrows is to 'lift' the arrow into the structure. The endofunctor `M` then, is some chosen structure.

The transformation `μ` (multiplication) can be seen as 'flattening' the duplicate structure `M ∘ M` down to `M`. 

The transformation `η` can be thought of as injecting unstructured data into the structure `M`.

>What about the monoid axioms?

* Associativity: 
If you have a triplicate structure `M ∘ M ∘ M = M³`, it doesn't matter if you flatten it down to `M` as `(M ∘ M) ∘ M -> M ∘ M -> M` or as `M ∘ (M ∘ M) -> M ∘ M -> M`. This can be symbolically stated as for every `X, μ_X ∘ M(μ_X) = μ_X ∘ μ_M(X)`.

Intuitively, there's a unique way to flatten layered structure.

* Unit is identity of multiplication: 
If you have strucured data, `M`, and then use `η` to inject this into `M` again, you obtain data in `M ∘ M`. You can then use `μ` to flatten this back down to `M`. This is the same as doing nothing. Symbolically, for every `X, μ_X ∘ η_M(X) = id_M(X)`.

If you have structured data, `M`, and then use `M(η)` to give structure to the 'contents' of your structure, you again obtain data in `M ∘ M`. You can again use `μ` to flatten this back down to `M`. This is, again, the same as doing nothing. Symbolically, for every `X, μ_X ∘ M(η_X) = id_M(X)`.

Intuitively, injecting structure and flattening it cancel each other out.

## A monoid in `Endo(Hask)`

Let's move to the world of Haskell again. As before, we look at `Endo(Hask)`, with its familiar endofunctors of `List` and `Maybe` and others.

Let's choose `M = List`. What should we pick for `μ`? It should be a natural transformation `μ :: [[a]] -> [a]`. We've seen one before: `concat` (also known as `join`). How about `η`? It should be a natural transformation `η :: id(a) -> [a]`, which is simply `η :: a -> [a]`. Well it's pretty simple to come up with one:

```hs
inject :: a -> [a]
inject x = [x]
```

This function is also known as `return` in Haskell.

>What about the monoid axioms?

For associativity we have to check that if we have some 3d list, that flattening the outer dimensions first will give the same result as flattening the inner dimensions.

So, `concat . concat = concat . (map concat)`. It shouldn't be too hard to convince yourself that this is true.

But does our choice of unit play nice with the multiplication? We need to check:

Injecting a list into a list and then concatenating is the same as doing nothing to that list: `concat . inject = id :: [a] -> [a]`. That sounds pretty reasonable.

Injecting every element of a list into a list and then concatenating is the same as doing nothing to that list: `concat . map inject = id :: [a] -> [a]`. Again, that seems clear.

So `List` is a monoid in the `Endo(Hask)`. 
Or, equivalently, `List` is a monad in `Hask`. 
Yey!
