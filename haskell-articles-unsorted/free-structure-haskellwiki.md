# Free structure - HaskellWiki

> This article attempts to give a relatively informal understanding of "free" structures from algebra/category theory, with pointers to some of the formal material for those who desire it. The later sections make use of some notions from category theory, so some familiarity with its basics will be useful.

### Introduction

This article attempts to give a relatively informal understanding of "free" structures from algebra/category theory, with pointers to some of the formal material for those who desire it. The later sections make use of some notions from [category theory](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/Category_theory "Category theory"), so some familiarity with its basics will be useful.

### Algebra

#### What sort of structures are we talking about?

The distinction between free structures and other, non-free structures, originates in [abstract algebra](http://en.wikipedia.org/wiki/Abstract_algebra), so that provides a good place to start. Some common structures considered in algebra are:

*   **[Monoids](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/Monoid "Monoid")**
    *   consisting of
    *   And satisfying the equations

*   **[Groups](http://en.wikipedia.org/wiki/Group_(mathematics))**
    *   consisting of
    *   satisfying
        *   ![ x * x^{-1} = e = x^{-1} * x](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/5/f/9/5f96b8c35107abaf1c550527a9fd91cc.png)

*   **[Rings](http://en.wikipedia.org/wiki/Ring_(mathematics))**
    *   consisting of
    *   such that

So, for algebraic structures, we have sets equipped with operations that are expected to satisfy equational laws.

#### Free algebraic structures

Now, given such a description, we can talk about the free structure over a particular set ![S](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/5/d/b/5dbc98dcc983a70728bd082d1a47546e.png) (or, possibly over some other underlying structure; but we'll stick with sets now). What this means is that given ![S](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/5/d/b/5dbc98dcc983a70728bd082d1a47546e.png), we want to find some set ![M](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/6/9/6/69691c7bdcc3ce6d5d8a1361f22d04ac.png), together with appropriate operations to make ![M](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/6/9/6/69691c7bdcc3ce6d5d8a1361f22d04ac.png) the structure in question, along with the following two criteria:

*   There is an embedding ![i : S \to M](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/5/c/b/5cb7bc1f91f8b73bc7ce2ed10f87cd62.png)
*   The structure generated is as 'simple' as possible.

So, in the case of a free monoid (from here on out, we'll assume that the structure in question is a monoid, since it's simplest), the equation ![x * y = y * x](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/e/7/e/e7e6600645d5aa4c9abd993c6b9f3056.png) should not hold unless ![x = y](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/9/1/9/919860b52317a584e5de6f3257631d16.png), ![x = e](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/3/c/5/3c5a29c6dae7df5f94c227a51bbe479f.png) or ![y = e](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/4/f/5/4f5f1a28b50bbfe74127485b7c7b7313.png). Further ![i x \in M](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/8/3/f/83f02529321a12c6301faf73d0bd3cc1.png), for all ![x](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/9/d/d/9dd4e461268c8034f5c8564e155c67a6.png), and ![e \in M](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/c/e/0/ce0a4af9724ad318f7bac1843ba16df5.png), and ![\forall x, y \in M.\,\, x * y \in M](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/a/d/2/ad2ad802139c5686c27fc3b0037a3ece.png) (and these should all be distinct, except as required by the monoid laws), but there should be no 'extra' elements of ![M](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/6/9/6/69691c7bdcc3ce6d5d8a1361f22d04ac.png) in addition to those.

For monoids, the free structure over a set is given by the monoid of lists of elements of that set, with concatenation as multiplication. It should be easy to convince yourself of the following (in pseudo-Haskell):

M \= \[S\]
e \= \[\]
\* \= (++)

i : S \-> \[S\]
i x \= \[x\] \-- i x = x : \[\]

\[\] ++ xs \= xs \= xs ++ \[\]
xs ++ (ys ++ zs) \= (xs ++ ys) ++ zs

xs ++ ys \= ys ++ xs iff xs \== ys || xs \== \[\] || ys \== \[\]
\-- etc.

### The category connection

#### Free structure functors

One possible objection to the above description (even a more formal version thereof) is that the characterization of "simple" is somewhat vague. [Category theory](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/Category_theory "Category theory") gives a somewhat better solution. Generally, structures-over-a-set will form a category, with arrows being structure-preserving homomorphisms. "Simplest" (in the sense we want) structures in that category will then either be [initial or terminal](http://en.wikipedia.org/wiki/Initial_and_terminal_objects), \[1\] and thus, freeness can be defined in terms of such universal constructions.

In its [full categorical generality](http://en.wikipedia.org/wiki/Free_object), freeness isn't necessarily categorized by underlying set structure, either. Instead, one looks at "forgetful" functors \[2\] from the category of structures to some other category. For our free monoids above, it'd be:

*   ![U : Mon \to Set](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/5/c/0/5c093931bfe4402d5a4740a5d80b7a1b.png)

The functor taking monoids ![(M, e, *)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/b/f/2/bf21857e03e2c705c23ff4b3ae527730.png) to their underlying set ![M](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/6/9/6/69691c7bdcc3ce6d5d8a1361f22d04ac.png). Then, the relevant universal property is given by finding an [adjoint functor](http://en.wikipedia.org/wiki/Adjunction):

![F](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/8/0/0/800618943025315f869e4e1f09471012.png) being the functor taking sets to the free monoids over those sets. So, free structure functors are left adjoint to forgetful functors. It turns out this categorical presentation also has a dual: cofree structure functors are right adjoint to forgetful functors.

#### Algebraic constructions in a category

Category theory also provides a way to extend specifications of algebraic structures to more general categories, which can allow us to extend the above informal understanding to new contexts. For instance, one can talk about monoid objects in an arbitrary [monoidal category](http://en.wikipedia.org/wiki/Monoidal_category). Such categories have a tensor product ![\otimes](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/e/9/d/e9dd9013ec300ceba41484dfc2c9a876.png) of objects, with a unit object ![I](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/d/d/7/dd7536794b63bf90eccfd37f9b147d7f.png) (both of which satisfy various laws).

A monoid object in a monoidal category is then:

such that:

Where:

So, hopefully the connection is clear: we've generalized the carrier set to a carrier object, generalized the operations to morphisms in a category, and equational laws are promoted to being equations about composition of morphisms.

#### Monads

One example of a class of monoid objects happens to be [monads](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/Monad_(sans_metaphors) "Monad (sans metaphors)"). Given a base category ![C](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/0/d/6/0d61f8370cad1d412f80b84d143e1257.png), we have the monoidal category ![C^C](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/e/d/2/ed2b0f08a3ac18ca86897d20acfa111f.png):

If we then specialize the definition of a monoid object to this situation, we get:

which satisfy laws that turn out to be the standard monad laws. So, monads turn out to be monoid objects in the category of endofunctors.

#### Free Monads

But, what about our intuitive understanding of free monoids above? We wanted to promote an underlying set, but we have switched from sets to functors. So, presumably, a free monad is generated by an underlying (endo)functor, ![F : C \to C](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/9/f/4/9f4ec315e661344406b5d7f9a7c8b505.png). We then expect there to be a natural transformation ![i : F \to M](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/8/f/1/8f190becd439fadcc04be7a047482a74.png), 'injecting' the functor into the monad.

In Haskell, we can write the type of free monads over Haskell endofunctors as follows:

data Free f a \= Return a | Roll (f (Free f a))

instance Functor f \=> Monad (Free f) where
  return a       \= Return a
  Return a \>>= f \= f a
  Roll ffa \>>= f \= Roll $ fmap (\>>= f) ffa

  \-- join (Return fa) = fa
  \-- join (Roll  ffa) = Roll (fmap join ffa)

inj :: Functor f \=> f a \-> Free f a
inj fa \= Roll $ fmap Return fa

This should bear some resemblance to free monoids over lists. `Return` is analogous to `[]`, and `Roll` is analogous to `(:)`. Lists let us create arbitrary length strings of elements from some set, while `Free f` lets us create structures involving `f` composed with itself an arbitrary number of times (recall, functor composition was the tensor product of our category). `Return` gives our type a way to handle the 0-ary composition of `f` (as `[]` is the 0-length string), while `Roll` is the way to extend the nesting level by one (just as `(:)` lets us create (n+1)-length strings out of n-length ones). Finally, both injections are built in a similar way:

inj\_list x \= (:) x \[\]
inj\_free fx \= Roll (fmap Return fx)

This, of course, is not completely rigorous, but it is a nice extension of the informal reasoning we started with.

### Further reading

For those looking for an introduction to the necessary category theory used above, Steve Awodey's [Category Theory](http://www.math.uchicago.edu/~may/VIGRE/VIGRE2009/Awodey.pdf) is a popular, freely available reference.

### Notes

#### Universal constructions

Initial (final) objects are those that have a single unique arrow from (to) the object to (from) every other object in the category. For instance, the empty set is initial in the category of sets, and any one-element set is final. Initial objects play an important role in the semantics of algebraic datatypes. For a datatype like:

data T \= C1 A B C | C2 D E T

we consider the following:

The datatype `T` is then given by an initial F-algebra. This works out nicely because the unique algebra homomorphism whose existence is guaranteed by initiality is the [fold](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/Fold "Fold") or 'catamorphism' for the datatype.

Intuitively, though, the fact that `T` is an F-algebra means that it is in some sense closed under forming terms of shape F---suppose we took the simpler signature `FX = 1 + X` of the natural numbers; then both Z = inl () and Sx = inr x can be incorporated into Nat. However, there are potentially many algebras; for instance, the naturals modulo some finite number, and successor modulo that number are an algebra for the natural signature.

However, initiality constrains what Nat can be. Consider, for instance, the above modular sets 2 and 3. There can be no homomorphism ![h : 2 \to 3](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wikiupload/math/b/1/3/b134992839884ec878d4b476f5436035.png):

This is caused by these algebras identifying elements in incompatible ways (2 makes SSZ = Z, but 3 doesn't, and 3 makes SSSZ = Z, but 2 doesn't). So, the values of an initial algebra must be compatible with any such identification scheme, and this is accomplished by identifying _none_ of the terms in the initial algebra (so that h is free to send each term to an appropriate value in the target, according to the identifications there). A similar phenomenon occurs in the main section of this article, except that the structures in question have additional equational laws that terms must satisfy, so the initial structure _is_ allowed to identify those, _but no more_ than those.

By the same argument, we can determine that 3 is not a final algebra. Nor are the naturals (for any modular set M, S(h(M-1)) = S(M-1) = M, but h(S(M-1)) = h0 = 0). The final algebra is the set {0}, with S0 = 0 and Z = 0, with unique homomorphism hx = 0. This can be seen as identifying as many elements as possible, rather than as few. Naturally, final algebras don't receive that much interest. However, finality is an important property of [coalgebras](http://en.wikipedia.org/wiki/Initial_algebra#Final_coalgebra).

#### Forgetful functors

The term "forgetful functor" has no formal specification; only an intuitive one. The idea is that one starts in some category of structures, and then defines a functor by forgetting part or all of what defines those structures. For instance:

#### Natural transformations

The wiki article gives a formal definition of natural transformations, but a Haskell programmer can think of a natural transformation between functors F and G as:

trans :: forall a. F a \-> G a


[Source](https://wiki.haskell.org/Free_structure)