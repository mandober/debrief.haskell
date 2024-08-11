# Functions

https://wiki.haskell.org/Function
https://wiki.haskell.org/Higher_order_function
https://www.haskell.org/tutorial/goodies.html
https://www.haskell.org/tutorial/functions.html
https://www.haskell.org/onlinereport/decls.html#type-semantics
https://ncatlab.org/nlab/show/function+type
https://ncatlab.org/nlab/show/function
https://ncatlab.org/nlab/show/function+application
https://ncatlab.org/nlab/show/partial+function
https://ncatlab.org/nlab/show/multi-valued+function
https://ncatlab.org/nlab/show/function+set

# Functions at the term-level

>Mathematical function associates each element of the domain set `A` to at most one element of the codomain set `B`, and is denoted by `f : A -> B` where `f` is an arbitrary name given to it.

Note the universal quantifier when talking about domain elements, and the existential quantifier when talking about codomain elements.

`∀a ∈ A. ∃!b ∈ B. f(a) = b`

Expanding the `!∃` we get:

`∀a ∈ A. ∃b∃c ∈ B. (f(a) = b ∧ f(a) = c) ⇒ (b = c)`

which shows the most important property of a function:
>A domain element must be associated with exactly one codomain element.
If any domain element happens to be associated with two or more codomain elements, such construction is not a function but a relation.

Functions are special kinds of relations. All functions are relations, but not vice versa. On the other hand, the important property that relations bring to the table, that will later be reused by functions, is *directionality*: a relation goes from a source (domain) set into a target (codomain) set. Relations are directional: *from* a set *into* a set. Even the empty relation has an implied direction although it doesn't exercise it. Unlike a function, a relation does not require participation of all dom elements; however, the association of those elements that do participate needs to "traverse the gap" between two sets - as the directionality dictates. It cannot happen that a domain element gets associated to itself or to another domain element - each associated dom el must cross the gap. This is certainly true in heterogenenous relation, but even homogenous relations (which relate a set to itself) are modelled as two sets and it just happens that the cod and dom are the same set; in the model (diagram) all arrows (representing associations) still must cross the gap.


Not a function (but a fine relation):

```
        b₁
      ⟋
    ⟋
  ⟋
a
  ⟍
    ⟍
      ⟍
        b₂
```


* A *binary relation* associates a set `A` to a set `B`, `R ⊆ A×B`.
* A *heterogeneous binary relation* associates a set `A` to a distinct set `B` (`A≠B`), `R ⊆ A×B`.
* A *homogeneous binary relation* associates a set `A` to a set `B`, but `A=B`, so `R ⊆ A×A`, i.e. `R ⊆ A²` (or, `R ⊆ A×B` and `A=B`).


The directionality also encodes the constraint that each domain element can only ever be associated with a number of codomain elements (and not to domain elements).



It is not easy to correctly state many-to-many relation between two sets.



The implied quantifier for codomain is the existential quantifier, possibly realized as the adjective 'some' in the sentence above (as in: "… to *some* elements of a target set …")



relates *all values* in a set A to values in a set B.


The function is an integer, will map all elements of the set of integers into another set -- in this case the set of square integers.

In Haskell, functions can be specified as below in the examples. The *signature*, aka the *type of the function*, or *function's type* is a **function type**.

The function type `Bool -> ()` is a type, an inhabited type at that. Like any inhabited type it classifies values. A value that belongs to this type is a function `\x -> ()` (a lambda function) that sends both `True` and `False` to `()`. Moreover, there is exactly 1 value in the function type `Bool -> ()`, which is exactly this function we name `unit`.

```hs
unit :: Bool -> ()
unit True  = ()
unit False = ()

-- | it may as well be implemented as:
unit :: Bool -> ()
unit _ = ()
```

Another value that belongs to this type is a lambda `\b -> ()` that sends `False` to `()`. 


## Function types

`->` is also a type constructor: given two types `a` and `b`, `a -> b` is the type of functions mapping elements of type `a` to elements of type `b`. Curiously, the function type ctor `->` has no data ctor, at least not one we can express. Semantically, and internally, `->` type surely has something of a data ctor, it's just that we cannot express it in the surface syntax. This is contrasted by other builtin types, which we can express, e.g. `[]` and `(,)`. T

That is, we can very well imagine that the types of pairs and lists are defined something like:

```hs
data (a, b) = (a, b)
data [a] = [] | a : [a]
```

but we cannot iamgine, or write, the possible implementation of the function type, at least not the right-hand side.

```hs
data a -> b = ???
```

The *builtin* function type is truly *abstract* and *opaque* type

```hs
-- an opaque (abstract) type lacks the right-hand side:
data a -> b
data Opaque
data Abstract
```

Even integers could be defined as a very long sum types...


We can, however, define a custom function type, *but only in terms of the builtin function type*:

```hs
data Op a b = Op (b -> a)
```
