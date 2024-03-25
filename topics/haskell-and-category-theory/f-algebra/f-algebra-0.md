# F-algebra

## Monoids

A *Monoid* may be defined in terms of a set, or as a single-object category `Mon`, or as an object in a *Monoidal Category*. And now, let's focus on the definition of a monoid as an algebraic structure informed by category theory.

A monoid is defined a set `m` with a pair of functions `μ` and `η`:

```hs
μ : m × m -> m
η : 1 -> m
```

Monoid
- `m` is a carrier set
- `μ` is an associative binary operation closed over `m`
- `η` is the identity function that selects the identity element from `m`
- `1` is the terminal object in `Set` category, `∀x. x -> 1`
- `1` is an element selector in `Set` category, `∀x∃n.|1 -> x| = n` for n = |x| since |1 -> x| = x¹ = x and |x| = n

The object `1` is a terminal object in `Set` category, but in set theory this is a singletion set. Still in set theory, there is 1 function from any set to a singletion set since `|a -> 1| = 1ᵃ = 1`. This translates into a single incoming arrow from any object into `1` in `Set` category, making `1` a terminal object. On the other hand, in set theory, there are exactly `n` distinct functions going from a singleton set to a set wih `n` elements; this means that each of those functions `1 -> a` is acting as an element selector. So in category `Set`, those functions form a singleton set to any set become arrows form `1` to any object, and each one of these arrows can still be viewed as selecting a particular element. It is thanks to these relations that we can tell cardinality of objects even in `Set` category.

- `Set` has the unique initial object `I` (the empty set).
- `Set` has a terminal object `T` (a singleton set).

The object denoted by `1` is the terminal object in the Set category, but, with the arrows from it to other objects, it can be viewed as an element selector since there are as many arrows from `1` to an object as there are elements in that other object - each arrow selects a particular element.

So, despite the fact that sets appear as opaque objects in category Set, these arrows exactly correspond to a set's elements. This is also the reason that `η` is a unary function and not a constant: more precisely, we can't reference an identity element itself (e.g. the element `1` in the set ℕ, which appears as a structureless object in the category Set), but we can refer to the "selection morphism" of type `1 -> m`, that can pinpoint elements. So the type `1 -> m` is the type of all the morphisms that pick elements of `m` - there are as many of these morphisms as there are elements in `m`. Then the `η` function somehow picks the identity element of `m`.

[A/N] Just how will `η` pick the identity element exactly - and not just some rando - is a complete mystery! Sure, there's a bunch of morphism from `1` to `m`, and `η` is one of them, but @#&* conceptually! `η` can only be a label that was slapped onto some arrow by someone (?!), but did they even correctly determine that is really leads to the identity element? Maybe they lied? Maybe it was done by ancient aliens, capable of peering through spacetime, to way back when these objects were transparent sets? Are we to trust these alians? And can we even read these scribbles?! I mean, `η` is identity, like wtf? Or perhaps `η` is one of those don't-ask magic functions, with an oracle up the ass, so it always spits the correct result? 
(later) Wait a minute, what about the composition? Maybe the composition will reveal a clue, knowing that the identity element as one operand acts neutral, so the operation just returns the other operand intact. But since this is a monoidal category, and it has a single object with a myriad of arrows possibly, all the arrows, composed or not, start and end up at the same object, so… wtf still? I mean, how are we even to take this, even for granted? Is `η` just a value of type `m`? Guess not, in that case it would be a constant, `η :: m`, and not a unary function `η :: 1 -> m`. It may be a const fn, that called with anything, just returns an (idenity) element from `m`. Let aside that its impl is never given, Bartosz later refers to it as a nullary op?! So it a nullary opearation (i.e. a constant value) or a unary function, for godsake?!
