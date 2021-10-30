# Universal and existential quantification

*Universal quantification*, denoted by `∀`, means that a logical statement of the form `∀x. Px` holds for all the possible instances of `x` taken from the set that is being quantified over. (`P` denotes a property, in general, a predicate).

*Existential quantification*, denoted by `∃`, means that a logical statement of the form `∃x. Px` holds for some unspecified element `x` taken from the set that is being quantified over.

In PLs, the sets being quantified over are types, that is, types are sets. The types, which is especially evident in type signatures, are logical statements, and so they can be assigned a truth value; however, it's more practical to replace the notion of "true" with "implementable" (then "false" means that a function cannot be defined).

## Universal quantification

A universally quantified type like `forall a. (a -> a)` means that, for any choice of `a`, i.e. for any argument the caller chooses, the implementation can return something reasonable; i.e. it can return that same argument right back since that's the only reasonable thing to do. It cannot return anything else because `a` represents a polymorphic type which cannot be examined nor manipulated, it is an unknown value of an unknown (but reasonable) type.

If types correspond to logical propositions, and a true proposition corresponds to an implementable type signature, and, since the proposition `a -> a` is always true (i.e. it is a tautology), then we can define such a function:

```hs
-- implicit universal quantification
id :: a -> a
id x = x

-- explicit quantification, implicit parens
id :: forall a. a -> a
id x = x

-- explicit quantification, explict parens, λ-style definition
id :: forall a. (a -> a)
id = \ x -> x

-- λω style
id :: Λa. ∀a. (a -> a)
id = \ x -> x
```

Again, a universally quantified type `forall a. (a -> a)` means that, for any possible type `a`, we can implement a function whose type is `a -> a`.

In Haskell, universal quantifier is implicit by default. All type variables in a signature are implicitly universally quantified. This is also known as *parametric polymorphism*, in Haskell often shortened to "polymorphism", and in other languages (e.g. C#) referred to as *generics*.

## Existential quantification

An existentially quantified type like `∃a. (a -> a)` means that, for some particular type `a`, we can implement a function whose type is `a -> a`. Any function will do, so pick one:

```hs
func :: ∃a. a -> a
func True  = False
func False = True
```

which is the `not` function on Booleans. However, the catch is that we can't use it, because all we know about the type `a` is that it exists; any information about which type it might be has been discarded, meaning we can't apply such functions to anything.

Essentially, the things you can do with something that has an existential type are the things you can do based on its non-existential aspects.

We can, however, compose it with itself since we know, at least, that its input and output type are the same.

Similarly, given something of type `∃a. [a]` we can find its length, or concatenate it to itself, or drop some elements, or anything else we can do to a list of a's, [a].

That last bit brings us back around to universal quantifiers, and the reason why Haskell doesn't have existential types directly: since things with existentially quantified types can only be used with operations that have universally quantified types.

The type `∃a. a` may be written as `forall r. (forall a. a -> r) -> r`, meaning, for all result types `r`, given a function that, for all types `a`, takes an argument of type `a` and returns a value of type `r`, we can get a result of type `r`.

Note that the overall type is not universally quantified for `a`, rather, it takes an argument that itself is universally quantified for `a`, which it can then use with whatever specific type it chooses.

The caller picks a type for `r`, but the implementation picks a type for `a`.

```hs
-- (a -> r) -> r
cont :: forall r. (forall a. a -> r) -> r -- ≅ ∃a. a
```

As an aside, while Haskell doesn't really have a notion of subtyping in the usual sense, we can treat quantifiers as expressing a form of subtyping, with a hierarchy going *from universal to concrete to existential*. Something of type `forall a. a` could be converted to any other type, so it could be seen as a *subtype of everything*. On the other hand, any type could be converted to the type `exists a. a`, making that a *supertype of everything*. Of course, the former is impossible (there are no values of type `forall a. a` except bottoms like `error` and `undefined`), while the latter is useless (you can't do anything with the type `exists a. a`), but the analogy works at least on paper.

Note that the equivalence between an existential type and a universally quantified argument works for the same reason that *variance flips for function inputs*.
