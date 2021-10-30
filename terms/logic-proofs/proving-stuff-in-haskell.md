---
created: 2021-08-20T07:15:58 (UTC +02:00)
tags: []
source: https://www.madsbuch.com/proving-stuff-in-haskell
author: 
---

# Proving Stuff in Haskell - Mads Buch [dot] Com

> ## Excerpt
> Main point: Haskell has the capacity to carry out proofs as languages.

---
_Main point: Haskell has the capacity to carry out proofs as languages._

_updated april 2021 for clarity._

This article is my give on the relationship between mathematical proofs and programming languages. Many details on specific implementation have been left out with the aim for clarity and conceptual coherency.

The source used in this article is available as [a Gist][1].

What is a proof? A proof is a series of deductive arguments, such that the proposition is justified. This description might seem quite abstract, so let us look at a concrete example using the Peano naturals for representing natural numbers (here written in number syntax).

As the proposition has no quantifiers, we can directly begin to reduce on defined operations. For Peano numerals addition (+) is well defined and we carry out steps as long as it is possible. In the end we should arrive at something akin to

We still have a mathematical object, a propositional claim. We know from the Peano axioms, that syntactical equivalence satisfies reflexivity, symmetry, and transitivity. Henceforth the properties of equality are satisfied, and we end our deductive sequence. QED.

This can be translated into programming utilizing the type system to verify that we are not "cheating" on deductive arguments. This translation has basis in what known as the [Curry–Howard correspondence][2]. From that we know that propositions corresponds to types and proofs corresponds to programs. Alright, so we need to make an expression that has above proposition as its type, and an implementation that satisfies the type.

## In Haskell

First, we need to define our objects: Peano naturals and Equality. We implement Peano naturals the usual way. I elaborate on this in a [previous post][3]. Equality is defined as follows.

```
data :~: where  Refl :: a :~: a
```

We can see that the value `Refl` is the only inhabitant of the type `a :~: b`. Furthermore, it only inhabits the type when `a` and `b` are identical. Concerning the Curry-Howard correspondence, the `Refl` value also has the unit type - it is not possible to attach further data to the constructor.

We have defined equality in terms of reflection. However, we need equality also to satisfy symmetry and transitivity. In the source, we have the code needed for that.

We now want to make the type for our proof, or, the equivalent to the proposition stated above:

```
onePlusOneEqualsTwo :: Add (S Z) (S Z) :~: S (S Z)
```

The `Add` in the type definition is a type family defined in the source. It is defined as we would usually define addition over Peano naturals.

To prove it we need to make an inhabitant to that type. The program is very simple for this case as the Haskell compiler reduces the type level expression per semantics of type families.

```
onePlusOneEqualsTwo = Refl
```

As it compiles[compiler][4] it shows that Haskell is content with the proof.

Reusing proofs seems like a convenience, though it is completely necessary for some classes of proofs. These are namely the _proofs by induction_ which we will have a look at later. For this to work we need to be able to lift a proof into Haskell's type solver.

We do that by the `gcastWith` operator. This operator has the type signature `gcastWith :: a :~: b -> (a ~ b => r) -> r`. It should be read as something like: Given a proof that `a` equals `b`, use that fact to decide if the type `r` is solvable and return the proof for `r`.

We want to abstract our proofs. In proving terminology, we do this through quantifiers.

To have a more graspable problem that includes only quantification, without induction, we detour to boolean algebra. Here we can try to formalize De Morgan's theorem:

here _a_ and _b_ can only assume two values, _true_ and _false_.

```
deMorgan :: SBool a -> SBool b -> Not (And a b) :~: Or (Not a) (Not b)deMorgan STrue STrue   = Refl -- The first casedeMorgan STrue SFalse  = RefldeMorgan SFalse STrue  = RefldeMorgan SFalse SFalse = Refl
```

We simply provide an inhabitant to the type based on pattern matching. This is the same as proving by case analysis.

To understand what goes on we instantiate the type expression in each case. Afterward, the compiler reduces per the semantics of the `Not`, `And`, and `Or` type families. In the first case we instantiate the type expression such that.

```
deMorgan :: SBool Tru -> SBool Tru -> Not (And Tru Tru) :~: Or (Not Tru) (Not Tru)
```

This instantiation is from the value `STrue` which has the type `SBool Tru`. The compiler then reduces the expression and derives that `Refl :: Fls :~: Fls`. From the interpreter, we get that.

```
*Proof> :t deMorgan STrue STruedeMorgan STrue STrue :: 'Fls :~: 'Fls
```

Many interesting properties we want to reason about includes unbound data. That is, the data we think about is inductively defined. We now go back to the examples considering natural numbers as they are a good medium for discussing inductively defined data.

`plus_id_r` is the property that adding zero to _n_ on the right side is the identity of _n_. `plus_id_l` is when we add 0 on the left side.

`plus_id_l` is given directly from our definition of addition. But `plus_id_r` needs to be proven, and we can do this inductively using following code.

```
plus_id_r :: forall n. SNat n -> (Add n Z) :~: nplus_id_r Zero = Reflplus_id_r (Succ x) = gcastWith (plus_id_r x) Refl
```

The first case is the base case. We know that the value is `Zero` and hence we can derive the type to `Z`. It is immediately visible that `Refl` inhabits the type `Refl Z Z`.

The next case is the induction case. Here we fold out the value such that if `n = Succ x`, then `x = n-1` - we reduce this on our argument. We justify that `Refl` also is an inhabitant in this case by calling `plus_id_r` on the reduced value.

It is indeed possible to prove stuff in Haskell. But it is not further practical. The reason is in particular because the language is not designed with the constructions we need, such as dependent types. We simulate them through GADTs.

The closest languages to Haskell that are suited for this is languages such as Idris and Gallina (Coq). They have all the facilities needed for incorporating proofs into one's code.

If one has a software development background firmly grounded in OOP (Java, C#), it requires quite some time to wrap one's head around the new way to understand types.

That we can do above is mostly of academic interest: How do make _sure_ that certain compilers indeed do what they should do etc. But the techniques are becoming steadily more accessible to all programmers.

New languages, like Idris, come with type constructions to formally reason about our software.

First, thanks to [Aslan Askarov][5] for providing valuable feedback on this article. It has been incorporated to provide a more coherent article.

This article was a precursor for a presentation at a [spare time teaching][6] event at Aarhus university. Thanks to them for letting me host the presentation.

Alexandros Peitsinis has written a much more in-depth article on this subject. If this article intrigued then take a look at [his work][7].

[1]: https://gist.github.com/madsbuch/12043c4ad1c1fd0a80008ffb443e29d7
[2]: https://en.wikipedia.org/wiki/Curry%E2%80%93Howard_correspondence
[3]: https://www.madsbuch.com/100-days-of-fibonacci-day-9-haskell-types/
[4]: https://www.madsbuch.com/proving-stuff-in-haskell#fn-compiler
[5]: http://askarov.net/
[6]: http://www.sparetimeteaching.dk/about.php
[7]: https://alexpeits.github.io/posts/2018-09-27-haskell-proofs.html
