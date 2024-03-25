# Induction and coinduction

http://tunes.org/wiki/induction_20and_20co-induction.html

Induction and coinduction are duals and contrasting terms for ways to describing a system.

An **inductive definition** begins with a set of primitive objects with some primitive operations, and, using constructive operations on them, it iteratively defines a whole group of things based on those primitives.

Data types such as lists, sets, records, and natural numbers can all be defined this way (see Algebra and Coalgebra).

By contrast, there are datatypes such as streams, graphs (including things that graphs describe, such as state machines), and the rational numbers, whose elements can only be described in relation to each other, rather than to some other, core types (even though there are ways to implement these types in terms of other types, this is not the same as a definition).

Objects like these, having no well-founded means of deriving a definition, have to be treated with a more general kind of logic, one that doesn't rely strictly on *recursion*, but may look for self-consistency instead (and co-recursion).

Formally, the **induction principle** is defined as follows: an *algebra* is said to be *minimal* when it has no proper (i.e. different from itself) *subalgebra*. This is the induction principle for the algebra.

For instance, natural numbers with zero (0) and successor (ʹ) form a *minimal algebra* of the functor `1 + _`. Then, any subalgebra (i.e. any subset of the naturals which is closed wrt zero and succ) is the whole algebra of natural numbers. This yields the familiar proof by induction for naturals, where in order to prove that a property holds for all naturals, you just prove that it holds for zero, and that, if it holds for `x`, then it holds for `xʹ`.

The **coinduction principle** is defined dually. A *coalgebra* is said to be *simple* when it has no proper *quotients*. This is the coinduction principle for a coalgebra, but usually its practical and equivalent formulation is:
>when two distinct states are not *bisimilar*.

The induction and coinduction principles for initial and final algebra can also be used to define functions.

*Initial algebra* allows us to exploit the induction principle to define functions from its carrier type to a carrier type of another algebra.

Dually, *final coalgebras* allow us to exploit the coinduction principle to define function from a carrier type of another algebra to its carrier type.

For example, if we have an initial algebra `O` with signature `F` and a carrier type `A`, then every function `f : A → B` which is also a *homomorphism* from the initial algebra to some F-algebra `P` is uniquely defined.

An dually for the coinductive definitions of functions.

An *inductive definition* has a typical flavour, where the effect of `f` on the constructors of `I` is defined in terms of the arguments of the constructors applied to `f`, where `P` has the role of defining the equational constraints which uniquely determine `f`.

Dually, a *coinductive definition* has a typical flavour, where the effect of the deconstructors of the final coalgebra `I` on an application of `f` is defined in terms of the arguments of this application applied in some way to `f`, where `P` (the other coalgebra) defines the equational constraints which uniquely determine `f`.

For instance, let's compare the inductive definition of the `length` function (which takes a list and returns its length) with the coinductive definition of the `zip` function (which takes two streams and merges them by full shuffle):

```hs
-- inductive definition of length
len []         = 0
len (xs ++ ys) = len xs + len ys

-- coinductive definition of zip
head (zip (s1, s2)) = head s1
tail (zip (s1, s2)) = zip (s2, tail s1)
```

In the case of `len`, on the lhs, we have the function `len` on the outside, but on the rhs, the function is "pushed inside", while the operators of `P` appear on the outside.

In the case of `zip`, on the lhs, we have the deconstructors on the outside (`head` and `tail`), and on the rhs, these deconstructors are "pushed inside", while an expression on `f` appears on the outside.
