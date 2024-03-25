# Algebra and coalgebra

http://tunes.org/wiki/algebra_20and_20coalgebra.html

Algebra and coalgebra are terms used to describe classes of mathematical structures commonly encountered in mathematics and computer science.

The relationship between algebras and coalgebras appears clear only when their definition is formulated inside category theory.

Algebra and coalgebra are dual concepts. This duality has been observed informally for long time, with algebras used to describe data types, and coalgebras used to describe systems (i.e. abstract machines).

An **algebra** is commonly described as a set plus some operations on it, and as such it is usually used to formalize many kinds of data types in programming languages, like stacks.

Consider a set of the integers, ℤ, with a binary operation (+), a unary operation (-) and the number zero (a constant as a nullary operation). This is an algebra. The signature of an algebra is mostly affected by the arity of these operations.

For instance, we can consider these three operations as the unique "bundle operation" defined as follows:

This operation picks a pair of numbers and return their sum; or picks just one number and return its inverse; or picks no number at all (say, it picks a fixed non-numeric input we indicate with `*`) and returns the number zero. In fact, the identity element is represented as a function `{*} -> 1`, where `{*}` is a one element set (with the only element labelled `*`). `1` is the identity element of ℤ wrt (+), i.e. 0.

```hs
e : {*} -> ℤ
e * = 0

-- or
identity :: () -> ℤ
identity () = 0
```

Let's indicate with `A × B` the Cartesian product of two sets A and B, and the special case `A × A` by `A²`. We also indicate with `1` the singleton set, `1 = {*}`, and the disjoint union of sets `A` and `B` by `A + B`.

By synthesizing, the 3 operations of our algebra can be seen as a single function, `(ℤ² + ℤ + 1) -> ℤ`.

The signature of this algebra can be written as `_² + _ + 1`, where `_` is a placeholder for the carrier type of choice (here ℤ).

Considering a generic signature `F`, an algebra with signature `F` and a carrier type `A` is any function `F(A) -> A`, i.e. any function picking a complexly structured value, among whose subparts may be values of type `A`, and returning a value of type `A`.


**Coalgebras** are defined dually.

Given a signature `F`, a coalgebra with signature `F` and a carrier type `A` is any function `A -> F(A)`, i.e. any function picking a value from `A` and returning a complexly structured value, among whose subparts there may be values from `A`.


As algebras are used to formalize data types, coalgebras are used to formalize automata and similar computational systems, or assimilable data types like streams.

Consider a finite state automaton with states in the set S, with I as input alphabet and with O as output alphabet. An automaton is defined by its transition function, taking a pair (state, input) and returning a pair (new_state, output). In word, by a function `S×I -> S×O`.

By currying, any function of two arguments can be conveniently described by an equivalent function consuming its first argument, and returning another function which consumes the second one.

This means, any function `S×I -> S×O` can be conveniently described by an equivalent function `S -> (S×O)ᶦ`, where `(S×O)ᶦ` is the set of all the functions from `I` to `S×O`.

In brief, an automaton with input alphabet I and output alphabet O is a coalgebra with signature `(_ × O)ᶦ`, whose carrier is the set of its states.

Algebras and coalgebras have similar properties.

For instance, there is a class of algebras, *initial algebras*, allowing proof and definition of functions by induction.

The corresponding dual class of coalgebras, *final coalgebras*, allow proof and definition by coinduction.


Note that the notation `Bᴬ` to mean the set of all the total functions from A to B does NOT clash with the notation where `A²` was used to indicate `A×A`. Indeed, if 2 is the set with two elements, `2 = {*, **}`, a function `2 -> A` is an ordered pair, mapping `*` to its car and `**` to its cdr. Then the set of all these functions is, precisely, the set of all these pairs, i.e. the Cartesian product `A×A`. More precisely, it is isomorphic to it.

In category theory, *signatures are represented by functors*. Functors based on the combination of coproducts (disjoint union), products (cartesian product) and exponentiation `_ⁿ` are callled *polynomial functors*. Polynomial functors have pleasant properties and are common signatures for many interesting algebras (the example algebra had precisely a polynomial functor as its signature). Unfortunately, interesting coalgebras usually have functors more complex than polynomial ones.
