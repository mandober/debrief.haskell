# Dependent Haskell

Haskell is not a dependently typed language, but it can get close using type trickery and encodings to push the limits of its type system.

Although realization of concepts from dependently typed languages still looks significantly different expressed in Haskell from the same concept expressed in e.g. Agda, that gap is slowely getting smaller as the GHC gets bigger and better with new extensions to the type system, such as GADTs, type families, datatype promotion.

However, there remains a significant difference between programming in Haskell and in full-spectrum dependently typed languages. Haskell enforces a *phase separation* between runtime values and compile-time types. Therefore, singleton
types are necessary to express the dependency between values and
types. These singleton types introduce overhead and redundancy
for the programmer.
This paper presents the singletons library, which generates the
boilerplate code necessary for dependently typed programming
using GHC. To compare with full-spectrum languages, we present
an extended example based on an Agda interface for safe database
access. The paper concludes with a detailed discussion on the
current capabilities of GHC for dependently typed programming
and suggestions for future extensions to better support this style of
programming.

phantom types [Leijen and Meijer 1999], to nested datatypes [Bird
and Paterson 1999; Okasaki 1999], to a higher-order polymorphism
encoding of Leibniz equality [Baars and Swierstra 2002; Cheney
and Hinze 2002; Weirich 2004], to overlapping type classes [Kiselyov et al. 2004], to a tagless algebra [Carette et al. 2009], to functional dependencies [Guillemette and Monnier 2008a; McBride
2002]. The flexibility of the Haskell type system and the ingenuity of Haskell programmers have been demonstrated beyond doubt.
However, the cleverness of these encodings is also their drawback. Although the ideas behind the encodings are inspired
by dependently typed programs, the code does not look like
code in any full-spectrum dependently typed language, such as
Cayenne [Augustsson 1998], Coq [Coq development team 2004],
Epigram [McBride 2004], or Agda [Norell 2007]. As a result,
several authors [Guillemette and Monnier 2008b; McBride 2002;
Neubauer and Thiemann 2002] have pushed for more direct mechanisms, and GHC implementors have responded with Generalized Algebraic Datatypes (GADTs) [Cheney and Hinze 2002; Peyton Jones et al. 2006; Schrijvers et al. 2009; Xi et al. 2003], typelevel functions [Chakravarty et al. 2005], and type-level datatypes
with kind polymorphism [Yorgey et al. 2012]. These additions
provide native support for constrained data (replacing the use of
phantom types, nested datatypes, and type equality encodings) and
type-level computation (replacing the use of logic programming
with type classes and functional dependencies)
