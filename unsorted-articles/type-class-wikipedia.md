# Type class - Wikipedia

> This article is about polymorphic type systems in computer science. For the mathematical class of order-types of a given cardinality, see Glossary of set theory § T.

This article is about polymorphic type systems in computer science. For the mathematical class of order-types of a given cardinality, see [Glossary of set theory § T](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Glossary_of_set_theory#T "Glossary of set theory").

In [computer science](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Computer_science "Computer science"), a **type class** is a [type system](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Type_system "Type system") construct that supports [ad hoc polymorphism](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Ad_hoc_polymorphism "Ad hoc polymorphism"). This is achieved by adding constraints to type variables in [parametrically polymorphic](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Parametric_polymorphism "Parametric polymorphism") types. Such a constraint typically involves a type class `T` and a [type variable](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Type_variable "Type variable") `a`, and means that `a` can only be instantiated to a type whose members support the overloaded operations associated with `T`.

Type classes were first implemented in the [Haskell programming language](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Haskell_(programming_language) "Haskell (programming language)") after first being proposed by [Philip Wadler](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Philip_Wadler "Philip Wadler") and Stephen Blott as an extension to "eqtypes" in [Standard ML](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Standard_ML "Standard ML"),[\[1\]](#cite_note-1)[\[2\]](#cite_note-2) and were originally conceived as a way of implementing [overloaded arithmetic and equality operators](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Operator_overloading "Operator overloading") in a principled fashion.[\[3\]](#cite_note-kaes88parametric-3)[\[4\]](#cite_note-wadler88how-4) In contrast with the "eqtypes" of Standard ML, overloading the equality operator through the use of type classes in Haskell does not require extensive modification of the [compiler frontend](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Compiler_frontend "Compiler frontend") or the underlying type system.[\[5\]](#cite_note-appel91standard-5)

Since their creation, many other applications of type classes have been discovered.

Overview\[[edit](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Type_class&action=edit&section=1 "Edit section: Overview")\]
---------------------------------------------------------------------------------------------------------------------------------------------------

Type classes are defined by specifying a set of function or constant names, together with their respective types, that must exist for every type that belongs to the class. In Haskell, types can be parameterized; a type class `Eq` intended to contain types that admit equality would be declared in the following way:

class Eq a where
  (\==) :: a \-> a \-> Bool
  (/=) :: a \-> a \-> Bool

where `a` is one instance of the type class `Eq`, and `a` defines the function signatures for 2 functions (the equality and inequality functions), which each take 2 arguments of type `a` and return a boolean.

The type variable `a` has [kind](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Kind_(type_theory) "Kind (type theory)") ![*](https://wikimedia.org/api/rest_v1/media/math/render/svg/8e9972f426d9e07855984f73ee195a21dbc21755) (also known as `Type` in the latest [GHC](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Glasgow_Haskell_Compiler "Glasgow Haskell Compiler") release),[\[6\]](#cite_note-6) meaning that the kind of `Eq` is

The declaration may be read as stating a "type `a` belongs to type class `Eq` if there are functions named `(==)`, and `(/=)`, of the appropriate types, defined on it". A programmer could then define a function `elem` (which determines if an element is in a list) in the following way:

elem :: Eq a \=> a \-> \[a\] \-> Bool
elem y \[\]     \= False
elem y (x:xs) \= (x \== y) || elem y xs

The function `elem` has the type `a -> [a] -> Bool` with the context `Eq a`, which constrains the types which `a` can range over to those `a` which belong to the `Eq` type class. (_Note_: Haskell `=>` can be called a 'class constraint'.)

A programmer can make any type `t` a member of a given type class `C` by using an _instance declaration_ that defines implementations of all of `C`'s methods for the particular type `t`. For instance, if a programmer defines a new data type `t`, they may then make this new type an instance of `Eq` by providing an equality function over values of type `t` in whatever way they see fit. Once they have done this, they may use the function `elem` on `[t]`, that is, lists of elements of type `t`.

Note that type classes are different from [classes](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Class_(computer_science) "Class (computer science)") in object-oriented programming languages. In particular, `Eq` is not a type: there is no such thing as a _value_ of type `Eq`.

Type classes are closely related to parametric polymorphism. For example, note that the type of `elem` as specified above would be the parametrically polymorphic type `a -> [a] -> Bool` were it not for the type class constraint "`Eq a =>`".

Higher-kinded polymorphism\[[edit](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Type_class&action=edit&section=2 "Edit section: Higher-kinded polymorphism")\]
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

A type class need not take a type variable of [kind](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Kind_(type_theory) "Kind (type theory)") `Type` but can take one of any kind. These type classes with higher kinds are sometimes called constructor classes (the constructors referred to are type constructors such as `Maybe`, rather than data constructors such as `Just`). An example is the [`Monad`](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Monad_(functional_programming) "Monad (functional programming)") class:

class Monad m where
  return :: a \-> m a
  (\>>=)  :: m a \-> (a \-> m b) \-> m b

The fact that m is applied to a type variable indicates that it has kind `Type -> Type`, i.e. it takes a type and returns a type, the kind of `Monad` is thus:

Monad :: (Type \-> Type) \-> Constraint

Multi-parameter type classes\[[edit](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Type_class&action=edit&section=3 "Edit section: Multi-parameter type classes")\]
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Type classes permit multiple type parameters, and so type classes can be seen as relations on types.[\[7\]](#cite_note-7) For example, in the [GHC](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Glasgow_Haskell_Compiler "Glasgow Haskell Compiler") standard library, the class `IArray` expresses a general immutable array interface. In this class, the type class constraint `IArray a e` means that `a` is an array type that contains elements of type `e`. (This restriction on polymorphism is used to implement [unboxed](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Object_type_(object-oriented_programming)#Unboxing "Object type (object-oriented programming)") array types, for example.)

Like [multimethods](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Multimethod "Multimethod")\[_[citation needed](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Wikipedia:Citation_needed "Wikipedia:Citation needed")_\], multi-parameter type classes support calling different implementations of a method depending on the types of multiple arguments, and indeed return types. Multi-parameter type classes do not require searching for the method to call on every call at runtime;[\[8\]](#cite_note-8):minute 25:12 rather the method to call is first compiled and stored in the dictionary of the type class instance, just as with single-parameter type classes.

Haskell code that uses multi-parameter type classes is not portable, as this feature is not part of the Haskell 98 standard. The popular Haskell implementations, GHC and [Hugs](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Hugs "Hugs"), support multi-parameter type classes.

Functional dependencies\[[edit](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Type_class&action=edit&section=4 "Edit section: Functional dependencies")\]
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

In Haskell, type classes have been refined to allow the programmer to declare functional dependencies between type parameters—a concept [inspired from relational database theory](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Functional_dependency "Functional dependency").[\[9\]](#cite_note-9)[\[10\]](#cite_note-10) That is, the programmer can assert that a given assignment of some subset of the type parameters uniquely determines the remaining type parameters. For example, a general [monad](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Monad_(functional_programming) "Monad (functional programming)") `m` which carries a state parameter of type `s` satisfies the type class constraint `Monad.State s m`. In this constraint, there is a functional dependency `m -> s`. This means that for a given monad `m` of type class `Monad.State`, the state type accessible from `m` is uniquely determined. This aids the compiler in [type inference](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Type_inference "Type inference"), as well as aiding the programmer in [type-directed programming](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Type-directed_programming&action=edit&redlink=1 "Type-directed programming (page does not exist)").

[Simon Peyton-Jones](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Simon_Peyton-Jones "Simon Peyton-Jones") has objected to the introduction of functional dependencies in Haskell on grounds of complexity.[\[11\]](#cite_note-11)

Type classes and implicit parameters\[[edit](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Type_class&action=edit&section=5 "Edit section: Type classes and implicit parameters")\]
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Type classes and implicit parameters are very similar in nature, although not quite the same. A polymorphic function with a type class constraint such as:

can be intuitively treated as a function that implicitly accepts an instance of `Num`:

sum\_ :: Num\_ a \-> \[a\] \-> a

The instance `Num_ a` is essentially a record that contains the instance definition of `Num a`. (This is in fact how type classes are implemented under the hood by the Glasgow Haskell Compiler.)

However, there is a crucial difference: implicit parameters are more _flexible_ – you can pass different instances of `Num Int`. In contrast, type classes enforce the so-called _coherence_ property, which requires that there should only be one unique choice of instance for any given type. The coherence property makes type classes somewhat antimodular, which is why orphan instances (instances that are defined in a module that neither contains the class nor the type of interest) are strongly discouraged. On the other hand, coherence adds an additional level of safety to the language, providing the programmer a guarantee that two disjoint parts of the same code will share the same instance.[\[12\]](#cite_note-12)

As an example, an ordered [set](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Set_(abstract_data_type) "Set (abstract data type)") (of type `Set a`) requires a [total ordering](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Total_order "Total order") on the elements (of type `a`) in order to function. This can be evidenced by a constraint `Ord a`, which defines a comparison operator on the elements. However, there can be numerous ways to impose a total order. Since set algorithms are generally intolerant of changes in the ordering once a set has been constructed, passing an incompatible instance of `Ord a` to functions that operate on the set may lead to incorrect results (or crashes). Thus, enforcing coherence of `Ord a` in this particular scenario is crucial.

Instances (or "dictionaries") in [Scala](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Scala_(programming_language) "Scala (programming language)") type classes are just ordinary values in the language, rather than a completely separate kind of entity.[\[13\]](#cite_note-scala-typeclasses-13)[\[14\]](#cite_note-14) While these instances are by default supplied by finding appropriate instances in scope to be used as the implicit actual parameters for explicitly-declared implicit formal parameters, the fact that they are ordinary values means that they can be supplied explicitly, to resolve ambiguity. As a result, Scala type classes do not satisfy the coherence property and are effectively a syntactic sugar for implicit parameters.

This is an example taken from the Cats [\[15\]](#cite_note-15) documentation:

// A type class to provide textual representation
trait Show\[A\] {
  def show(f: A): String
}

// A polymorphic function that works only when there is an implicit 
// instance of Show\[A\] available
def log\[A\](a: A)(implicit s: Show\[A\]) \= println(s.show(a))

// An instance for String
implicit val stringShow \= new Show\[String\] {
  def show(s: String) \= s
}

// The parameter stringShow was inserted by the compiler.
scala\> log("a string")
a string

[Coq](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Coq "Coq") (version 8.2 onwards) also supports type classes by inferring the appropriate instances.[\[16\]](#cite_note-16) Recent versions of [Agda](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Agda_(theorem_prover) "Agda (theorem prover)") 2 also provide a similar feature, called "instance arguments".[\[17\]](#cite_note-17)

Other approaches to operator overloading\[[edit](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Type_class&action=edit&section=6 "Edit section: Other approaches to operator overloading")\]
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

In [Standard ML](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Standard_ML "Standard ML"), the mechanism of "equality types" corresponds roughly to Haskell's built-in type class `Eq`, but all equality operators are derived automatically by the compiler. The programmer's control of the process is limited to designating which type components in a structure are equality types and which type variables in a polymorphic type range over equality types.

SML's and [OCaml](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/OCaml "OCaml")'s modules and functors can play a role similar to that of Haskell's type classes, the principal difference being the role of type inference, which makes type classes suitable for _ad hoc_ polymorphism.[\[18\]](#cite_note-dreyer06modular-18) The object oriented subset of [OCaml](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/OCaml "OCaml") is yet another approach which is somewhat comparable to the one of type classes.

\[[edit](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Type_class&action=edit&section=7 "Edit section: Related notions")\]
--------------------------------------------------------------------------------------------------------------------------------------------------

An analogous notion for overloaded data (implemented in [GHC](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Glasgow_Haskell_Compiler "Glasgow Haskell Compiler")) is that of [type family](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Type_family "Type family").[\[19\]](#cite_note-19)

In [C++](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/C%2B%2B "C++") notably C++20 has excellent support for type classes using the [Concepts\_(C++)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Concepts_(C%2B%2B) "Concepts (C++)").

As an illustration, the above mentioned Haskell example of typeclass Eq would be written as

template <typename T\>
concept Equal \=
      requires (T a, T b) {
            { a \== b } \-> bool;
            { a != b } \-> bool;
};

In [Clean](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Clean_(programming_language) "Clean (programming language)") typeclasses are similar to Haskell, but have a slightly different syntax.

[Rust](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Rust_(programming_language) "Rust (programming language)") supports [traits](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Trait_(computer_programming) "Trait (computer programming)"), which are a limited form of type classes with coherence.[\[20\]](#cite_note-20)

[Mercury](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Mercury_(programming_language) "Mercury (programming language)") has typeclasses, although they are not exactly the same as in Haskell.\[_[further explanation needed](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Wikipedia:Please_clarify "Wikipedia:Please clarify")_\]

In [Scala](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Scala_(programming_language) "Scala (programming language)"), type classes are a [programming idiom](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Programming_idiom "Programming idiom") which can be implemented with existing language features such as implicit parameters, not a separate language feature per se. Because of the way they are implemented in Scala, it is possible to explicitly specify which type class instance to use for a type at a particular place in the code, in case of ambiguity. However, this is not necessarily a benefit as ambiguous type class instances can be error-prone.

The proof assistant [Coq](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Coq_(proof_assistant) "Coq (proof assistant)") has also supported type classes in recent versions. Unlike in ordinary programming languages, in Coq, any laws of a type class (such as the monad laws) that are stated within the type class definition, must be mathematically proved of each type class instance before using them.

See also\[[edit](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Type_class&action=edit&section=8 "Edit section: See also")\]
---------------------------------------------------------------------------------------------------------------------------------------------------

*   [Polymorphism (computer science)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Polymorphism_(computer_science) "Polymorphism (computer science)") (other kinds of polymorphism)
*   [Haskell programming language](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Haskell_(programming_language) "Haskell (programming language)") (the language in which type classes were first designed)
*   [Operator overloading](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Operator_overloading "Operator overloading") (one application of type classes)
*   [Monad (functional programming)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Monad_(functional_programming) "Monad (functional programming)") (`Monad` is an example of a type class)
*   [Concepts (C++)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Concepts_(C%2B%2B) "Concepts (C++)") (since C++20)
*   [Rust (programming language)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Rust_(programming_language) "Rust (programming language)")

References\[[edit](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Type_class&action=edit&section=9 "Edit section: References")\]
-------------------------------------------------------------------------------------------------------------------------------------------------------

1.  **[^](#cite_ref-1 "Jump up")** Morris, John (2013). ["Type Classes and Instance Chains"](https://jgbm.github.io/pubs/morris-dissertation.pdf) (PDF).
2.  **[^](#cite_ref-2 "Jump up")** Wadler, Philip (October 1988). ["How to make ad-hoc polymorphism less ad hoc"](https://www.researchgate.net/publication/2710954).
3.  **[^](#cite_ref-kaes88parametric_3-0 "Jump up")** Kaes, Stefan (March 1988). "Parametric overloading in polymorphic programming languages". _Proc. 2nd European Symposium on Programming Languages_. [doi](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Doi_(identifier) "Doi (identifier)"):[10.1007/3-540-19027-9\_9](https://doi.org/10.1007%2F3-540-19027-9_9).
4.  **[^](#cite_ref-wadler88how_4-0 "Jump up")** Wadler, Philip; Stephen Blott (January 1989). ["How to make ad-hoc polymorphism less ad hoc"](http://homepages.inf.ed.ac.uk/wadler/papers/class/class.ps). _Proc. 16th ACM Symposium on Principles of Programming Languages_.
5.  **[^](#cite_ref-appel91standard_5-0 "Jump up")** Appel, Andrew; David MacQueen (June 1991). ["Standard ML of New Jersey"](http://citeseer.ist.psu.edu/appel91standard.html). _Proc. 3rd International Symposium on Programming Language Implementation and Logic Programming_.
6.  **[^](#cite_ref-6 "Jump up")** [`Type`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Kind.html#t:Type) from `Data.Kind` appeared in version 8 of the [Glasgow Haskell Compiler](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Glasgow_Haskell_Compiler "Glasgow Haskell Compiler")
7.  **[^](#cite_ref-7 "Jump up")** [Haskell'](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Haskell%27 "Haskell'") page _[MultiParamTypeClasses](http://prime.haskell.org/wiki/MultiParamTypeClasses)_.
8.  **[^](#cite_ref-8 "Jump up")** In GHC, the C Core uses Girard & Reynold's System F type signatures to identify a typed case for processing in the optimization phases. -- Simon Peyton-Jones "[Into the Core - Squeezing Haskell into Nine Constructors"](https://www.youtube.com/watch?v=uR_VzYxvbxg) Erlang User Conference, Sep 14, 2016
9.  **[^](#cite_ref-9 "Jump up")** Mark Jones. _[Type Classes with Functional Dependencies](http://web.cecs.pdx.edu/~mpj/pubs/fundeps.html)_. From Proc. 9th European Symposium on Programming. March, 2000.
10.  **[^](#cite_ref-10 "Jump up")** [Haskell'](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Haskell%27 "Haskell'") page _[FunctionalDependencies](http://prime.haskell.org/wiki/FunctionalDependencies)_.
11.  **[^](#cite_ref-11 "Jump up")** [http://www.haskell.org/pipermail/haskell-prime/2006-February/000289.html](http://www.haskell.org/pipermail/haskell-prime/2006-February/000289.html)
12.  **[^](#cite_ref-12 "Jump up")** [Edward Kmett, _Type Classes vs. the World_, Boston Haskell Meetup.](https://www.youtube.com/watch?v=hIZxTQP1ifo)
13.  **[^](#cite_ref-scala-typeclasses_13-0 "Jump up")** Oliveira, Bruno; Adriaan Moors; Martin Odersky (2010). ["Type Classes as Objects and Implicits"](http://ropas.snu.ac.kr/~bruno/papers/TypeClasses.pdf) (PDF). _OOPSLA_.
14.  **[^](#cite_ref-14 "Jump up")** ["The Neophyte's Guide to Scala Part 12: Type classes - Daniel Westheide"](http://danielwestheide.com/blog/2013/02/06/the-neophytes-guide-to-scala-part-12-type-classes.html).
15.  **[^](#cite_ref-15 "Jump up")** [typelevel.org, Scala Cats](http://typelevel.org/cats/typeclasses.html)
16.  **[^](#cite_ref-16 "Jump up")** [A Gentle Introduction to Type Classes and Relations in Coq](http://www.labri.fr/perso/casteran/CoqArt/TypeClassesTut/typeclassestut.pdf)
17.  **[^](#cite_ref-17 "Jump up")** "[Modelling Type Classes With Instance Arguments](http://wiki.portal.chalmers.se/agda/pmwiki.php?n=ReferenceManual.ModellingTypeClassesWithInstanceArguments)".
18.  **[^](#cite_ref-dreyer06modular_18-0 "Jump up")** Dreyer, Derek; Robert Harper; Manuel M.T. Chakravarty (April 2006). ["Modular Type Classes"](http://citeseer.ist.psu.edu/751836.html).
19.  **[^](#cite_ref-19 "Jump up")** ["GHC/Type families - HaskellWiki"](http://www.haskell.org/haskellwiki/GHC/Type_families).
20.  **[^](#cite_ref-20 "Jump up")** ["Specialization, coherence, and API evolution · Aaron Turon"](https://aturon.github.io/blog/2017/02/06/specialization-and-coherence/).

External links\[[edit](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Type_class&action=edit&section=10 "Edit section: External links")\]
----------------------------------------------------------------------------------------------------------------------------------------------------------------

*   A Gentle Introduction to Haskell, Version 98, chapter [5\. Type Classes and Overloading](http://www.haskell.org/tutorial/classes.html). June 2000.
*   Advanced Functional Programming course at Utrecht University, 74 lecture slides on [Advanced Type Classes](http://www.cs.uu.nl/wiki/pub/Afp/CourseSchedule/AFP-2008.3-12.pdf). 2005-06-07.
*   [Implementing, and Understanding Type Classes](http://okmij.org/ftp/Computation/typeclass.html). 2014-11-13.


[Source](https://en.wikipedia.org/wiki/Type_class#Higher-kinded_polymorphism)