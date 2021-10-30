# Parametric Polymorphism

> A powerful expressive form that allows generic code to be written that applies to a range of types. An example is collection classes like Vector which are independent of the types of their constituent members. Parametric polymorphism allows classes and methods to accept types as parameters, in addition to their conventional parameters, meaning that classes can be abstracted with respect to types.

A powerful expressive form that allows generic code to be written that applies to a range of types. An example is collection classes like Vector which are independent of the types of their constituent members. Parametric polymorphism allows classes and methods to accept types as parameters, in addition to their conventional parameters, meaning that classes can be abstracted with respect to types.

(from [http://www.cogs.susx.ac.uk/users/timothyo/research/direction/node11.html](http://www.cogs.susx.ac.uk/users/timothyo/research/direction/node11.html)) [BrokenLink](http://wiki.c2.com/?BrokenLink)

\-- [ChanningWalton](http://wiki.c2.com/?ChanningWalton)

* * *

[ParametricPolymorphism](http://wiki.c2.com/?ParametricPolymorphism) is usually contrasted with [AdHocPolymorphism](http://wiki.c2.com/?AdHocPolymorphism). Quoting [ChristopherStrachey](http://wiki.c2.com/?ChristopherStrachey):

*   "Parametric polymorphism is obtained when a function works uniformly on a range of types; these types normally exhibit some common structure. Ad-hoc polymorphism is obtained when a function works, or appears to work, on several different types (which may not exhibit a common structure) and may behave in unrelated ways for each type."

Also see [OnUnderstandingTypes](http://wiki.c2.com/?OnUnderstandingTypes).

* * *

It should probably be pointed out that [ParametricPolymorphism](http://wiki.c2.com/?ParametricPolymorphism) as such is completely unrelated to [ObjectOrientedProgramming](http://wiki.c2.com/?ObjectOrientedProgramming) (unlike the suggestion created by the link above). [ParametricPolymorphism](http://wiki.c2.com/?ParametricPolymorphism) is a typical feature of all statically-typed [FunctionalProgrammingLanguage](http://wiki.c2.com/?FunctionalProgrammingLanguage)s, such as [HaskellLanguage](http://wiki.c2.com/?HaskellLanguage) or [MlLanguage](http://wiki.c2.com/?MlLanguage). C++'s templates provide similar functionality, but unfortunately lack nice syntax, separate compilation and full type-safety at definition time. -- [StephanHouben](http://wiki.c2.com/?StephanHouben)

* * *

[ParametricPolymorphism](http://wiki.c2.com/?ParametricPolymorphism) comes [ForFree](http://wiki.c2.com/?ForFree) in any language with first class types, but for reasons I don't understand, almost no languages have such. -- [ThomasColthurst](http://wiki.c2.com/?ThomasColthurst)

* * *

A lot of people in the [LanguagePissingMatch](http://wiki.c2.com/?LanguagePissingMatch) have pointed out that Java doesn't have [ParametricPolymorphism](http://wiki.c2.com/?ParametricPolymorphism). They're actually working on this as we speak. Go to the java language specification at [http://java.sun.com/](http://java.sun.com/) (it's in there somewhere) and there should be something about planned updates. It includes assertions and [ParametricPolymorphism](http://wiki.c2.com/?ParametricPolymorphism).

_The [JavaCommunityProcess](http://wiki.c2.com/?JavaCommunityProcess) list of all specification requests does not list [ParametricPolymorphism](http://wiki.c2.com/?ParametricPolymorphism). The closest it comes is support for generic types. Can you provide a direct URL? I suspect that [ParametricPolymorphism](http://wiki.c2.com/?ParametricPolymorphism) won't make it into any JVM since it requires changes to the JLS, perhaps to bytecodes, and may not be backwards compatible._

* * *

Surely the "Generics" feature of Java 1.5 provides [ParametricPolymorphism](http://wiki.c2.com/?ParametricPolymorphism), while [CeePlusPlus](http://wiki.c2.com/?CeePlusPlus)'s "Templates" provides [AdHocPolymorphism](http://wiki.c2.com/?AdHocPolymorphism)? Or have I misunderstood the definitions of [ParametricPolymorphism](http://wiki.c2.com/?ParametricPolymorphism) provided on this page? -- [ChrisHandley](http://wiki.c2.com/?ChrisHandley)

*   No, C++ templates are parameteric polymorphism; actually they can provide both. Language features in support of [AdHocPolymorphism](http://wiki.c2.com/?AdHocPolymorphism) include:
    *   Overloading (in particular, selection of a method based on the static type of a reference; as opposed to true [MultipleDispatch](http://wiki.c2.com/?MultipleDispatch)).
    *   Template specialization (though this has many useful uses in C++ besides [AdHocPolymorphism](http://wiki.c2.com/?AdHocPolymorphism))
    *   Type coercion.

All of which C++ can do. But templates themselves are a technique for [ParametricPolymorphism](http://wiki.c2.com/?ParametricPolymorphism). Read the paper [OnUnderstandingTypes](http://wiki.c2.com/?OnUnderstandingTypes); it explains all of the above quite well.

* * *

See also: [IncludeFileParametricPolymorphism](http://wiki.c2.com/?IncludeFileParametricPolymorphism), [PredicateTypes](http://wiki.c2.com/?PredicateTypes), [PredicateDispatching](http://wiki.c2.com/?PredicateDispatching)

* * *

[CategoryPolymorphism](http://wiki.c2.com/?CategoryPolymorphism)

* * *

Last edit January 2, 2013, See [github](https://github.com/WardCunningham/remodeling) about remodeling.


[Source](http://wiki.c2.com/?ParametricPolymorphism)