# Polymorphism

In PLT, the term "polymorphism", derived from the ancient Greek, means "many forms (shapes)", and the forms in question are those of values - all values belonging to a certain type, are said to have a certain shape; that is, the types are the molds that shape the language expressions thereby giving them a particular form. A polymorphic entity is a language entity with shape-shifting capabilities. A polymorphic type is a type whose operations are applicable to values of more than one type. A polymorphic value is a value of a polymorphic type.

Although other PL constructs may exhibit polymorphic behaviour, polymorphism is primarily a property of functions. Polymorphic functions can work with more than one type, as opposed to monomorphic functions that can only ever handle one single type (which is usually imposed on them by the capabilities of a PL, or the lack there of).


A *type variable* or *type parameter* is a language entity whose purpose is to represent some (unknown) type. In Haskell, type variables are distinguished by begining with a small letter, unlike the types (type ctors) that begin with a capital letter; type variables are usually dented just by single-letter names, unlike types (type ctors) that have longer, more descriptive, names.

Type variables are analogous to term-level variables: term variables stand for an unknown and arbitrarily complex *expression*, while a type variable stands for an unknown and arbitrarily complex *type expression* (not necessarily just a single type).

Note that, a type variable is often said to stand for a type, and that is true because a type expression eventually represents a type; but that takes work,meaning it has to be evaluated in order to be reduced to a single type. So, more generally, a type variable stands for a type expression, not just for a single type. It's the same with variables at the term-level that don't just stand for a single value but for arbitrarily complex expressions.
