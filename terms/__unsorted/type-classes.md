# Type classes

In PLT, *overloading* is used to describe the ability of a single symbol to have different interpretations as determined by the context in which it appears.

A standard example of overloading is the use of the plus operator to represent addition on the natural numbers, on the integers, on the real numbers, etc., rather than having to come up with a distinct name every time the addition operation is defined on some new type. In PL without overloading, one commonly follows a naming scheme, perhaps, as a compound name consisting of some variation of the operation name and type name, e.g. `addNat`, `addInt`, `addReal`, etc. But with overloading, there can be just a single name (alphabetic or even symbolic) that represent addition for any type that supports it. Then the intended meaning of the overloaded identifier can be determined from the types of the arguments to which it is applied.

Ad hoc polymorphism (overloading) consolidates all the distinct names for a similar concept under one umbrella term. Parametric polymorphism consolidates all the distinct implementations that are practically the same.

Ad hoc polymorphism reduces the number of names, parametric polymorphism reduces the number of implementations.

One common approach is to completely resolve overloading at compile time. The compiler installs type specific meanings for all overloaded symbols, based either on type information attached to operands (type dispatch) or some more general overloading resolution mechanism. A significant drawback to this approach is that overloaded operations cannot be abstracted while retaining their overloaded nature.

A more dynamic approach to overloading which preserves the ability to abstract overloaded definitions is found in OOP where the resolution of overloaded operations occurs at run time.

An alternative approach to the treatment of overloading was introduced by Wadler and Blott based on the notion of a *type class* and is intended to provide a uniform and general framework for solving exactly these kinds of problems.

Summarizing the main features of a system of type classes for a very simple and well known example - the definition of the equality operator, `==`, that is:
- polymorphic: the operator is not restricted to values of any single type
- overloaded: the interpretation of equality is determined by its arg types
- extensible: the definition of equality can be extended to include new types

The class system terminology:
- *type class*: a group of related methods is gathered under a common name. A class has some aspects similar to interfaces. Each class is distinguished by a unique name (up to the scope of the loaded modules), which is used in the type language. A class' name is also used as a constraint in functions and other desinitions.
- *method*: the overloaded name (the name of the function or operator, such as `==`) is a method. Methods are used in the term language.
- *data type*: is a builtin or a custom type declaration consisting of type ctor and one or more data ctors (a potentially recursive sum of products)
- *type constructor*: names a data type in the type language; a data constructor creates values in the expression language. Type ctors are the targets of class implementations, that is, a class' type variable is meant to refer to a, partially or completely saturated, type ctor.
- *instance*: an instance binds a data type to operations which implement the methods of a specified class for that type.
- superclass
- constraint
- flexible instance
- flexible contexts
- undecidable instances

The types which are members of a class are dened by a
collection of instance declarations which may be distributed
throughout the program, typically in dierent program modules where new datatypes are introduced

The expression Eq a => Eq [a] in the rst line
indicates that the denition of equality on lists depends on
the denition of equality used for the elements held in the
list: if a is an instance of Eq, then so is [a].

The set of types dened by a nite collection of instance
declarations may be innite (but recursively enumerable).
