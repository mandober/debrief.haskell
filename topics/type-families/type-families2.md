# Type family

https://wiki.haskell.org/GHC/Type_families

## Intro

Indexed type families (or just type families) are a GHC extension supporting ad-hoc overloading of data types.

Type families are parametric types that can be assigned specialized representations based on the type parameters they are instantiated with.

They are the data type analogue of type classes: families are used to define overloaded data in the same way that classes are used to define overloaded functions.

Type families are useful for generic programming, for creating highly parameterised library interfaces, and for creating interfaces with enhanced static information, much like dependent types.

Type families come in two flavors: data families and type synonym families:
- Data families are the indexed form of data and newtype definitions
- Type synonym families are the indexed form of type synonyms

Each of these flavors can be defined:
- standalone
- associated with a type class

Standalone definitions are more general, while associated types can more clearly express how a type is used and lead to better error messages.


## TOC

1. Description of type families
2. Requirements to use type families
3. An associated data type example
  3.1 The class declaration
  3.2 An Int instance
  3.3 A unit instance
  3.4 Product and sum instances
  3.5 Using a generic map
  3.6 Download the code
4. Detailed definition of data families
  4.1 Family declarations
    4.1.1 Associated family declarations
  4.2 Instance declarations
    4.2.1 Associated type instances
    4.2.2 Scoping of class parameters
    4.2.3 Type class instances of family instances
    4.2.4 Overlap
  4.3 Import and export
    4.3.1 Associated families
    4.3.2 Examples
    4.3.3 Instances
5. An associated type synonym example
  5.1 The class declaration
  5.2 An instance
  5.3 Using generic collections
6. Detailed definition of type synonym families
  6.1 Family declarations
    6.1.1 Associated family declarations
  6.2 Type instance declarations
    6.2.1 Closed family simplification
    6.2.2 Associated type instances
    6.2.3 Overlap
    6.2.4 Decidability
  6.3 Equality constraints
7. Frequently asked questions
  7.1 Comparing type families and functional dependencies
  7.2 Injectivity, type inference, and ambiguity
8. References


## Description of type families

An *indexed type family* in type theory is a *partial function at the type level*. Applying the function to parameters, referred to as *type indices*, yields a type.

Type families permit a program to compute what data ctors it will operate on, rather than having them fixed statically, as with simple type systems; or treated as *opaque unknowns*, as with *parametrically polymorphic types*.

Type families are to vanilla data types what type class methods are to regular functions. Vanilla polymorphic data types and functions have a single definition, which is used at all call sites (at all type instances). On the other hand, type classes and type families, have an interface declaration (possibly with a definition) and then they may have any number of instance definitions.

A type family's interface definition declares its *kind and arity*. 
Instance definitions define the type family over some part of the domain.

### Type family vs parametric data types

As a simple example of how type families differ from ordinary parametric data types, consider a strict list type. We can represent a list of Char in the usual way, with cons cells. We can do the same thing to represent a list of (), but since a strict () value carries no useful information, it would be more efficient to just store the length of the list. This can't be done with an ordinary parametric data type, because the data constructors used to represent the list would depend on the list's type parameter: if it's Char then the list consists of cons cells; if it's (), then the list consists of a single integer. We basically want to select between two different data types based on a type parameter. Using type families, this list type could be declared as follows:

```hs
-- Declare a list-like data family
data family XList a

-- Declare a list-like instance for Char
data instance XList Char = XCons !Char !(XList Char) | XNil

-- Declare a number-like instance for ()
data instance XList () = XListUnit !Int
```

The RHSs of the two *data instance declarations* are exactly *ordinary data definitions*. In fact, a `data instance` declaration is nothing more than a shorthand for a `data declaration` followed by a `type instance` declaration.

However, they define two instances of the same parametric data type, `XList Char` and `XList ()`, whereas ordinary data declarations define completely unrelated types. A recent [tutorial paper](https://wiki.haskell.org/Simonpj/Talk:FunWithTypeFuns) has more in-depth examples of programming with type families.

[GADTs](https://wiki.haskell.org/GADT) bear some similarity to type families, in the sense that they allow a parametric type's constructors to depend on the type's parameters. However, all GADT constructors must be defined in one place, whereas type families can be extended.

`Functional dependencies` are similar to type families, and many type classes that use functional dependencies can be equivalently expressed with type families. Type families provide a more functional style of type-level programming than the relational style of functional dependencies.


## Requirements to use type families

Type families are a GHC extension enabled with the `-XTypeFamilies` flag. The first stable release of GHC that properly supports type families is 6.10.1. Release 6.8 includes an early partial deprecated implementation.

## An associated data type example

As an example, consider Ralf Hinze's [*generalised tries*](http://www.cs.ox.ac.uk/ralf.hinze/publications/GGTries.ps.gz), a form of *generic finite maps*.

### The class declaration

We define a type class whose instances are the types that we can use as keys in our generic maps:
