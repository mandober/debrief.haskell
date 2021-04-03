# Type Family

There are 3 hot topics in the Haskell community which apparently addressed the same kind of problems: *GADTs*, *TypeFamilies* and *Functional Dependencies*.

## Refs

* GHC/Type families
https://wiki.haskell.org/GHC/Type_families

* GHC Users Guide: Type Family
https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#type-families

* Let generalisation in GHC 7.0
https://www.haskell.org/ghc/blog/20100930-LetGeneralisationInGhc7.html

* Simonpj/Talk: Fun With Type Fun with Type Functions
https://wiki.haskell.org/Simonpj/Talk:FunWithTypeFuns
- [Fun With Type Functions (version 3)](http://research.microsoft.com/~simonpj/papers/assoc-types/fun-with-type-funs/typefun.pdf)
- [Slides (PDF)](http://research.microsoft.com/~simonpj/papers/assoc-types/fun-with-type-funs/FunWithTypeFuns-Apr09.pdf)
- [Source code](http://research.microsoft.com/~simonpj/papers/assoc-types/fun-with-type-funs.zip)

* An Introduction to Type Families
http://www.mchaver.com/posts/2017-06-21-type-families.html

* Haskell's Type Families (slideshow)
https://cdepillabout.github.io/haskell-type-families-presentation/

* Why Haskell V: Type Families
https://mmhaskell.com/blog/2019/2/4/why-haskell-v-type-families

* type family vs data family, in brief?
https://stackoverflow.com/questions/20870432/type-family-vs-data-family-in-brief

* Type Families and Pokemon
https://www.schoolofhaskell.com/user/nubis/type-families-and-pokemon


## GHC amd Type families

https://wiki.haskell.org/GHC/Type_families

* Enabled with the language pragma `TypeFamilies`

* *Indexed type families* (or type families for short) are a Haskell extension supporting *ad-hoc overloading of data types*.

* Type families are parametric types that can be assigned specialized representations based on the type parameters they are instantiated with.

* They are the data type analogue of type classes: families are used to define overloaded data in the same way that classes are used to define overloaded functions.

* Type families are useful for generic programming, for creating highly parameterised library interfaces, and for creating interfaces with enhanced static information, much like dependent types.

* Type families come in two flavors: data families and type synonym families. *Data families* are the indexed form of data and newtype definitions. *Type synonym families* are the indexed form of type synonyms.

* Each of these flavors can be defined in a standalone manner or associated with a type class. Standalone definitions are more general, while associated types can more clearly express how a type is used and lead to better error messages.


* About type families
* An associated data type example
  - The class declaration
  - An Int instance
  - A unit instance
  - Product and sum instances
  - Using a generic map
* Detailed definition of data families
  - Family declarations
    - Associated family declarations
  - Instance declarations
    - Associated type instances
    - Scoping of class parameters
    - Type class instances of family instances
    - Overlap
  - Import and export
    - Associated families
    - Examples
    - Instances
* An associated type synonym example
  - The class declaration
  - An instance
  - Using generic collections
* Detailed definition of type synonym families
  - Family declarations
    - Associated family declarations
  - Type instance declarations
    - Closed family simplification
    - Associated type instances
    - Overlap
    - Decidability
  - Equality constraints
* Frequently asked questions
  - Comparing type families and functional dependencies
  - Injectivity, type inference, and ambiguity


## About type families

The concept of a type family comes from type theory. An *indexed type family* in type theory is a partial function at the type level. Applying the function to parameters, called *type indices*, yields a type. Type families permit a program to compute what data ctors it will operate on, rather than having them fixed statically (as with simple type systems) or treated as opaque unknowns (as with parametrically polymorphic types).

Type families are to vanilla data types what type class methods are to regular functions.

Vanilla polymorphic data types and functions have a single definition, which is used at all type instances. Classes and type families, on the other hand, have an interface definition and any number of instance definitions.

A type family's interface definition declares its kind and its arity, or the number of type indices it takes. Instance definitions define the type family over some part of the domain.

As an example of how type families differ from ordinary parametric data types, consider a strict list type. We can represent a list of Char in the usual way, with cons cells. We can do the same thing to represent a list of (), but since a strict () value carries no useful information, it would be more efficient to just store the length of the list.

This can't be done with an ordinary parametric data type, because the data constructors used to represent the list would depend on the list's type param:
- if it's Char then the list consists of cons cells
- if it's (), then the list consists of a single integer.

We basically want to select between two different data types based on a type parameter. Using type families, this list type could be declared as follows:

```hs
-- Declare a list-like data family
data family XList a

-- Declare a list-like instance for Char
data instance XList Char = XNil | XCons !Char !(XList Char)

-- Declare a number-like instance for ()
data instance XList () = XListUnit !Int
```

The right-hand sides of the two *data instance declarations* are exactly ordinary data definitions. In fact, a data instance declaration is nothing more than a shorthand for a data declaration followed by a type instance declaration.

However, they define two instances of the same parametric data type, `XList Char` and `XList ()`, whereas ordinary data declarations define completely unrelated types.

*GADTs* bear some similarity to type families, in the sense that they allow a parametric type's constructors to depend on the type's parameters. However, all GADT ctors must be defined in one place, whereas type families can be extended.

*Functional dependencies* are similar to type families, and many type classes that use functional dependencies can be equivalently expressed with type families. Type families provide a more functional style of type-level programming than the *relational style of functional dependencies*.

Type families are a GHC extension enabled with the `-XTypeFamilies` flag or the `{-# LANGUAGE TypeFamilies #-}` pragma. The first stable release of GHC that properly supports type families is 6.10.1.


## An associated data type example

As an example, we consider Ralf Hinze's generalised tries, a form of generic finite maps.

### The class declaration

We define a type class whose instances are the types that we can use as keys in our generic maps:

```hs
class GMapKey k where
    data GMap k :: * -> *
    empty       :: GMap k v
    lookup      :: k -> GMap k v -> Maybe v
    insert      :: k -> v -> GMap k v -> GMap k v
```

The interesting part is the *associated data family* declaration of the class. It gives a kind signature (* -> *) for the associated data type `GMap k`,analogous to how methods receive a type signature in a class declaration.

It is important to notice that the first parameter of the associated type `GMap` coincides with the class parameter of `GMapKey` (both are `k`).

This indicates that in all instances of the class, the instances of the associated data type need to have their first argument match up with the instance type.

In general, type arguments of an associated type can be a subset of the class parameters (in a multi-parameter type class) and they can appear in any order, possibly, in an order different from the one in the class head.

The free ordering can be useful if the associated data type is partially applied in some contexts.

The second important point is that since `GMap k` has kind `* -> *` so `k` (implicitly) has kind `*`, the type constructor `GMap` (without an argument) has kind `* -> * -> *`.

GMap     :: * -> * -> *
GMap k   :: * -> *
GMap k v :: *

Consequently, we see that `GMap` is applied to two arguments in the signatures of the methods empty, lookup, and insert.

### An Int instance

To use Ints as keys into generic maps, we declare an instance that simply uses `Data.IntMap`.

```hs
instance GMapKey Int where
    data GMap Int v        = GMapInt (Data.IntMap.IntMap v)
    empty                  = GMapInt Data.IntMap.empty
    lookup k   (GMapInt m) = Data.IntMap.lookup k m
    insert k v (GMapInt m) = GMapInt (Data.IntMap.insert k v m)
```

The `Int` instance of the associated data type GMap needs to have both of its parameters, but as only the first one corresponds to a class parameter, the second needs to be a type variable (here, `v`).

As mentioned before, any associated type parameter that corresponds to a class parameter must be identical to the class parameter in each instance. The right hand side of the associated data declaration is like that of any other data type.

NB: At the moment, GADT syntax is not allowed for associated data types (or other indexed types). This is not a fundamental limitation, but just a shortcoming of the current implementation, which we expect to lift in the future.

As an exercise, implement an instance for Char that maps back to the Int instance using the conversion functions Char.ord and Char.chr.


### A unit instance



### Product and sum instances



### Using a generic map


## Detailed definition of data families

Data families appear in two flavours:
1. *toplevel form*: defined at the toplevel
2. *classlevel form*: in type classes, where they're called *associated types*

The toplevel form is the more general variant, as it lacks the requirement for the type-indices to coincide with the class parameters.

The class-level form can lead to a better structured code and clearer compiler warnings, if some type instances were, possibly accidentally, omitted.

In the following, we always discuss the general toplevel form first and then cover the additional constraints placed on associated types.

### Family declarations

Indexed data families are introduced by a signature, such as

```hs
data family GMap k :: * -> *
```

The special `family` keyword distinguishes a family from regular data declaration. The result kind annotation is optional and, as usual, defaults to a `*` if omitted. For example:

```hs
data family Array e
```

Named arguments can also be given explicit kind signatures if needed. Just as with GADT declarations named arguments are entirely optional, so that we can declare `Array` alternatively with

```hs
data family Array :: * -> *
```
