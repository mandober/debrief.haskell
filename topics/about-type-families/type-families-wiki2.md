# Type families

https://wiki.haskell.org/GHC/Type_families

- lang ext: `TypeFamilies`, since GHC 6.10

**Indexed type families** 
(or just type families) 
are a GHC extension supporting 
ad-hoc overloading of data types.

Type families are parametric types 
that can be assigned 
specialized representations 
based on the type parameters 
they are instantiated with.

Type families are the data type 
analogue of type classes - 
type families are used 
to define overloaded data types 
similarly to how type classes 
are used to define overloaded functions.

Type families come in two flavors:
- data families, using the `data` keyword
- type synonym families, using the `type` keyword

Data families are the indexed form of `data` and `newtype` definitions.   
Type synonym families are the indexed form of `type` synonyms.

Both data families and type synonym families can be located
- *standalone*: defined at the top-level
- *associated*: defined inside a type class

Standalone definitions are more general, while associated ones can more clearly express how a type is used and lead to better error messages.

Type families are useful for generic programming, for creating highly parameterised library interfaces and interfaces with enhanced static information, much like dependent types.

## Contents

* Type families description
* An associated data type example
  - The class declaration
  - An Int instance
  - A unit instance
  - Product and sum instances
  - Using a generic map
  - Download the code
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



## Type families description

The concept of a type family comes from type theory.

An indexed type family in type theory 
is a partial function at the type level.

Applying the function to parameters 
called *type indices*, yields a type.

Type families permit a program to compute 
what data constructors it will operate on, 
rather than having them fixed statically (as with simple type systems) 
or treated as opaque unknowns (as with parametrically polymorphic types).

Type families are to vanilla data types 
what type class methods are to regular functions.

Vanilla polymorphic data types and functions 
have a single definition, 
which is used at all type instances.

Classes and type families, on the other hand, 
have an interface definition 
and any number of instance definitions.

A type family's *interface definition* 
declares its *kind* and its *arity* 
(the number of type indices it takes).

*Instance definitions* 
define the type family 
over some part of the domain.


As a simple example of how type families differ from ordinary parametric data types, consider a strict list type. We can represent a [Char] in the usual way, with cons cells. We can do the same thing to represent a [()], but since a unit carries no useful info, it'd be more efficient to just store the list length.

This can't be done with an ordinary parametric data type because the data ctors used to represent the list would depend on the list's type parameter: if it's `Char` then the list consists of cons cells, if `()` then the list is a single integer.

We basically want to select 
between two different data types 
based on a type parameter.

Using type families, this example list type could be declared as follows:

```hs
-- Declare a list-like data family
data family XList a

-- Declare a strict, list-represented, instance for Char type
data instance XList Char = XNil | XCons !Char !(XList Char)

-- Declare a strict, int-represented, instance for () type
data instance XList () = XListUnit !Int
```

The RHSs of the two data instance declarations are exactly ordinary data definitions. In fact, a data instance declaration is nothing more than a shorthand for a data declaration followed by a type instance declaration. 
However, they define two instances of the same parametric data type, `XList Char` and `XList ()`, whereas ordinary data declarations define completely unrelated types.

This tutorial paper has more in-depth examples of programming with type families: [Fun With Type Function by SPJ](https://wiki.haskell.org/Simonpj/Talk:FunWithTypeFuns)

[GADTs](https://wiki.haskell.org/GADT) bear some similarity to type families, in the sense that they allow a parametric type ctors to depend on the type parameters. However, all GADT ctors must be defined in one place, whereas type families can be extended.

Functional dependencies are similar to type families, and many type classes that use functional dependencies can be equivalently expressed with type families. Type families provide a more functional style of type-level programming than the relational style of functional dependencies.


## An associated data type example

As an example, consider Ralf Hinze's generalised tries, a form of generic finite maps.

### The class declaration

We define a type class whose instances are the types that we can use as keys in our generic maps:

```hs
class GMapKey k where
  data GMap   k :: * -> *
  empty         :: GMap k v
  lookup        :: k -> GMap k v -> Maybe v
  insert        :: k -> v -> GMap k v -> GMap k v
```

The associated data family declaration of the class gives a kind signature for the associated data type `GMap k`, analogous to how methods receive a type signature in a class declaration.

It is important that the first parameter of the associated type `GMap` coincides with the class parameter of `GMapKey`. This also indicates that in all instances of the class, the instances of the associated data type need to have their first argument match up with the instance type.

In general, the type args of an associated type must form a *subset of the class parameters* (in a multi-parameter type class), they can appear in a different order than in the class head, which can be useful if the associated data type is partially applied in some contexts.

The second important point is that as `GMap k` has kind `* -> *` and `k` (implicitly) has kind `*`, the type constructor `GMap` (without an arg) has kind `* -> * -> *`. Consequently, we see that `GMap` is applied to two arguments in the signatures of the methods empty, lookup, and insert.

### An Int instance

To use ints as keys into generic maps, we declare an instance that uses `Data.IntMap`.

```hs
instance GMapKey Int where
  data GMap Int v        = GMapInt (Data.IntMap.IntMap v)
  empty                  = GMapInt  Data.IntMap.empty
  lookup k   (GMapInt m) =          Data.IntMap.lookup k m
  insert k v (GMapInt m) = GMapInt (Data.IntMap.insert k v m)
```

The `Int` instance of the associated data type `GMap` needs to have both of its parameters, but as only the first one corresponds to a class parameter, the second needs to be a type variable (here `v`). As mentioned before, any associated type parameter that corresponds to a class parameter must be identical to the class parameter in each instance. The right hand side of the associated data declaration is like that of any other data type.
