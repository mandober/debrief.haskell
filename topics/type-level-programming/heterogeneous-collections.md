# Heterogeneous collections

A homogeneous collection (lists, sets, maps, etc.) stores items of the same type. These are parametrically polymorphic collections - each accespts any type it might be instantiated with. For exampler, the ordinary Haskell list, `[a]`, can store terms of any type `a`; the same function `map` is used to map a list, whether it is a list of `[Int]`, `[Maybe a]`, `[a -> b]`. Also, the type of a list is the same, whether is contains no elements or an infinite number of elements.

However, homogeneous collections cannot meet the scenarios that require storing values of different types. These situations include
- a symbol table must store entries of different types; it is a finite map, where the result type depends on the argument value
- an XML element is heterogeneously typed; XML elements are nested collections that are constrained by regular expressions and the 1-ambiguity property.
- each row returned by an SQL query is a heterogeneous map from column names to cells; the result of a query is a homogeneous stream of heterogeneous rows.
- adding an object system to a FPL requires heterogeneous collections of a kind that combines extensible records with subtyping and an enumeration interface.

A heterogeneous collection is a datatype that is capable of storing data of different types, while providing operations for look-up, update, iteration, etc.

Considering the omnipresent list type, which is a homogeneous collection of elements (term-level values), we can extend it in two dimensions:
- `Vec` is a list with statically tracked length
- `HList` is a heterogeneous sequence of elements, each of a distinct type

The type that combines these two is unnecessary since `HList` already tracks the length implicitly: each item in the list has a corresponding type; that is, there is a list of elements at the term-level, accompanied with a type-level list of corresponding types. Even if all the elements in a `HList` have the same type, still, each entry will have its corresponding type in the type-level list.

term-level   | type-level                     | kind-level
-------------|--------------------------------|------------------------------
∅            | []                             | `[] :: Type -> Type`
[]           | [a]                            | Type
∅            | Vec                            | `Vec :: Nat -> Type -> Type`
[1,2,3]      | [Int]                          | Type
[]           | Vec 0 a                        | Type
[1,2,3]      | Vec 3 Int                      | Type
∅            | HList                          | `HList :: [Type] -> Type`
[]           | HList '[]                      | Type
[True,1,'c'] | HList '[Bool,Int,Char]         | Type
[True,1]     | HList (Bool ': Int ': '[])     | Type
