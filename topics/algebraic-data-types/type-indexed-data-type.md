# Type-indexed data type

A *polytypic function* is a function that can be instantiated at many data types to obtain specific functionality (for the type it is instantiated at). Examples of such functions are `show`, `read`, `==`, all methods of the basic classes that GHC can derive automatically for any new data type.

More advanced examples are functions for digital searching, pattern matching, unification, rewriting, and structure editing. For each of these problems, we not only have to define the polytypic functionality, but also a data type indexed by another type.

More commonly, a data type is *parameterized* by another type represented by a type variable. 

## Parameterized data types

A **parameterized data type** is a type (i.e. a type ctor) parameterized by a type (i.e. by another data type), which is (usually) inhabited, and thus has the kind `Type`.

For example, the type `Maybe` is parameterized by a single type, represented by a type variable `a`. The type variable ranges over all inhabited types, so its kind is `Type`, making the `Maybe` type ctor be of the kind `Type -> Type`. This means that only when the `Maybe` type ctor is given an inhabited type (e.g. `Int`, `Int -> Bool`, `Either Exception String`, etc.) it produces a valid data type. Therefore, it cannot be given an unsaturated type with the kind like `Type -> Type`.

```hs
type Maybe :: Type -> Type
data Maybe a = Nothing | Just a
```

## Indexed data type

A type-indexed data type is a data type that is constructed in a generic way from an argument data type. For example, in the case of digital searching, we have to define a search tree type by induction on the structure of the type of search keys.

An indexed data type is pretty much the same as a parameterized data type, with the difference being that the type used as the index is ordered in some way. For example, the type of the natural numbers is often used as the type-level indexing type - the well-known instance of which is the `Vec Nat a` data type.

The `Vec Nat a` is data type indexed by the `Nat` type and parameterized by any inhabited type (of kind `Type`). The `Nat` type used as the index need not be inhabited (it's probably not even possible).

`Vec` is usually defined as an augmented list, i.e. it is based on the list type augmented with the `Nat` indexing so it track its length. The list type ctor is parameterized by an inhabited type, so it specializes at any saturated type like `[Int]`, `[Int -> Bool]`, `[[String] -> Map Char String]`.

On the other hand, `Vec Nat a`, is indexed by an uninhabited type `Nat` but also parameterized by an inhabited type, `a`. Thus, it stands for a whole family of types - if we fix `a` as `Int`, we get `Vec Nat Int` that may be realized as a countable infinity of types (because naturals are countably infinite). There is a type `Vec Z Int`, `Vec (S Z) Int`, `Vec (S (S Z)) Int`, etc.
