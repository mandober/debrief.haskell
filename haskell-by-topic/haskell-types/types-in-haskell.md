# Types in Haskell

**Types** classify values. In Haskell, terms (values) are classified into types, and types are classified into **kinds**. Values live at the term-level, types and kinds live at the type-level. The axiom `TypeInType` has made types and kinds practically the same. Now we say that types are classified by other types, but we still call them kinds.

**Values** are classified by types. Each value belongs to exactly one type. Some types are empty, so they classify no values - they are called **uninhabited type**. `Void` is the Haskell's *canonical* empty type.

A canonical type is the chosen representative - although there are other empty types, they are all *isomorphic*; i.e. they are virtually the same, for all intents and purposes; as far as the other types are concerned, they couldn't tell them apart. **Isomorphism** or **equivalence** is a relation that is not the same as equality - considering the totality of aspects - but it is, if only some aspects (important for a particular situation) are considered. For example, two books may be different, all things concerned (e.g. different format, release, medium, even different ISBN), even though they are the same in terms of the (main) content.

An **inhabited type** classifies at least one value.
Inhabited types have kind `*`. 
Types with kind `*` are inhabited and they classify values. 

>`Void` has kind `*` (?) even though it is empty.

This is really strange cos it was always said that only `*` types classify values.

So `Int`, [Int], `Maybe Int`, `Either Int Int` all have kind `*`.

Haskell has *first-class functions*, meaning functions are also values.
>Function types classify function values (lambdas).
For example, 
- the function type `Bool -> ()` is inhabited 
by a single value, which is a function `\x -> ()`
- the function type `Int -> String` is inhabited by many values, all which are functions that take an `Int` and return a `String`
- the function type `forall a -> a` is inhabited by a single many values, all which are functions that take an `Int` and return a `String`
