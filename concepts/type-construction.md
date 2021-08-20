# Type construction

Unlike most PLs, Haskell offers the so-called algebraic data types, which, at least in the begining of the Haskell learning, can be considered the means of constructing a new data type or data structure.

A data type is any type, from a scalar base type (Int, Double, Char), to any compound type. Actually, compound types are very similar to a data structure since both consists of a combination of multiple types, often with various levels of inner type nesting, with one "topmost" type that represents the entire compound type or data structure. Briefly, the line between data structures and data types (or, sometimes even type structures) is very blurry. For example, `Maybe a` is commonly considered a type, while `[a]` is a considered a (list) data structure, but both are constructed using sum type construction.

Unlike product type construction, which is available in almost every PL, sum type construction is inexplicably rarely encountered in mainstream PLs.

Perhaps, an "archi sum type" was `union` from `C`, but it wasn't the proper sum type, aka *tagged disjoint union*; it was more similar to a `struct` type (which is a product type) with the additional remark while a union type represents a unoin of different types, at any time, only a single type among them is contained within that space, but which one - that was never obvious like it should be. Instead, people have associated a "tag" with a unoin, such that it identified union's contents, but had to be manually synced with the ever-changing contents of the union.

After "C", it seems PLs have forgotten about the union type, solving everything with product types (structs, records, objects). This is very strange since bith means of type construction go hand in hand like conjunction and disjunction in logic (and all these PLs has AND and OR Boolean operators). In fact, product types repr conjunction of types, they are "AND" types, so all values of the constituent types must be specified in order to construct the type. Sum types repr the exclusive disjunction of types, they are the "XOR" types, so only a single value of one particular type (out of several constituent types) must be specified to construct the type.

If everyone knows the benefits of product types, it is very strange people did so long without recognizing the benefits of sum types. Actually, the problems that would be best solved using sum types, were solved using some very ackward approaches. Hence the famous universal (it inhabits all types) `null` value; or, signaling the failure by using an in-band value, e.g. using 0 or -1 (that is a valid member of the domain) to repreent some out of domain consition like repr an EOF marker or exception. You can't reason about such attrocities.

Most of all, sum types allow for clean repre of a unoin of types, like when you write an evaluator function which has to accept expressions that have diff types, or if a function needs to, normally, return one type, but under some special condition, another type. These things are best described with a *disjoint tagged union*. The tag indicates the type of the actual value. Functions that accept sum types must do the "case analysis" so every potential type of value is handled, which is very elegantly handled by pattern matching.

## Sum types

A declaration of the new begins with a keyword (data, newtype) followed by a name, which is a type ctor, and as such it may accept none or more type arguments. These type arg are, most generally, type parameters standing for a all or broad range os types; concrete types (Int, Char) do not go here, on the LHS (which is the side for declaration) since they need not be declared (again).


Anyway, it is best to consider data ctors of a sum type, as constructor functions, which take a value and return a particular variant value. `Either e a` is a general sum type for expressing disjuntion (dijoint union) of two types, but e.g. `IntXorChar` may express it for conrete types


```hs
-- in general
data Either e a = Left e | Right a

-- union of two concrete types - this def is redundant since
data IntXorChar = Int Int | Char Char

-- it may have been defined in terms of Either as:
type IntXorChar = Either Int Char
-- and it would behave exactly the same.
```

Mentioning a data ctor alone, `Left`, is meaningless. Left is a function that produces values of its union type.

```hs
Left  :: a -> Either a b
Right :: b -> Either a b

Just :: a -> Maybe a
Nothing  ::  Maybe a
```

It is interesting that data ctors are functions that need not be given a concrete implementation - they don't require or need you to write out their definition; although these definitions are nothing more than applications, this is done automatically.

```hs
-- if the implementation was allowed to be given manually it'd look like:
Left :: a -> Either a b
Left x = Left x
-- the same as the point-free form:
Left = Left

(:) :: a -> [a] -> [a]
(:) x xs = (:) x xs
-- the same as:
(:) x = (:) x
-- the same as the point-free form:
(:) = (:)
```
