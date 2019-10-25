# Kinds

**Functions** take 
*value parameters* (i.e. values as parameters) 
to produce *values*.

**Type constructors** take 
*type parameters* (i.e. types as parameters) 
to produce *concrete types*. 

Type constructors can be partially applied, e.g. 
`Either String` is a type that takes one type and 
produces a concrete type, like `Either String Int`

Functions can also be partially applied.

We'll see how *types* are applied to *type constructors* in *type declarations*,
just like how *values* are applied to *functions*.

**Values** like `3::Int`, `'c':: Char`, `Int -> Bool`, `a -> a` are *concrete values* and each has its type. Values are elements of a *type* set. But even type sets are elements in a *kind* set.

value ∈ type
  * a value is an element of a set (that set is called type)
  - a value belongs to a type set: `True ∈ Bool`
  * a type is a set containing certain values
  - a type set contains values: `Bool = {True, False}`
  * some types are subsets of other types:
  - `Bool ⊆ Int ⊆ Integer`
  * There are many concrete types
  - ctypes = {Bool, Int, Integer, Char, (,), [], Int -> Bool, a -> a}

type ∈ kind
  * types are also elements belonging to a set called *kind*
  - e.g. `* ∈ kind`


A **kind** is like a type of a type.

A `*` means that the type is a concrete type. A concrete type is a type that doesn't take any type parameters and values can only have types that are concrete types.

```hs
ghci> :k Int
Int :: *

-- Maybe is a type ctor expecting a type param:
ghci> :k Maybe
Maybe :: * -> *

-- when TP is applied:
ghci> :k Maybe Int
Maybe Int :: *

-- that's similar to a fn:
ghci> :t isUpper
isUpper :: Char -> Bool

-- but when the value is applied:
ghci> :t isUpper 'A'
isUpper 'c' :: Bool
```

`Maybe` type constructor takes one concrete type (e.g. `Int`) and then returns a concrete type, e.g. `Maybe Int`.

Just like `Int -> Int` means that a function takes an Int and returns an Int, 
alike `* -> *` means that the type constructor takes one concrete type and returns a concrete type.

When the type parameter is applied to Maybe the kind of that type is `*` again.


The kind of `Either` is `* -> * -> *`

```hs
ghci> :k Either
Either :: * -> * -> *
```

This tells us that `Either` takes 2 concrete types as type parameters to produce a concrete type.

It looks like a sog of a function that takes 2 values and returns one.


**Partially applying type constructors**   
Type constructors are curried (the same as functions), so we can partially apply them:

```hs
ghci> :k Either String
Either String :: * -> *

ghci> :k Either String Int
Either String Int :: *
```

Only types are eligable to be queried about their kind:
 
```hs
ghci> :k Bool
Bool :: *

ghci> :k Eq
Eq :: * -> Constraint

ghci> :k Functor 
Functor :: (* -> *) -> Constraint

ghci> :k Monad
Monad :: (* -> *) -> Constraint
```
