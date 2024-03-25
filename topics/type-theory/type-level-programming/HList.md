# HList - heterogeneous list

We can extend the common Haskell list, which is a homogeneous sequence of elements, in two dimensions:
- `Vec` is a list with statically tracked length, `Vec (n :: Nat) a`
- `HList` is a heterogeneous sequence of elements, each one of distinct type

A heterogeneous list, `HList` is capable of storing data of different types, while still providing operations for look-up, update, iteration, etc.

In fact, `HList` also has the property of static length tracking because each item in a term-level list must have its type in the corresponding type-level list. A `Hlist` has a one-to-one correspondance between the term-level and type-level list elements.

```hs
xs :: '[Int, Bool, Char, String]
xs =   [123, True,  'a', "abcd"]
```

The usual list is a special data type; the compiler has an intimate knowledge about lists, which allows lists to use symbols that are not otherwise allowed:
- the list type ctor is denoted by `[]` (which are two symbols acting as one)
- the empty list data ctor uses the same notation, `[]`
- the cons data ctor is the only one using an allowed symbol, `:`

```hs
-- a possible declaration for the builtin lists:
data [a] = a : [a]
-- or, equivalently:
data []   a = a   :    ([]   a)
-- corresponding declaration:
data List a = a `Cons` (List a)
```

With `DataKinds`, these 3 entities are promoted onelevel up:
- the list type ctor,       `[]`, becomes a new kind        `[]`
- the empty list data ctor, `[]`, becomes a new type ctor, `'[]`
- the cons list data ctor,   `:`, becomes a new type ctor, `':`


and `'(:)`, both uninhabited in the term-level.
