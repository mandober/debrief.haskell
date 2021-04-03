# Higher-Kinded Type

A **higher-kinded type** (HKT) is a type-ctor that has type variables. Haskell supports higher-kinded types, which are types that take other types and construct a new type.

- A higher-kinded type is a type that abstracts over a type that, in turn, abstracts over another type. It's a way to generically abstract over entities that take type-ctors.

Functions: domains and codomains
- from terms to terms: regular functions
- from types to terms: parametric polymorphism
- from types to types: higher-kinded types
- from terms to types: dependent types


- HKT allows functions (operators) between types (from type to type), these type-level functions are called *type operators*.

- higher-kinded types (type vars with args) shall not be confused with higher-order types.



## Examples

```hs
-- type-ctor Either :: * -> *
-- takes 2 types, * and *
data Either f a = Either f a
-- e.g.
:k Either :: * -> * -> *
:k Either Int :: * -> *
:k Either Int Int :: *
:k Either (Maybe Int) :: * -> *
:k Either (Maybe Int) Int :: *
:k Either Maybe -- ERROR
-- it doesn't want `Maybe`, for that it have kind (* -> *) -> *
-- which is impossible (right?),  it'd have to be (* -> *) -> * -> *
-- at least.

-- type-ctor T2 :: (* -> *) -> * -> *
-- takes a unary type-ctor :: (* -> *) like Maybe or []
-- and a Type :: *
data T2 f a = T2 (f a)
:k T2 :: (* -> *) -> * -> *
:k T2 Maybe :: -> * -> *
:k T2 []    :: -> * -> *
:k T2 Maybe Int :: *
:k Maybe :: * -> *
:k []    :: * -> *


```


## References

https://en.wikipedia.org/wiki/Kind_(type_theory)
http://dev.stephendiehl.com/fun/001_basics.html
https://www.stephanboyer.com/post/115/higher-rank-and-higher-kinded-types
