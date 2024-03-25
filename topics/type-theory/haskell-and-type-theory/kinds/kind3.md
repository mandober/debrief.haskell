# Kind

- *kind* is a *type-ctor*'s property
- types are sorted into kinds just like terms are sorted into types
- a kind is a sort of a type-ctor or higher-order type operator

- Haskell defines kinds as `κ := Type | κ -> κ` where `κ` is a kind variable

Kinds, `# ∪ Type ∪ Constraint =/∈/⊆/? TYPE`
- the most general kind that includes other kinds is `TYPE`
- unlifted types have kind `#`
- lifted types have kind `Type` (i.e. now deprecated `*`)
- type constraints have kind `Constraint`


- Kinds `?`, `??`, `#` and `(#)`

- the sort of types, which have kind `Type`, can be called **value types**

- In Haskell 98, `*` is the only inhabited kind, that is, all values have types of kind `*`. GHC introduces another inhabited kind, `#`, for unlifted types

- *Higher-kinded type* (HKT) is a type-ctor that has type variables. A saturated HKT always has the kind `Type` (hence its type-ctor doesn't).




## Kind in type theory
https://en.wikipedia.org/wiki/Kind_(type_theory)

A kind system is essentially a simply typed lambda calculus "one level up", endowed with a primitive type, denoted `*`, which is the kind of any saturated type-ctor.

Syntactically, it is natural to consider polymorphic types to be type-ctors, thus non-polymorphic types to be nullary type-ctors. But all nullary ctors, thus all monomorphic types, have the same, simplest kind `*`.

Since higher-order type operators are uncommon, in most PLs, kinds are used to distinguish between data types and the types of ctors which are used to implement parametric polymorphism.

Kinds appear (explicitly or implicitly) in a language whose type system accounts for parametric polymorphism in a programatically accessible way, such as Haskell.
