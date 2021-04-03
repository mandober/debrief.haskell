# Isomorphism

An abstract data type (structure) may be represented using different concrete implementations. For example, sets can be implemented using a list or a tree (or some other undelying structure) and although these types (list, tree) represent the same abstract data structure (set), they implement it differently.

In one sense, they are certainly unequal, possibly because they have a different name, but more fundamentally, because each type uses a different underlying type (structure) which implies a possibly different set of available functions.

Consequently, programs which use two or more of these types together must contain conversion functions. The amount of code required to define conversion functions may not be large, but if the declarations are machine-generated, or the number of representations to be simultaneously supported is large, then the size of the conversion code can grow.

Isomorphisms

The fact that some types represent the same abstract type is captured by the notion of isomorphism. Two types are *isomorphic* if we can define an inversion function between them, i.e. a pair of conversion functions, to convert one type to the other.

Besides invertibility, two basic facts about isomorphisms (isos, for short):
* the identity function is an iso, so every type is isomorphic to itself
* the composition of two isos is an iso.

> Considered as a relation isomorphism is an equivalence on types.

Other familiar isos are a consequence of the semantics of base types. For example, the conversion between meters and miles is a non-identity iso between the floating point type Double and itself; if we preserve the origin, the conversion between cartesian and polar coordinates is another example. Finally, some polymorphic isos arise from the structure of types themselves. For example, one often hears that products are associative *up to isomorphism*.

It is the last sort, often called *canonical or coherence (iso)morphisms*, which are of main interest. Canonical isos are special because they are uniquely determined by the types involved, that is, *there is at most one canonical iso between two polymorphic type schemes*.

## Monoidal isomorphisms

Let `Unit` and `:*:` be nullary and binary product constructors, respectively, and `Zero` and `:+:` nullary and binary sum constructors, defined as the following datatypes.

```hs
-- product type
data Unit = Unit
data a :*: b = a :*: b

-- sum type
data Zero
data a :+: b = Inl a | Inr b
```

A few canonical isos of Haskell are summarized by the syntactic theory:

```hs
-- IDENTITY
-- a * 1 = a = 1 * a
a :*: Unit ≅ a ≅ Unit :*: a     -- product type identity
a :*: Unit ≅ a                  -- left identity
a ≅ Unit :*: a                  -- right identity

-- a + 0 = a = 0 + a
a :+: Zero ≅ a ≅ Zero :+: a     -- sum type identity
a :+: Zero ≅ a                  -- left identity
a ≅ Zero :+: a                  -- right identity

-- ASSOCIATIVITY
-- a * (b * c) = (a * b) * c = a * b * c
(a :*: b) :*: c ≅ a :*: (b :*: c)
-- a + (b + c) = (a + b) + c = a + b + c
(a :+: b) :+: c ≅ a :+: (b :+: c)

-- COMMUTATIVITY
-- a * b = b * a
a :*: b ≅ b :*: a
-- a + b = b + a
a :+: b ≅ b :+: a

-- CURRYING
-- (a, b) -> c ≅ a -> (b -> c) == a -> b -> c
(a :*: b) -> c ≅ a -> (b -> c) == a -> b -> c

-- DISTRIBUTIVITY
-- a(b + c) = ab + ac
a :*: (b :+: c) ≅ (a :*: b) :+: (a :*: c)   -- distributivity (left)
-- (b + c)a = ba + ca
(b :+: c) :*: a ≅ (b :*: a) :+: (c :*: a)   -- distributivity (right)
```
