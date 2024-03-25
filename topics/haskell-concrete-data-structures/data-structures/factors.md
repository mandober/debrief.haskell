# Factors of classification of data structures in Haskell

## Data structures in Haskell

In Haskell, data structures are classified according to a number of traditional factors like linearity, finiteness, succinctness, and similar, but also according to some factors that are more-less unique to Haskell, most importantly by their purity, laziness/strictness, and then by the features shared with other FPL, such as persistency, recursion, cyclicity, nestedness.

Since Haskel is a pure FPL with non-strict (lazy) semantics (purity sort of implies laziness), many of the imperative categories of data structures need to be suitably modified. Some imperative data structures are inefficient to express in a pure language, but, on the other hand, some data structures difficult to express imperatively become easy.

First of all, the notion of a data structure versus the notion of a data type, that are blurred in many languages, are especially so in Haskell, where types become intertwined with operations to the point they become intechangable. For example, like in the lambda calculus, many data structures can be implemented in terms of functions. This may be done explicitly, by the programmer, but it is also done implicitly, by the complier; for example, an infinite list is more a function then it is a bona fide data structure.

In fact, Haskell's capability to express infinite data strutures doesn't hold just for list. Lists are indeed the go-to data structure in Haskell, intimately known to the complier and subject to a priviledged syntax sugar. Haskell manipulates them most efficintly and provides a huge library of operations over them. Even the primitive type String is realized as list (it's just a list of characters), but more efficient textual types are also available.

## Polymorphic data structures

All data structures are polymorphic - the structure is hard-coded but the type of payload is free to vary. In fact, it's possible to realize data structures so that even their structure is left to vary (it can have various degrees of polymorphism).

## Infinite data structures

It is easy to express infinite data structures. For example, the syntax to express an infinite list of non-negatove integers is just [1..].

```hs
nats  = [0..]
evens = [0,2..]
odds  = [1,3..]
```

## Cyclic data structures

Truly cyclic data structures do not contain thunks like infinite data structures, even though their are themselves also infinite. The trick to express a truly cyclic structure is made possible due to laziness and it is called 'tying the knot'.

```hs
-- cyclic and infinite
ones 1 = let 1 : xs in xs = ones 1

-- infinite but not cyclic
ones 1 = 1 : ones
```

## Recursive data structures

It is remarkable that recursion can be separated from a recursive data structure, permitting for a non-recursive definition of a data structure! At a later time (and possibly in a different module), recursion can be plugged back in! It's recursion as an opt-in feature, or a structural enhancement.


## Factors

Types of DS
- polymorphic data structures
- infinite data structures
- cyclic data structures
- recursive data structures
- nested data structures
- pure data structures
- persistent data structures (sharing, versions)

Factors
- overhead
- mutation
- in-place mutation
