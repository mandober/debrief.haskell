# Types

## Values, Types, and Other Goodies

https://www.haskell.org/tutorial/goodies.html

Haskell is a purely functional language which means that all computations are done via the evaluation of expressions to yield values.

- Expressions are entities at the syntax level, i.e. syntactic terms.
- Values are expression. Values are abstract entities that we regard as solutions. Values live at the syntactic and semantic levels.
- Types may be considered as sets of values.

Every expression, therefore every value, has an associated type.

A set-theoretic interpretation is to think of a *type as a set of values*, or, better, *type as a class of values* since classes are sets with more restricted membership requirements, which also applies to types, e.g. unlike a set, we cannot construct a type consisting of arbitrary values { 1, True, a -> a }; we cannot even easily define a type that is a subtype of another type, e.g. a subtype of integers, ℕ = { n ∈ ℤ | n >= 0 }.

The basic set of types that a language provides may consists of more then just the fundamental, atomic, primitive i.e. *base types*.

Examples of expressions include
- the integer `5` is a numeric literal value (¹)
- the character `'a'` is also a literal
- the list `[1,2,3]` is a literal structured value
- the pair `('b',4)` is a literal structured value
- the function `\x -> x+1`


(¹) Actually, bare numeric literals, such as `5`, are polymorphic values whose type is inferred based on the surrounding context, or by the predefined rules, if inference is not possible. On its own, its type begins as `5 :: Num a => a`, i.e. it is only restricted to one of the number types, but may later become more specialized, e.g. `5 :: Integral a => a`, if the context suggest that it must be a whole number; it may become a concrete type, e.g. `5 :: Double`, if its contet suggest so.


Just as expressions denote values, *type expressions* are syntactic terms that denote type values (or just types).

Examples of type expressions include
- `Integer` infinite-precision integers (atomic type)
- `Char` characters (atomic type)
- `Integer -> Integer` functions mapping integers to integers
- `[Integer]` homogeneous lists of integers (structured type)
- `(Char, Integer)` pairs of characters and integers (structured type)



## Type properties and behaviour

In Haskell, types are mainly compile-time construct. After the compilation, types are erased, so the runtime has no access to type information.
