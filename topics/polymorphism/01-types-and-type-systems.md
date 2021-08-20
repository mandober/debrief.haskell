# Types and type systems

## Values, expressions and sorts of terms, types and kinds

Types classify language terms according to their common properties and behaviour. Kinds classify types in a similar manner. These three kinds of language entities are also known as *sorts*. Haskell has a tertiary hierarchy of sorts: terms are the most essential sort that sits at the bottom of the hierarchy; types classify the terms below, so they take the middle level; they are also classified by kinds which live in the penthouse.

Terms are the most basic language entities; they are the concrete data and the reason for the whole shenanigans. They are most commonly referred to by the terms "values" and "expressions", although these two terms have referants in the other sorts as well, especially the latter (e.g. "type expression" and "kind expression" are not uncommon expressions). The term "data" also has a semantic baggage that varies with the context. It seems the terms "term-level value" or "term-level expression" will have to do. Then, the other two sorts will have the corresponding forms, i.e. "type-level" and "kind-level" values and expressions.

## The base types

All expressions at the term-level strive to be reduced to a single primitive (scalar) value. Similarly, all expressions at the type-level strive to be reduced to a single concrete type, a base type such as Int, Float, Double. Even kind expressions long for a simple life.

The most primitive term-level values (scalars) are the leaves of a term expression tree; the whole longs to be reduced down to a single scalar. The base types terminate the type grammars. They are the leafs of a type-expression tree, and that tree awaits reduction down to a single base type as well.
