# Algebraic types

Algebraic types are so called because they have a certain degree of correspondence with algebra and some arithmetical operations.

Results of some aritmetical expression are determined almost arbitrarily. For example, `0⁰ = 1`, which is the result based on convenience rather than intuition or even logic. It has to do with the fact that 1 is the identity of multiplicative operations. Another, combinatorics-related interpretation is that there is 1 way to rearrange 0 elements, although, come to think of it, that interpretation is also based on 1 being the identity of multiplication. The important thing is that the other operations behave consistently in relation with exponentiation, especially with its result in the case of raising zero to the zeroth power. Note that `a⁰ = 1` and `0ᵃ = 0 if a ≠ 0`.

This result is employed in showing (below) that all empty types are isomorphic to each other - meaning that for all intents and purposes there is only one empty type, or, said differently, the empty type is unique. But even though we should say "the empty type", just how we say "the empty set", it is often conveninent to be able to distinguish between different empty types, which can only be done if they have names, i.e. it is only possible to refer to empty types nominally (in a nominal type system), since structurally (in a structural type system) they are the same (since they are empty they have the same structure, i.e. no structure at all, and being empty they have the same content, i.e. no content - no value is a member of an/the empty type; which, of course, is not entirelly true since… stop! over and out).

Arithmetically, we deal with the concrete numbers, but algebraically we swap the numbers with variables to concentrate on the behavior of operations instead. In algebra, it is nevertheless necessary to determine what the *identity (or unit)* of an operation is, and those are concrete numbers. Therefore, it is useful to start by first establishing the correspondence between types and the numbers 0 and 1.

Note: 0 and 1 are just labels denoting the *additive and multiplicative identity*, respectivelly, which are often not even numbers. So the additive identity is often denoted by, possibly subscripted, `0` even though when it doesn't stand for a literal number 0. Even more frequently, because many operations are considered as multiplicative (as opposed to additive) for no good reason, their identity is denoted by, possibly subscripted, `1`.

For example, the *composition of functions* is a binary operation whose unit is the identity function.

Note: the phrase ending the previous paragraph (i.e. "the[sic] identity function") is tricky article-wise: Polymorphically, there is only one identity function called `id`, which justifies the use of "the". Logically, or in the least, logistically, however, each type does have its own identity function, `id @τ` for some type `τ` (e.g. `id @Int :: Int → Int`), calling for the use of "an". Logically, still and moreover, this apparent contradition underlines the paradoxical, if not that then semi-undecidable, nature of this matter, by its own granting us the (or "a"?) poetic license to use whichever article hitherto, henceforth nothwithstanding. [the importance of being super clear]

- the identity {axiom, property}
- an identity element, or a neutral element, or a unit, (always wrt an op)
- left identity + right identity = total identity
- the number zero: 0
- the number one: 1
- additive identity: `0`, "zero", e.g. `(ℕ,0,S)`
- multiplicative identity: `1`, "one", e.g. `1ᵃ`, `1ₘ`, `1ꜰₐ = F 1ₐ`
- operation: denoted by a symbol

One of the popular generic labels for an identity (element) is the possibly subscripted `id`.



Void ≅ 0 ≅ ∅

() ≅ 1 ≅ {∙}

A type that corresponds to 0 should have no inhabitants (no values); such a type is said to be uninhabited (implying that more than one `x` exists such that `x` is a type and uninhabited - this is a reason "the" doesn't often qualify the phrase "empty type").

Haskell's type `Void` is the best correspondence to zero.






## Table of correposndences

ar| Haskell type | set                | tt         | cat
--|--------------|--------------------|------------|------------
0 | Void         | the empty set, ∅   | uninhabited| category 𝟘
1 | ()           | singleton set, {∙} | singleton  | category 𝟙
2 | Bool         | 2-set              | 2-type     | category 𝟚
\+| Either       | Disjoint set, ⨄    | sum type   | coproduct
⨯ | Pair         | Cartesian product, ⨯| product   | product
^ | Function type| Function on sets    | exp       | exponential object
