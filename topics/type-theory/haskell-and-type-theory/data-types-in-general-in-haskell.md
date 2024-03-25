# Haskell data types in general

From the type theoretical standpoint, Haskell's data types are algebraic:
- (0) the `𝟘` type is called `Void`
- (1) the `𝟙` type is unit and denoted by `()`, both at term and type level
- (⨯) product types model records and logical conjunction
- (+) sum types model disjoint unions and logical disjunction
- (^) function types and type families model exponential types

Canonically, Haskell's algebraic data types (ADTs) are, possibly recursive, sums (+) of products (⨯).

While Haskell has both algebraic data types (ADTs) and generalized algebraic data types (GADTs) nailed down, it is still lacking an essential association between the term and the type level covered by the *dependent types*. The work on implementing dependent types in Haskell is underway, but since Haskell wasn't made with this feature in mind, it turns out to be quite hard to introduce it now without breaking coherence of other established language features. One of these difficulties has to do with *type erasure* - knowing which types to keep at runtime and which are safe to erase at compile time. Discarding the unnecessary data in the form of type information contributes to efficiency, in no small amount either.

In the meantime, dependent types are modeled using the *singletons* approach. However, to get a feel for how it is to work in a language with dependent types proper, you can see Agda and Idris (both written in Haskell), each of which approaches and makes further exploration in type theory in its own way.
