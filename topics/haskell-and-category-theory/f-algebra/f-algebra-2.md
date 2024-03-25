# F-algebra

## Algebraic structure

In mathematics, an *algebraic structure*, or *algebra*, is a mathematical object consisting of a set, called the underlying or *carrier set*, endowed with one or more *operations* over the elements of the set, along with a set of *axioms* that the set and operations must satisfy.

## The building blocks of algebraic structures

So, there are sets, standalone sets (like the sets ℕ and ℤ), and there are operations (like the arithmetic operations of addition and multiplication) that operate over certain sets. Usually the sets compatible with the arithmetic operations are called numeric sets; however, the sets need not be numeric, nor the operations arithmetical to construe an algebra.

Together with the axioms, we have the 3 entities: sets, operations, axioms. With each being a set, we actually have
1. the set of the carrier sets (𝔹, ℕ, ℤ, set of strings, set of functions, …)
2. the set of the operations (+, -, ⨯, ∸, :, ++, ∘, div, abs, mod, …)
3. the set of the axioms (closure, associativity, identity, commutativity, distributivity, cancellativity, domination, idempotence, …)

We can combine the members of the 3 sets in myriad ways, and even though not all combinations produce sensible results or valid algebras, we can imagine that the number of distinct algebras is a very big one, especially when we also calculate in the possibility that the same carrier set may be combined with one, two, or more operations, each time producing a distinct algebra.

Axioms prescribe the expected behavior of algebras. Axioms constrain possible effects and interactions of an operation with the undelying set. Axioms are related to the set and operation together. It is only when we condsider a pair, made out of an operation and a set, that we are able to determine whether some property, prescribed by an axiom, is satisfied. However, some axioms, like associativity, seem to be more relevant to an operation than to its underlying set. Particularly since we can discuss the associativity of an operation without mentioning any particular set.





One of the ubiqutous axioms is *the axiom of closure or totality* which states that the operation must not produce results (elements) outside the carrier set. For example, *addition over ℕ* respects the axiom of closure, but *subtraction over ℕ* does not (as it may produce a negative number). Naturally, this depends on the way subtraction is defined wrt the set ℕ - here, we're assuming the usual definition of subtraction, not the similar operation called *monus* (`∸`) which is, indeed, closed over ℕ (because it defines `n - m = 0` if `n <= m`, e.g. 2 ∸ 5 = 0).





Each set that admits some operation has its own particular way the operation is performed over its elements.

This is reflected in the PLs in that the operations, particularly the elementary arithemtic operations like addition and multiplication work differently with each type that admits such operations over its elements.

In Haskell particularly, addition and multiplication have numerous conrete implementations, one implementation for each primitive numeric type (e.g. addInt, addFloat, mulDouble, addWord8, mulWord, etc.). With all the other arithemtic operations this amount to an enourmous number of distinct functions. To curb this, the arithmetic operations are available at the higher level of abstraction, using ad hoc polymorphism, as the methods of the `Num` class.

The `Num` class declares the methods, most having to do with the basic arithemtic operations, so any type that classifies numeric values can becomes a member of this class. In a similar way, algebraic structures are realized as type classes, so a type together with a certain "attached" operation can become a member of an appropriate class.




An algebraic structure is also called an algebra, although, confusingly, certain algebraic structures are also called algebras; thus the term "algebra" may denote the general class as well as a certain subclass of objects.


## Monoids, again

A monoid is an algebraic structure consisting of a carrier set that is endowed with an associative binary operation closed over the carrier set, which also has a unique identity element.

In Haskell, sets correspond to types, the operations attached to a carrier sets correspond to the methods of a class, and algebraic structures correspond to classes. Perhaps it's more precise to say that the algebraic structures are implemented as Haskell classes.

monoid is a class that groups the types with these prescribed behaviours. Actually, the `Monoid` class is a subclass of `Semigroup` class that groups types 

The additional requirement is the existence of the unique identity element, denoted by `ε`, that is neutral when combined with the other elemens of the carrier set. This may be stated as saying that the identity element respects left and right identity axioms: it leaves any other element unchanged when combined with it, whether as a left or right operand.

Monoid, `M`
- carrier set `A`
- binary operation `∙`
- axioms (3)
  - closure
    - `∀ a b ∈ M. a • b ∈ M`
    - Magma is an algebraic structure with closure.
    - closure is a property of the attached binary operation (binop).
    - the binop must be closed over the elements of the carrier set.
  - associativity
    - `∀ a b c ∈ M. a • b • c = a • (b • c) = (a • b) • c`
    - associativity is a property of the attached binary operation (binop).
    - Semigroup is an algebraic structure with closure and associativity.
    - Semigroup is an associative magma.
  - identity
    - left identity,  `∀a !∃ε ∈ M. ε • a = a`
    - right identity, `∀a !∃ε ∈ M. a • ε = a`
    - identity axioms asserts the existance of a unique neutral element; when the identity element is combined (as a binop operand) with any other element, the result is always that other element unchanged.
    - The identity is the property of the carrier set and the binop together.
    - Monoid is a Semigroup with identity.


## Monoids, categorically

Categorically, a monoid may be defined with a pair of functions `μ` and `η`. The type of the `μ` function is straightforward - it combines any two elems of the monoid to produce the result that is also an elem in the monoid, i.e. `μ` has the closure property, `∀ a b ∈ M. a • b ∈ M`. 


and called unit or identity - while it definitely has the type `M`, it seems we can't represent it as a value, `1 : M`, but it must be represented as a function. Since this is Haskell from the perspective of category theory, and we're focusing on the category `Set`, …wait, what? `Monoid` is a single object category, and we're discussing monoids, so we're focusing on a monoidal category, not on `Set` cat!


we recall that a terminal object in this category is actually a (any) singleton set, denoted by `1`. Further, there are `x` number of functions from `1` to any set `x`, 

`1 -> a` ~ a¹ = a


```hs
-- binary assoc fn
μ :: m × m -> m    ≅  μ ∈ m^(m×m)
-- identity elem selector fn
η :: 1 -> m        ≅  η ∈ m¹

m^(m × m) × m¹ = m^(m × m + 1) = m^(m² + 1)
```
