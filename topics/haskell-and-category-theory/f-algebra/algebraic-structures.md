# F-algebra

## Monoids

In mathematics, an *algebraic structure*, or *algebra*, is a mathematical object consisting of a set, called the underlying or *carrier set*, endowed with one or more *operations* over the elements of the set, along with a set of *axioms* that the set and operations must satisfy.

So, there are sets, standalone sets, like the sets ℕ and ℤ but also more complex ones, and there are operations over the set that are often taken to be like the arithmetic operations. In fact, since many algebraic structures have 2 attached operations and since one behaves similar to addition and the other similar to multiplication, they are often identified with the same names, and sometimes even with the same symbols, `+` and `⨯`.

Further, since the identity element of arithmetical addition is `0`, the same symbol `0` is used to denote the additive identity element, and similarly with `1` as the multiplicative identity. Also, if the attached operation is additive, then it may have the additive inverse, denoted by `-x`, for any element `x`. And if the operation has multiplicative inverse, it may be denoted by `x⁻¹`, for any element `x`.

Summary of operations over the carrier set `S`:
- when there are 2 attached operations, they called addition and multiplication
- when there is 1 attached operations, it may not be identified as either

Addition
- additive operation is denoted by `+`
- additive operation may be closed over the set, `∀x y ∈ S. x + y ∈ S`
- additive identity, `0`, such that `∀x !∃0 ∈ S. 0 + x = x = x + 0`
- additive inverse, `∀x !∃(-x). x + (-x) = 0 = (-x) + x

Multiplication
- multiplicative operation is denoted by `⨯` or `∙`, or by juxtaposition
- multiplicative identity, `0`
- multiplicative inverse, ∀x !∃x⁻¹. x ⨯ x⁻¹ = ε = x⁻¹ ⨯ ε


Usually the sets compatible with the arithmetic operations are called numeric sets; however, the sets need not be numeric, nor the operations arithmetical to construe an algebra.



A **monoid** consists of a carrier set endowed with a single associative binary operation closed over the set.

Monoid
- a carrier set `S`
- 1 associative binary operation (•) closed over the set
- closure: `(∀ a b ∈ S). a • b ∈ S`
- associativity: `(∀ a b c ∈ S). a • (b • c) = (a • b) • c = a • b • c`

We've seen a monoid as a set, as a single-object category, as an object in a monoidal category, and now let's focus on the definition of a monoid as a set `m` with a pair of functions `μ` and `η`.

```hs
μ : m × m -> m
η : 1 -> m
```

Monoid:
- `m` is a carrier set
- `μ` is a closed binary operation
- `η` function selects the unit element
- `1` is the terminal object in the Set category
