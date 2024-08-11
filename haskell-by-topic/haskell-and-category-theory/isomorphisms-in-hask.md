# Haskell :: Haskell and CT :: Isomorphisms in Hask

However - and this is the same with sets - any two types that have the same cardinality are isomorphic to each other. Being **isomorphic** means we can convert one type into the other, without a loss of information. Actually, some information might be lost because isomorphism is a weaker form of equality. In fact, an isomorphism really means that - from the point of view of any other object in the same category - the two isomorphic objects are indistiguishable from each other; any third object cannot tell them apart.

An **isomorphism** is defined as a bijection (bijective function) between two objects (sets, types). A function `f : a → b` is an isomorphism if there is the inverse function of f, `g : b → a`, such that `f ∘ g = 1ᵇ` and `g ∘ f = 1ᵃ`.

When this is the case, the two objects `a` and `b`, are said to be *isomorphic*, and both `f` and `g` are called *isomorphisms*.

### Example of invertable bijections

Here is an example of a pair of bijective function `f` and `g`, that are each other's inverses, acting on a pair of sets `a` and `b`:

```hs
a = {p, q, r}
b = {x, y, z}

f : a -> b
f(p) = x
f(q) = y
f(r) = z

g : b -> a ∧ g = f⁻¹
g(x) = p
g(y) = q
g(z) = r

g ∘ f = 1ᵃ
g(f(p)) = g(x) = p
g(f(q)) = g(x) = q
g(f(r)) = g(x) = r

f ∘ g = 1ᵇ
f(g(x)) = f(p) = x
f(g(y)) = f(q) = y
f(g(z)) = f(r) = z
```

The two bijective functions `f` and `g` are each other's inverses: the roundtrip `f ∘ g` is exactly the same as the identity function `1ᵇ`; and the roundtrip `g ∘ f` is exactly the same as the identity function `1ᵃ`.

### Example of non-invertable bijections

When comparing the cardinality of two sets `a` and `b`, we say that they are *equinumerous* if there is a bijection `a -> b` and a bijection `b -> a`. But these two bijections need not be inverses of each other - in general, we won't get the same element back from the roundtrip and from the identity function. Such two functions could still be used to compare cardinalities of sets but they are not isomorphisms.

```hs
a = {p, q, r}
b = {x, y, z}

j : a -> b
j(p) = y
j(q) = z
j(r) = x

j : b -> a
k(x) = q
k(y) = r
k(z) = p

k ∘ j ≠ 1ᵃ
since
  k(j(p)) = k(y) = r   and p ≠ r

j ∘ k ≠ 1ᵇ
since
  j(k(x)) = j(q) = z   and x ≠ z

so
  j ≠ k⁻¹
  k ≠ j⁻¹
```

## Bijection, inverse and isomorphism

- bijection
- injection
- surjection
- inverse, invertability of jections
- uniqueness
- isomorphism
- epimorphism
- monomorphism

When a function has an inverse function, that inverse is a unique function - the inverse function of a function is always unique.

A bijection is in general invertable, but when it is not paired with its inverse, the objects that the round-trip returns are not, in general, the same as these returned by the identity function.

* An isomorphism implies the existence of a function `f` and the inverse function `g = f⁻¹` such that `f ∘ g = 1ᵇ` and `g ∘ f = 1ᵃ`, which guarantees they are *each other's inverses*. An isomorphism implies the existence of two bijective functions `f` and `g` that are each other's *inverses*.
* A bijective function `f` does not imply anything; a function `g` that is an inverse of `f` may or may not exist. If it does than both `f` and `g` are bijections and inverses of each other.
* Being an inverse implies bijectivity. Being bijective implies shit.
* Being an isomorphism implies both bijectivity and inverse; it implies the existence of the inverse function which must be bijective.

* Two bijective functions may be used to compare sets for cardinality - it doesn't matter that the roundrip does not return the same element as the identity function - only that they both map elements injectively and totaly.

>An isomorphism is thus a pretty strong requirement - it requires the existence of two functions, both of which are bijective and each other's inverses. Isomorphism is often said to be weaker than identity, but defined like it is, does not leave room for non-identity mappings; each roundtrip is equal to the associated identity function: `f ∘ g = 1ᵇ` and `g ∘ f = 1ᵃ`, which just doesn't leave room for non-identity mappings. Only *half-isomorphism* does, i.e. when only one of these two conditions holds.

Two bijections, `f : a -> b` and `g : b -> a`, between sets `a` and `b`, when composed together, do not return the same elements, in general, as the appropriate identity functions; that is, `f ∘ g ≠ 1ᵇ` and `g ∘ f ≠ 1ᵃ` in general. Even though both `f` and `g` are bijections - they may not be each other's inverses.
