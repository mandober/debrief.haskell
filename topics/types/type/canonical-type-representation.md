# Canonical type representation

Cardinality of a set is the number of elements it contains, and by Curry-Howard correspondence between types and sets, we can talk about the cardinality of types - the number of inhabitants (terms, values) each type has, usually ignoring bottoms.

Any two types with the same cardinality are isomorphic to each another. An isomorphism between them is defined by a pair of functions to convert between the two types.

In general, for any two types with cardinality n, there are n! unique isomorphisms between them. As far as the math is concerned, any one of these is fine, and for the most purposes, just knowing that an isomorphism exists at all is good enough.

An isomorphism between two types is the proof that, for all intents and purposes, the two types are the same; we usually say that the two types are equivalent up to that isomorphism.

A direct corollary that any two types with the same cardinality are isomorphic, is that there are multiple ways to represent any given type. However, having multiple forms of representation will soon spawn the question of what form is the most representative, the most canonical one. This is especially true when working with types generically (Haskell Generics).

The canonical representation of any given type is known as a *possibly recursive sum of products*.

- The canonical sum type (disjunction) is `Either`
- The canonical product type (conjunction) is `(,)`

We need to represent all sum types in terms of `Either`, and all product types in terms of a 2-tuple i.e. `(,)`, and all other types in term of these two, as sum of products.

## Examples

```hs
Bool ≅ Either () ()
Maybe a ≅ Either () a

(a, Bool) ≅ (a, Either () ())

List a ≅ Either () (a, List a)
```

`Bool` is a sum type so its canonical repr is `Either () ()`. `Either` has two distinguishing variants, `Left` and `Right`, and they need not carry any additional info (units suffice) since the `Bool` itself also doesn't hold any information in its two nullary variants (data ctors), `True` and `False`.
