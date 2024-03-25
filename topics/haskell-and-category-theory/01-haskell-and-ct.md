# Haskell and Category Theory

## Morphisms in Haskell

Along with functors and natural transformations, morphisms are a fundamental concept in category theory.

# Hask category

The Hask category is a category consisting of Haskell data types as objects and functions as morphisms. Hask is not a proper category due the to presence of the bottom value in every type (all types are lifted), but we can ignore this problem invoking the "fast and loose reasoning" mantra and proceed to investigate Hask as if it were a proper category.

Hask
- objects: Haskell data types, `Int`, `Char`, `Double`, etc.
- morphisms: Haskell functions between Haskell data types, `Int -> Double`, etc.
- isomorphisms: functions between types of the same cardinality
- initial object: `Void`
- terminal object: `()`
- product object: `(,)`
- sum object: `Either`
- exponential objects, `(->)`
- functors: type ctors (type-level functions)
- natural transformations: functions between functors
- monads
- comonads
- adjunctions
- natural isomorphisms

Confined to just Haskell, we don't have the variety of categories at our disposal - there's the canonical `Hask` category, that resembles the `Set` category in many ways, but there are no others, except perhaps the categories we get by carving Hask into subcategories.

We could maybe also talk about the category that's opposite to Hask, but I'm not sure what would `Haskᴼᴾ` consist of? Is it enough to just reverse all the arrows of Hask and call it `Haskᴼᴾ`? (Probably. We'll see about this later).

## Natural transformations in Haskell

Along with morphisms and functors, natural transformations are a fundamental concept in category theory.
