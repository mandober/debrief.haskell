# Haskell :: Haskell and CT :: The Hask category

The Hask category
- category: `Hask`
- objects: Haskell types
- morphisms: Haskell functions

## Objects in Hask

Objects in Hask are *Haskell types* like the base types `Void`, `()` `Bool`, `Int`, `String`, and the compound types like `Maybe Float`, `(Bool, Int)`, `Either String Int`, `String -> IO ()`. That is, the *saturated Haskell types* play the role of objects in Hask. A type is more than just a set, but sometimes it is useful to consider Haskell types as set of values.

Some important objects of the `Set` category also appear in the `Hask` category: there is the initial object, and there is the terminal object as well. Like Set, Hask also has products, coproducts, exponential objects.

## Morphisms in Hask

**Morphisms in Hask** are *Haskell functions* like `Bool -> Int`, `String -> IO ()`. The fact that the `String -> IO ()` is both a morphism and an object in Hask is due to the fact that Hask is a very morphism-rich category, so all the usual categorical constructions such as products, coproducts, exponential objects, initial and terminal object, the lot, are present in it. So this and similar functions are both morphisms and exponential objects in Hask.

## Parameteric polymorphism

Haskell has parameteric polymorphism, so types can be represented by type variables. For example, `forall a. a` represents any Haskell type.
