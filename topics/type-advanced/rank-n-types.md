---
title        : Rank-N Types
slug         : rank-n-types.md
path         : debrief.haskell/terms/
keywords     : rankN types, forall
---

# Rank-N Types

If we were to write a fn, we intend for it that it takes `id :: a -> a` fn and some integer, and then applies id to the integer, the naive approach'd be:

```hs
bad :: (a -> a) -> Int  -- TYPE ERROR
bad f = f 5
```

This is wrong! The reasoning was that because `id` has type `a -> a`, the fn `bad` should have type `(a -> a) -> Int`. Unfortunately, Haskell disagrees and warns about mismatch types: *"Couldn't match expected type a with actual type Int"*.

> The caller of a polymorphic function is responsible for choosing which concrete types the type variables get.

The signature `(a -> a) -> Int` promises that `bad` will happily take any function that returns the same type it takes. We wanted it to only be able to take `id` fn as an arg, but instead we've written a function which would happily take any endomorphism.

> **Endomorphisms** are functions which take and return the same type.

For example, `not :: Bool -> Bool`, `show @String :: String -> String` and 
`id :: a -> a` are all endomorphisms, but `words :: String -> [String]` is not.

Because the choice of the type for a TP is at the mercy of the caller, Haskell has no choice but to reject the above definition; it must anticipate that some user will call it with some other fn then `id`.

**By default, Haskell implicitly universally quantifies the TPs**, meaning that the type signature `a -> a` is really `forall a. a -> a`.

By enabling *RankNTypes* we can write these types explicitly. Comparing `id` and `ok` side-by-side is revealing:

```hs
{-# LANGUAGE RankNTypes #-}

id :: forall a. a -> a
id a = a

ok :: forall a. (a -> a) -> Int
ok f = f 5
```
