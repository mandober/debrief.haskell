# Empty type

https://wiki.haskell.org/Empty_type

An empty type is an inhabited type. It is a type that has no values, which is not really possible in Haskell since *bottom* inhabits any type and it is impossibly to be rid of; *bottom* is a truly polymorphic type, present as a member of any and all types, past and future.

The empty types are often used with *phantom types* or *type type*. How you define one depends on how picky you are that the type has genuinely no values.

Frequently, when defining a type whose values are never meant to be used, the simplest way is to just define it with a single, token value, whose constructor you don't export:

```hs
data EmptyType = DoNotExportCtor
```

However, this is not a truly empty type, so some people find the following more satisfying:

```hs
newtype Void = Void Void
```

Although we do have ctors here, they do nothing (in fact, `Void` is essentially `id`) and the only value here is *bottom*, which is impossible to be rid of. This is as close as we can get to declaring the empty type (except that we might regard the syntax as somewhat non-obvious).

To address that concern, Haskell 2010, or GHC with *EmptyDataDecls*, allows you to just not specify any constructors at all - this is theoretically equivalent to the previous type, with less typing:

```hs
{-# LANGUAGE EmptyDataDecls #-}

data Void
```
