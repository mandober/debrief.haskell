# Making new types

The 3 keywords related to making new types: `type`, `newtype` and `data`.

## Type synonym

A weak type alias is made using the `type` keyword. It's weak because the compiler doesn't consider the alias any different then the type it is aliasing. As far as compiler is concerned, they are type synonyms. Aliasing with the `type` may only be beneficial for the developers if they need the different names for the same type, but only at design-time. Also, type synonyms are often used for abbreviating a long type signature. You cannot do anything with the alias that you couldn't do with the type it is aliasing: you can't make a `type` alias an instance of some class; that is, you can, but only if the expanded type is a viable candidate for a class' instance.

## Newtypes

The `type` keyword doesn't create new types, so how about the `newtype` keyword?
