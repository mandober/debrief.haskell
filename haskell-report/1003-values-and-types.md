# 1.3 Values and Types

https://www.haskell.org/onlinereport/haskell2010/haskellch1.html#x6-120001.3

The term "expression" is not unambigious anymore, because Haskell has long outlived its Haskell2010 roots (let alone Haskell98 ones), becomming a language with strong type-programming capabilities (enabled with various GHC language extensions).

> Haskell 1.0 > … > Haskell98 > Haskell2010 > GHC2021

De facto Haskell standard in the year 2021 is a GHC-flavoured Haskell varant known as **GHC2021**. It takes Haskell2010 as a base, but enhances it with numerous extensions that make the language outlive that standard (after all, it's been 11 years!). The people that would write a new standard are the same people that author these extensions, so the publishing of the new standard is delayed, but it goes by the unofficial name, `GHC2021`. This name comes from the eponymous language extension, available in GHC 9.2+, that enables a number of vetted and safe extensions, the most of which have been used in the wild for a long while now.

GHC2021 is compatible with dependent types (to be implemented sometimes in the future) and already sports many type-level programming features. Partly because of that, it has become common to discuss about type expressions and type values, "lifting" these phrases from the term-level up to the type-level, thereby making the unqualified phrases ambiguous: "expression" alone (probably) still refers to a term-level value, but it's better to qualify it appropriately with "term-level" or "type-level" modifier.


Each expression evaluates to a value and has a static type. Term-level and type-level values are clearly separated in Haskell. The type system allows user-defined datatypes of various sorts, and permits not only *parametric polymorphism*, using a traditional *Hindley-Milner type structure*, but also *ad hoc polymorphism* (overloading) using type classes.

Errors are semantically equivalent to `⟘`. Moreover, errors are indistinguishable from non-termination, so there is no mechanism for detecting and acting upon a particular failure.
