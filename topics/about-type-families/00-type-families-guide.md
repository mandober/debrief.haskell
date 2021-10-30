# Type Families in Haskell

`Type Families in Haskell: The Definitive Guide` by Vladislav Zavialov, 2021
https://serokell.io/blog/type-families-haskell


## Abstract

Type families are one of the most powerful type-level programming features in Haskell. You can think of them as type-level functions, but that doesn't really cover the whole picture. By the end of this article, you will know what they are exactly and how to use them. We will talk about the following topics:

- type constructor flavours
- closed type families
- type constructor arity
- synergy with GADTs
- evaluation order or the lack thereof
- open type families
- overlapping equations
- compatible equations
- injective type families
- associated types
- data families
- non-parametric quantification
- non-linear patterns

## Conclusion

Type families are a powerful and widely used ([20%][1] of Hackage packages) feature. They were introduced in 2005 in the form of [associated type synonyms][2], and remain a subject of active research to this day, with ongoing innovations.

- 2005 associated type synonyms
- 2013 [closed type families][3]
- 2015 [injective type families][4]
- 2017 [constrained type families][5]

While a useful tool, type families must be used with great care due to open issues such as [#8095][6] ("TypeFamilies painfully slow") and [#12088][7] ("Type/data family instances in kind checking"). However, there are ongoing efforts to address these issues.


[1]: https://mail.haskell.org/pipermail/ghc-steering-committee/2020-November/001876.html
[2]: https://www.microsoft.com/en-us/research/wp-content/uploads/2005/01/at-syns.pdf
[3]: https://richarde.dev/papers/2014/axioms/axioms-extended.pdf
[4]: https://richarde.dev/papers/2015/injective/injective-type-families-extended.pdf
[5]: https://richarde.dev/papers/2017/partiality/partiality-extended.pdf
[6]: https://gitlab.haskell.org/ghc/ghc/-/issues/8095
[7]: https://gitlab.haskell.org/ghc/ghc/-/issues/12088
