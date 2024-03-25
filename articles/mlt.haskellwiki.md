---
downloaded:       2022-01-01
page-url:         https://wiki.haskell.org/Monad_Transformer_Library
page-title:       Monad Transformer Library - HaskellWiki
article-title:    Monad Transformer Library - HaskellWiki
---
# Monad Transformer Library - HaskellWiki

The MTL provides a selection of monads and their transformer variants
along with type classes that allow uniform handling of a base monad and its transformer.
The MTL provides a selection of [monads][1] and their [transformer][2] variants along with type classes that allow uniform handling of a base monad and its transformer.

See the package on [Hackage][3].

Version 2 of the MTL has some small [incompatibilities][4] relative to version 1. See "[Upgrading from MTL 1 to MTL 2][5]" for instructions on how to make code written for version 1 work with version 2.

### History

Once upon a time, mtl was a standalone monad transformer library, that defined monad transformers and constructor classes for them. The constructor classes were frequently [multiparameter classes][6] with [functional dependencies][7].

Some time later, [type families][8] were invented, and were found to solve several of the same problems as multiparameter classes with functional dependencies. A package named mtl-tf was developed, to provide the functionality of mtl but using type families instead.

However, there was a lot of duplicated code involved: mtl and mtl-tf both defined their own monad transformers, so they didn't work well together. The constructor classes were different by necessity, because they used different technology, but the data types themselves were duplicated for no reason.

Hence, transformers was developed: a library that used no extensions to Haskell98 and only defined the transformer types themselves, so that more advanced libraries could share them as a base. monads-fd and monads-tf were developed as libraries that took transformers from the transformers package and defined the constructor classes over the top of them, monads-fd using multiparameter typeclasses with functional dependencies and monads-tf using type families.

The technological problem was then solved, but unfortunately the original mtl library already had a lot of traction: it had existed for longer and was in widespread use. So instead of encouraging people to switch libraries, the maintainers switched implementations instead: they decided to merge monads-fd and mtl into a single library.

Since the mtl name was far better known, monads-fd was deprecated, and mtl was switched to depend on transformers. Since transformers had some stylistic differences as well, this resulted in some API changes (see above).

As it now stands, transformers is the "standard" monad transformer library, but mtl and monads-tf both act as extensions of it. monads-tf is not very well-used, which is unfortunate as it's quite a nice approach.

### See also

-   [Stack Overflow: mtl, transformers, monads-fd, monadLib, and the paradox of choice][9]
-   [libraries mailing list: Haskell Platform Proposal: add transformers and revise the mtl package to depend on it][10]

[1]: https://wiki.haskell.org/Monad "Monad"
[2]: https://wiki.haskell.org/Monad_Transformers_Explained "Monad Transformers Explained"
[3]: http://hackage.haskell.org/package/mtl/
[4]: https://wiki.haskell.org/Incompatibilities_between_MTL_1_and_MTL_2 "Incompatibilities between MTL 1 and MTL 2"
[5]: https://wiki.haskell.org/Upgrading_from_MTL_1_to_MTL_2 "Upgrading from MTL 1 to MTL 2"
[6]: https://wiki.haskell.org/Multi-parameter_type_class "Multi-parameter type class"
[7]: https://wiki.haskell.org/Functional_dependencies "Functional dependencies"
[8]: https://wiki.haskell.org/Type_families "Type families"
[9]: http://stackoverflow.com/questions/2769487/mtl-transformers-monads-fd-monadlib-and-the-paradox-of-choice
[10]: http://www.haskell.org/pipermail/libraries/2010-September/014281.html
