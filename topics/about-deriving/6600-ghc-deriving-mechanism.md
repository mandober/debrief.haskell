# Deriving mechanism

6. Language extensions »    
6.6. Deriving mechanism

https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/deriving.html

Haskell98 permits attaching a deriving clause to a data type declaration in order to generate a standard instance declaration for specified class. GHC extends this mechanism along several dimensions:

* The derivation mechanism can be used separately from the data type declaration, using the [standalone deriving][sad].

* In Haskell98, the complete set of derivable classes is `Eq`, `Ord`, `Enum`, `Bounded`, `Ix`, `Read`, `Show`; however, not all of these classes are always derivable - there are some restricting conditions. On the other hand, through various [language extensions][ext] GHC supports deriving an additional set of classes, including `Data`, `Generic`, `Functor`, `Traversable`, `Foldable`, etc.

* The *stock* strategy more-less matches the behaviour of Haskell98, and derives instances by generating all method definitions. In fact, the generated code is probably more efficient than what was available at the time of the Haskell2010 and especially Haskell98 standards. For example, GHC 9.2 will use safe coercions to implement the required methods when generating instances for `Eq` and `Ord` classes (probably for other classes as well, but this two I've checked) for a newtype-based datatype, resulting in a very efficient code.

* Besides the *stock*, GHC supports two additional deriving strategies, which can derive arbitrary classes:
  - [generalised newtype deriving][ntd] for newtypes
  - [deriving any class][dac] using an empty instance declaration

* The user can also explicitly specify the desired [deriving strategy][ds], especially if the [default deriving strategy][dds] makes the wrong choice.




[sad]: https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/standalone_deriving.html#stand-alone-deriving
[ext]: https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/deriving_extra.html#deriving-extra
[ntd]: https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/newtype_deriving.html#newtype-deriving
[dac]: https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/derive_any_class.html#derive-any-class
[ds]: https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/deriving_strategies.html#deriving-strategies
[dds]: https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/deriving_strategies.html#default-deriving-strategy
