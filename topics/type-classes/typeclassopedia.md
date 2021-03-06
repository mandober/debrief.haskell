# Typeclassopedia


![classes](https://wiki.haskell.org/wikiupload/d/df/Typeclassopedia-diagram.png)

https://wiki.haskell.org/Typeclassopedia


* Solid arrows point from the general to the specific; e.g. the arrow from `Functor` to `Applicative` means that every `Applicative` is a `Functor`.
* Dotted lines indicate some other sort of relationship.
* `Monad` and `ArrowApply` are equivalent.
* `Apply` and `Comonad` are greyed out since they're not in the std.



1. *Functor*
  - Definition
  - Instances
  - Laws
  - Intuition
  - Utility functions
2. *Applicative*
  - Definition
  - Laws
  - Instances
  - Intuition
  - Utility functions
  - Alternative formulation
3. *Monad*
  - Definition
  - Instances
  - Intuition
  - Utility functions
  - Laws
  - do notation
4. *MonadFail*
  - Definition
  - Law
5. *Monad transformers*
  - Standard monad transformers
  - Definition and laws
  - Transformer type classes and "capability" style
  - Composing monads
6. *MonadFix*
  - do rec notation
  - Examples and intuition
  - mdo syntax
7. *Semigroup*
  - Definition
  - Laws
8. *Monoid*
  - Definition
  - Laws
  - Instances
9. Failure and choice: *Alternative*, *MonadPlus*, *ArrowPlus*
  - Definition
  - Instances
  - Laws
  - Utility functions
10. *Foldable*
  - Definition
  - Instances and examples
  - Derived folds
  - Utility functions
  - Foldable actually isn't
11. *Traversable*
  - Definition
  - Intuition
  - Instances and examples
  - Laws
12. *Bifunctor*
  - Definition
  - Laws
  - Instances
13. *Category*
  - Definition
14. *Arrow*
  - Definition
  - Intuition
  - Instances
  - Laws
  - ArrowChoice
  - ArrowApply
  - ArrowLoop
  - Arrow notation
15. *Comonad*
  - Definition



## References

- https://wiki.haskell.org/Typeclassopedia
- https://wiki.haskell.org/Instances_of_List_and_Maybe
