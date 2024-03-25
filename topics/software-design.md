# Software Design in Haskell

Software Design approaches
- Raw IO
- mtl
  - Final Tagless
- effect systems
  - algebraic effects
- ReaderT pattern
- Service Handle pattern
- Free Monads
  - Church Encoded Free Monads
  - Hierarchical Free Monads (HFM)
  - Freer Monads


Software Design requirements
- manage complexity
- separation of concerns
- testability
- reliablility
- maintainability
- bussiness requirements
- no reinventing the wheel


Practices, patterns, methodologies, approaches to Software Design in Haskell.

Until relatively recently (~ GHC 8.10), Haskell was lacking high-level approaches and ideas composed into a complete methodology of Software Design.

Pure functional programming languages such as Haskell manage side-effects using monads, and allow multiple effects to be combined using a stack of monad transformers. However, monad transformers can quickly become unwieldy when there are lots of effects to manage, leading to a temptation in larger programs to combine everything into one coarse-grained state and exception monad.

Monad transformers are an effective tool for structuring large Haskell apps. A simpler app using a REPL, for example, may have some global state and perform console I/O, and hence be built with an IO monad transformed into a state monad using the StateT transformer. However, there are some difficulties with building applications in this way. Two of the most important are that the order in which transformers are applied matters (that is, transformers do not commute easily), and that it is difficult to invoke a function which uses a subset of the transformers in the stack.
