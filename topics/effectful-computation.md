# Effectful computation

**Effectful computations** are computations that do not only (or necessarily) produce a result. They also do or can do something additional (such as logging, failing, or changing a mutable state) that somehow interacts with the context in which the computation takes place. These are called side effects of computation.

It is quite paradoxical. On the one hand, Haskell is called a purely functional programming language and emphasizes that its functions do not have side effects. On the other hand, Haskell has a more sophisticated infrastructure (in the form of libraries) to model, classify, and structure effectful computations than most impure programming languages. In some sense, effects can be seen as one of the main obsessions of the language.

How do we reconcile these two opposites? Well, with its infrastructure, Haskell acknowledges that side effects are undeniably convenient for practical and pragmatic programming. At the same time, it aims to harness their use with a structured, type-based approach that prevents unbridled and unmanageable proliferation.
