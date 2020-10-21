# Monad Transformers Explained

https://wiki.haskell.org/Monad_Transformers_Tutorial

Adam McCullough - Monad Transformers for the Easily Confused - λC 2018
video: https://www.youtube.com/watch?v=SMj-n2f7wYY
code : https://github.com/TheWizardTower/monadTransformers


# Monad Transformers Explained

https://wiki.haskell.org/Monad_Transformers_Explained

Monad Transformers are about composition of monads - using characteristic properties of different monads at once, in the same monadic transformer. For example, using reader monad with IO monad. It's about making a monad that merges the properties and powers of other monads.

A monad transformer wraps a (innermost) monad, which is usually `Identity` or `IO` monad but it can be any other monad. `Identity` has no real powers, so all the needed "powers" need to be added; but the `IO` monad provides basic dealing with input and output, providing a good starting point for enhancemnt by e.g. adding exceptions, coroutines, continuations, etc.

The package *mtl* (monad transformer library) provides many useful functions for dealing with monad transformers.



## Monad Transformers for the Easily Confused

Adam McCullough - Monad Transformers for the Easily Confused - λC 2018
video: https://www.youtube.com/watch?v=SMj-n2f7wYY
code : https://github.com/TheWizardTower/monadTransformers

Monad transformers transform monads! ...but how? We're going to review Monoid, Functor, Applicative, and Monad laws, then see if we can figure out a way to get to what MTs do for us.
