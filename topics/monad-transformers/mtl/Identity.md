# Identity

`Control.Monad.Identity` from `mtl` package

https://hackage.haskell.org/package/mtl-2.3.1/docs/Control-Monad-Identity.html

- Computation type: simple function application
- Binding strategy: the bound function is applied to input value,   
  `Identity x >>= f == f x`
- Useful for: monads can be derived from MTs applied to the `Identity` monad
- Zero and plus: none
- Example type `Identity a`

The Identity monad is a monad that does not embody any computational strategy. It simply applies the bound function to its input without any modification. Computationally, there is no reason to use the Identity monad instead of the much simpler act of simply applying functions to their arguments. The purpose of the Identity monad is its fundamental role in the theory of monad transformers. Any monad transformer applied to the Identity monad yields a non-transformer version of that monad.
