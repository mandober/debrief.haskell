# Fuzzling

1. (verb, intransitive, Haskell)
  To solve the implementation, that is, to write out a valid definition of a function guided by its given type signature.

2. (verb, transitive, obsolete)
  to make drunk; to confuse; to befuddle.


## Fuzzling in Haskell

Fuzzling is to be the name of the act that is, at a minimum (i.e. in the least) a form of Haskell coding kata: Hackage > search for `mtl` package > locate the page about `ContT` monad transformer > observe the implementation of `ContT`'s instance for `Applicative` > note the type signature > try to implement the function for yourself relying (only) on the type signature.

Knowing (or maybe not) that a valid implementation exists, and moreover, that the implementation uses nothing strange (like summing an unexpected force that appears out of nowhere and solves the whole thing, i.e. deusex function, that is, dualsexyeahparty) is to be called *fuzzling* or, every so often, just to shake things up, *finkelling*.
