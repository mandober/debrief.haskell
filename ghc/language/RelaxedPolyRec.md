# RelaxedPolyRec

- Lang Pragma: `RelaxedPolyRec`
- (undocumented)

https://gitlab.haskell.org/ghc/ghc/-/issues/11691

Documentation indicates `RelaxedPolyRec` is optional

The documentation in "Other Type System Extensions" says "If `RelaxedPolyRec` is specified ..." and "With `RelaxedPolyRec` ..." and "This flag implies `RelaxedPolyRec`". 

There may be other references elsewhere. In fact, RelaxedPolyRec has been not only the default but in fact impossible to turn off since at least GHC 7.6.3.

The documentation should probably stop mentioning the (long-meaningless) flag, and simply state that GHC uses Jones's extension instead.

The Haskell Report section cited regarding contexts in explicit signatures for declaration groups is so vague that I'm not sure it's even worth mentioning that GHC relaxes it.
