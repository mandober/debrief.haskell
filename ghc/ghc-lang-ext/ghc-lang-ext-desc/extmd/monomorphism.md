Switching off the Monomorphism Restriction {#monomorphism}
==========================================

::: {.extension shortdesc="Disable the monomorphism restriction."}
NoMonomorphismRestriction

default

:   on

since

:   6.8.1

Prevents the compiler from applying the monomorphism restriction to
bindings lacking explicit type signatures.
:::

Haskell\'s monomorphism restriction (see [Section
4.5.5](http://www.haskell.org/onlinereport/decls.html#sect4.5.5) of the
Haskell Report) can be completely switched off by
`NoMonomorphismRestriction`{.interpreted-text role="extension"}. Since
GHC 7.8.1, the monomorphism restriction is switched off by default in
GHCi\'s interactive options (see
`ghci-interactive-options`{.interpreted-text role="ref"}).
