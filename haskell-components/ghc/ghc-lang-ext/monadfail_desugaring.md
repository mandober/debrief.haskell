New monadic failure desugaring mechanism {#monadfail-desugaring}
========================================

::: {.extension shortdesc="Enable monadfail desugaring."}
MonadFailDesugaring

since

:   8.0.1

Use the `MonadFail.fail` instead of the legacy `Monad.fail` function
when desugaring refutable patterns in `do` blocks.
:::

The `-XMonadFailDesugaring` extension switches the desugaring of
`do`-blocks to use `MonadFail.fail` instead of `Monad.fail`.

This extension is enabled by default since GHC 8.6.1, under the
[MonadFail Proposal
(MFP)](https://gitlab.haskell.org/haskell/prime/-/wikis/libraries/proposals/monad-fail).

This extension is temporary, and will be deprecated in a future release.
