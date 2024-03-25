Negative literals
=================

::: {.extension shortdesc="Enable support for negative literals."}
NegativeLiterals

since

:   7.8.1

Enable negative numeric literals.
:::

The literal `-123` is, according to Haskell98 and Haskell 2010, two
tokens, a unary minus (`-`) and the number 123, and is desugared as
`negate (fromInteger 123)`. The language extension
`NegativeLiterals`{.interpreted-text role="extension"} causes it to be
treated as a single token and desugared as `fromInteger (-123)`.

This can be useful when the positive and negative range of a numeric
data type don\'t match up. For example, in 8-bit arithmetic -128 is
representable, but +128 is not. So `negate (fromInteger 128)` will
elicit an unexpected integer-literal-overflow message.

Whitespace can be inserted, as in `- 123`, to force interpretation as
two tokens.

In 9.0, the behavior of this extension changed, and now we require that
a negative literal must not be preceded by a closing token (see [GHC
Proposal
\#229](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0229-whitespace-bang-patterns.rst)
for the definition of a closing token). In other words, we parse
`f -123` as `f (-123)`, but `x-123` as `(-) x 123`. Before this
amendment, `NegativeLiterals`{.interpreted-text role="extension"} caused
`x-123` to be parsed as `x(-123)`.

`NegativeLiterals`{.interpreted-text role="extension"} is a subset of
`LexicalNegation`{.interpreted-text role="extension"}. That is, enabling
both of those extensions has the same effect as enabling
`LexicalNegation`{.interpreted-text role="extension"} alone.
