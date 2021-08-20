Fractional looking integer literals {#num-decimals}
===================================

::: {.extension shortdesc="Enable support for 'fractional' integer literals."}
NumDecimals

since

:   7.8.1

Allow the use of floating-point literal syntax for integral types.
:::

Haskell 2010 and Haskell 98 define floating literals with the syntax
`1.2e6`. These literals have the type `Fractional a => a`.

The language extension `NumDecimals`{.interpreted-text role="extension"}
allows you to also use the floating literal syntax for instances of
`Integral`, and have values like `(1.2e6 :: Num a => a)`
