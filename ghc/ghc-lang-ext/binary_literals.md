Binary integer literals {#binary-literals}
=======================

::: {.extension shortdesc="Enable support for binary literals."}
BinaryLiterals

since

:   7.10.1

Allow the use of binary notation in integer literals.
:::

Haskell 2010 and Haskell 98 allows for integer literals to be given in
decimal, octal (prefixed by `0o` or `0O`), or hexadecimal notation
(prefixed by `0x` or `0X`).

The language extension `BinaryLiterals`{.interpreted-text
role="extension"} adds support for expressing integer literals in binary
notation with the prefix `0b` or `0B`. For instance, the binary integer
literal `0b11001001` will be desugared into `fromInteger 201` when
`BinaryLiterals`{.interpreted-text role="extension"} is enabled.
