Safe imports {#safe-imports-ext}
============

::: {.extension shortdesc="Enable the :ref:`Safe Haskell <safe-haskell>` Safe mode." noindex=""}
Safe

since

:   7.2.1

Declare the Safe Haskell state of the current module.
:::

::: {.extension shortdesc="Enable the :ref:`Safe Haskell <safe-haskell>` Trustworthy mode." noindex=""}
Trustworthy

since

:   7.2.1

Declare the Safe Haskell state of the current module.
:::

::: {.extension shortdesc="Enable Safe Haskell Unsafe mode." noindex=""}
Unsafe

since

:   7.4.1

Declare the Safe Haskell state of the current module.
:::

With the `Safe`{.interpreted-text role="extension"},
`Trustworthy`{.interpreted-text role="extension"} and
`Unsafe`{.interpreted-text role="extension"} language flags, GHC extends
the import declaration syntax to take an optional `safe` keyword after
the `import` keyword. This feature is part of the Safe Haskell GHC
extension. For example: :

    import safe qualified Network.Socket as NS

would import the module `Network.Socket` with compilation only
succeeding if `Network.Socket` can be safely imported. For a description
of when a import is considered safe see `safe-haskell`{.interpreted-text
role="ref"}.
