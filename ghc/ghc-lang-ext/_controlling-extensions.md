Controlling extensions {#options-language}
======================

::: {.index}
single: language; option single: options; language single: extensions;
options controlling
:::

Language extensions can be controlled (i.e. allowed or not) in two ways:

-   Every language extension can be switched on by a command-line flag
    \"`-X...`\" (e.g. `-XTemplateHaskell`), and switched off by the flag
    \"`-XNo...`\"; (e.g. `-XNoTemplateHaskell`).
-   Language extensions can also be enabled using the `LANGUAGE` pragma,
    thus `{-# LANGUAGE TemplateHaskell #-}` (see
    `language-pragma`{.interpreted-text role="ref"}).

::: {.extension shortdesc="Use the Haskell 2010 language variant."}
Haskell2010

Compile Haskell 2010 language variant. Enables the following language
extensions:

::: {.hlist}
-   `ImplicitPrelude`{.interpreted-text role="extension"}
-   `StarIsType`{.interpreted-text role="extension"}
-   `CUSKs`{.interpreted-text role="extension"}
-   `MonomorphismRestriction`{.interpreted-text role="extension"}
-   `DatatypeContexts`{.interpreted-text role="extension"}
-   `TraditionalRecordSyntax`{.interpreted-text role="extension"}
-   `EmptyDataDecls`{.interpreted-text role="extension"}
-   `ForeignFunctionInterface`{.interpreted-text role="extension"}
-   `PatternGuards`{.interpreted-text role="extension"}
-   `DoAndIfThenElse`{.interpreted-text role="extension"}
-   `RelaxedPolyRec`{.interpreted-text role="extension"}
:::
:::

::: {.extension shortdesc="Use the Haskell 98 language variant."}
Haskell98

Compile using Haskell 98 language variant. Enables the following
language extensions:

::: {.hlist}
-   `ImplicitPrelude`{.interpreted-text role="extension"}
-   `StarIsType`{.interpreted-text role="extension"}
-   `CUSKs`{.interpreted-text role="extension"}
-   `MonomorphismRestriction`{.interpreted-text role="extension"}
-   `NPlusKPatterns`{.interpreted-text role="extension"}
-   `DatatypeContexts`{.interpreted-text role="extension"}
-   `TraditionalRecordSyntax`{.interpreted-text role="extension"}
-   `NondecreasingIndentation`{.interpreted-text role="extension"}
:::
:::

Although not recommended, the deprecated
`-fglasgow-exts`{.interpreted-text role="ghc-flag"} flag enables a large
swath of the extensions supported by GHC at once.

::: {.ghc-flag shortdesc="Deprecated. Enable most language extensions;
see :ref:`options-language` for exactly which ones." type="dynamic" reverse="-fno-glasgow-exts" category="misc"}
-fglasgow-exts

The flag `-fglasgow-exts` is equivalent to enabling the following
extensions:

::: {.hlist}
-   `ConstrainedClassMethods`{.interpreted-text role="extension"}
-   `DeriveDataTypeable`{.interpreted-text role="extension"}
-   `DeriveFoldable`{.interpreted-text role="extension"}
-   `DeriveFunctor`{.interpreted-text role="extension"}
-   `DeriveGeneric`{.interpreted-text role="extension"}
-   `DeriveTraversable`{.interpreted-text role="extension"}
-   `EmptyDataDecls`{.interpreted-text role="extension"}
-   `ExistentialQuantification`{.interpreted-text role="extension"}
-   `ExplicitNamespaces`{.interpreted-text role="extension"}
-   `FlexibleContexts`{.interpreted-text role="extension"}
-   `FlexibleInstances`{.interpreted-text role="extension"}
-   `ForeignFunctionInterface`{.interpreted-text role="extension"}
-   `FunctionalDependencies`{.interpreted-text role="extension"}
-   `GeneralizedNewtypeDeriving`{.interpreted-text role="extension"}
-   `ImplicitParams`{.interpreted-text role="extension"}
-   `InterruptibleFFI`{.interpreted-text role="extension"}
-   `KindSignatures`{.interpreted-text role="extension"}
-   `LiberalTypeSynonyms`{.interpreted-text role="extension"}
-   `MagicHash`{.interpreted-text role="extension"}
-   `MultiParamTypeClasses`{.interpreted-text role="extension"}
-   `ParallelListComp`{.interpreted-text role="extension"}
-   `PatternGuards`{.interpreted-text role="extension"}
-   `PostfixOperators`{.interpreted-text role="extension"}
-   `RankNTypes`{.interpreted-text role="extension"}
-   `RecursiveDo`{.interpreted-text role="extension"}
-   `ScopedTypeVariables`{.interpreted-text role="extension"}
-   `StandaloneDeriving`{.interpreted-text role="extension"}
-   `TypeOperators`{.interpreted-text role="extension"}
-   `TypeSynonymInstances`{.interpreted-text role="extension"}
-   `UnboxedTuples`{.interpreted-text role="extension"}
-   `UnicodeSyntax`{.interpreted-text role="extension"}
-   `UnliftedFFITypes`{.interpreted-text role="extension"}
:::

Enabling these options is the *only* effect of `-fglasgow-exts`. We are
trying to move away from this portmanteau flag, and towards enabling
features individually.
:::
