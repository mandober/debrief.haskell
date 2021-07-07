Empty case alternatives {#empty-case}
=======================

::: {.extension shortdesc="Allow empty case alternatives."}
EmptyCase

since

:   7.8.1

Allow empty case expressions.
:::

The `EmptyCase`{.interpreted-text role="extension"} extension enables
case expressions, or lambda-case expressions, that have no alternatives,
thus: :

    case e of { }   -- No alternatives

or :

    \case { }       -- -XLambdaCase is also required

This can be useful when you know that the expression being scrutinised
has no non-bottom values. For example:

    data Void
    f :: Void -> Int
    f x = case x of { }

With dependently-typed features it is more useful (see
`2431`{.interpreted-text role="ghc-ticket"}). For example, consider
these two candidate definitions of `absurd`:

    data a :~: b where
      Refl :: a :~: a

    absurd :: True :~: False -> a
    absurd x = error "absurd"    -- (A)
    absurd x = case x of {}      -- (B)

We much prefer (B). Why? Because GHC can figure out that
`(True :~: False)` is an empty type. So (B) has no partiality and GHC is
able to compile with `-Wincomplete-patterns`{.interpreted-text
role="ghc-flag"} and `-Werror`{.interpreted-text role="ghc-flag"}. On
the other hand (A) looks dangerous, and GHC doesn\'t check to make sure
that, in fact, the function can never get called.
