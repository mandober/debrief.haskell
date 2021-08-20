Explicit universal quantification (forall) {#explicit-foralls}
==========================================

::: {.extension shortdesc="Enable explicit universal quantification.
Implied by :extension:`ScopedTypeVariables`, :extension:`LiberalTypeSynonyms`,
:extension:`RankNTypes` and :extension:`ExistentialQuantification`." since="6.12.1"}
ExplicitForAll

Allow use of the `forall` keyword in places where universal
quantification is implicit.
:::

Haskell type signatures are implicitly quantified. When the language
option `ExplicitForAll`{.interpreted-text role="extension"} is used, the
keyword `forall` allows us to say exactly what this means. For example:
:

    g :: b -> b

means this: :

    g :: forall b. (b -> b)

The two are treated identically, except that the latter may bring type
variables into scope (see `scoped-type-variables`{.interpreted-text
role="ref"}).

This extension also enables explicit quantification of type and kind
variables in `data-instance-declarations`{.interpreted-text role="ref"},
`type-instance-declarations`{.interpreted-text role="ref"},
`closed-type-families`{.interpreted-text role="ref"},
`assoc-inst`{.interpreted-text role="ref"}, and
`rewrite-rules`{.interpreted-text role="ref"}.

Notes:

-   As well as in type signatures, you can also use an explicit `forall`
    in an instance declaration: :

        instance forall a. Eq a => Eq [a] where ...

    Note that the use of `forall`s in instance declarations is somewhat
    restricted in comparison to other types. For example, instance
    declarations are not allowed to contain nested `forall`s. See
    `formal-instance-syntax`{.interpreted-text role="ref"} for more
    information.

-   If the `-Wunused-foralls`{.interpreted-text role="ghc-flag"} flag is
    enabled, a warning will be emitted when you write a type variable in
    an explicit `forall` statement that is otherwise unused. For
    instance: :

        g :: forall a b. (b -> b)

    would warn about the unused type variable [a]{.title-ref}.

The `forall`-or-nothing rule {#forall-or-nothing}
----------------------------

In certain forms of types, type variables obey what is known as the
\"`forall`-or-nothing\" rule: if a type has an outermost, explicit
`forall`, then all of the type variables in the type must be explicitly
quantified. These two examples illustrate how the rule works: :

    f  :: forall a b. a -> b -> b         -- OK, `a` and `b` are explicitly bound
    g  :: forall a. a -> forall b. b -> b -- OK, `a` and `b` are explicitly bound
    h  :: forall a. a -> b -> b           -- Rejected, `b` is not in scope

The type signatures for `f`, `g`, and `h` all begin with an outermost
`forall`, so every type variable in these signatures must be explicitly
bound by a `forall`. Both `f` and `g` obey the `forall`-or-nothing rule,
since they explicitly quantify `a` and `b`. On the other hand, `h` does
not explicitly quantify `b`, so GHC will reject its type signature for
being improperly scoped.

In places where the `forall`-or-nothing rule takes effect, if a type
does *not* have an outermost `forall`, then any type variables that are
not explicitly bound by a `forall` become implicitly quantified. For
example: :

    i :: a -> b -> b             -- `a` and `b` are implicitly quantified
    j :: a -> forall b. b -> b   -- `a` is implicitly quantified
    k :: (forall a. a -> b -> b) -- `b` is implicitly quantified

GHC will accept `i`, `j`, and `k`\'s type signatures. Note that:

-   `j`\'s signature is accepted despite its mixture of implicit and
    explicit quantification. As long as a `forall` is not an outermost
    one, it is fine to use it among implicitly bound type variables.
-   `k`\'s signature is accepted because the outermost parentheses imply
    that the `forall` is not an outermost `forall`. The
    `forall`-or-nothing rule is one of the few places in GHC where the
    presence or absence of parentheses can be semantically significant!

The `forall`-or-nothing rule takes effect in the following places:

-   Type signature declarations for functions, values, and class methods
-   Expression type annotations
-   Instance declarations
-   `class-default-signatures`{.interpreted-text role="ref"}
-   Type signatures in a `specialize-pragma`{.interpreted-text
    role="ref"} or `specialize-instance-pragma`{.interpreted-text
    role="ref"}
-   `standalone-kind-signatures`{.interpreted-text role="ref"}
-   Type signatures for `gadt`{.interpreted-text role="ref"}
    constructors
-   Type signatures for `pattern-synonyms`{.interpreted-text role="ref"}
-   `data-instance-declarations`{.interpreted-text role="ref"},
    `type-instance-declarations`{.interpreted-text role="ref"},
    `closed-type-families`{.interpreted-text role="ref"}, and
    `assoc-inst`{.interpreted-text role="ref"}
-   `rewrite-rules`{.interpreted-text role="ref"} in which the type
    variables are explicitly quantified

Notes:

-   `pattern-type-sigs`{.interpreted-text role="ref"} are a notable
    example of a place where types do *not* obey the `forall`-or-nothing
    rule. For example, GHC will accept the following: :

        f (g :: forall a. a -> b) x = g x :: b

    Furthermore, `rewrite-rules`{.interpreted-text role="ref"} do not
    obey the `forall`-or-nothing rule when their type variables are not
    explicitly quantified: :

        {-# RULES "f" forall (g :: forall a. a -> b) x. f g x = g x :: b #-}

-   GADT constructors are extra particular about their `forall`s. In
    addition to adhering to the `forall`-or-nothing rule, GADT
    constructors also forbid nested `forall`s. For example, GHC would
    reject the following GADT: :

        data T where
          MkT :: (forall a. a -> b -> T)

    Because of the lack of an outermost `forall` in the type of `MkT`,
    the `b` would be implicitly quantified. In effect, it would be as if
    one had written `MkT :: forall b. (forall a. a -> b -> T)`, which
    contains nested `forall`s. See
    `formal-gadt-syntax`{.interpreted-text role="ref"}.
