# 6.6.3. Stand-alone deriving declarations

6.6.3. Stand-alone deriving declarations

https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/standalone_deriving.html

`StandaloneDeriving`


# Stand-alone deriving declarations

6. Language extensions
» 6.6. Deriving mechanism »
» » 6.6.3. Stand-alone deriving declarations

::: {.extension shortdesc="Enable standalone deriving."}
StandaloneDeriving

since

:   6.8.1

Allow the use of stand-alone `deriving` declarations.
:::

GHC allows stand-alone `deriving` declarations, enabled by
`StandaloneDeriving`{.interpreted-text role="extension"}: :

    data Foo a = Bar a | Baz String

    deriving instance Eq a => Eq (Foo a)

The syntax is identical to that of an ordinary instance declaration
apart from (a) the keyword `deriving`, and (b) the absence of the
`where` part.

However, standalone deriving differs from a `deriving` clause in a
number of important ways:

-   The standalone deriving declaration does not need to be in the same
    module as the data type declaration. (But be aware of the dangers of
    orphan instances (`orphan-modules`{.interpreted-text role="ref"}).
-   In most cases, you must supply an explicit context (in the example
    the context is `(Eq a)`), exactly as you would in an ordinary
    instance declaration. (In contrast, in a `deriving` clause attached
    to a data type declaration, the context is inferred.)

    The exception to this rule is that the context of a standalone
    deriving declaration can infer its context when a single,
    extra-wildcards constraint is used as the context, such as in: :

        deriving instance _ => Eq (Foo a)

    This is essentially the same as if you had written `deriving Eq`
    after the declaration for `data Foo a`. Using this feature requires
    the use of `PartialTypeSignatures`{.interpreted-text
    role="extension"} (`partial-type-signatures`{.interpreted-text
    role="ref"}).

-   Unlike a `deriving` declaration attached to a `data` declaration,
    the instance can be more specific than the data type (assuming you
    also use `FlexibleInstances`{.interpreted-text role="extension"},
    `instance-rules`{.interpreted-text role="ref"}). Consider for
    example :

        data Foo a = Bar a | Baz String

        deriving instance Eq a => Eq (Foo [a])
        deriving instance Eq a => Eq (Foo (Maybe a))

    This will generate a derived instance for `(Foo [a])` and
    `(Foo (Maybe a))`, but other types such as `(Foo (Int,Bool))` will
    not be an instance of `Eq`.

-   Unlike a `deriving` declaration attached to a `data` declaration,
    GHC does not restrict the form of the data type. Instead, GHC simply
    generates the appropriate boilerplate code for the specified class,
    and typechecks it. If there is a type error, it is your problem.
    (GHC will show you the offending code if it has a type error.)

    The merit of this is that you can derive instances for GADTs and
    other exotic data types, providing only that the boilerplate code
    does indeed typecheck. For example: :

        data T a where
           T1 :: T Int
           T2 :: T Bool

        deriving instance Show (T a)

    In this example, you cannot say `... deriving( Show )` on the data
    type declaration for `T`, because `T` is a GADT, but you *can*
    generate the instance declaration using stand-alone deriving.

    The down-side is that, if the boilerplate code fails to typecheck,
    you will get an error message about that code, which you did not
    write. Whereas, with a `deriving` clause the side-conditions are
    necessarily more conservative, but any error message may be more
    comprehensible.

-   Under most circumstances, you cannot use standalone deriving to
    create an instance for a data type whose constructors are not all in
    scope. This is because the derived instance would generate code that
    uses the constructors behind the scenes, which would break
    abstraction.

    The one exception to this rule is `DeriveAnyClass`{.interpreted-text
    role="extension"}, since deriving an instance via
    `DeriveAnyClass`{.interpreted-text role="extension"} simply
    generates an empty instance declaration, which does not require the
    use of any constructors. See the [deriving any
    class](#derive-any-class) section for more details.

In other ways, however, a standalone deriving obeys the same rules as
ordinary deriving:

-   A `deriving instance` declaration must obey the same rules
    concerning form and termination as ordinary instance declarations,
    controlled by the same flags; see `instance-decls`{.interpreted-text
    role="ref"}.
-   The stand-alone syntax is generalised for newtypes in exactly the
    same way that ordinary `deriving` clauses are generalised
    (`newtype-deriving`{.interpreted-text role="ref"}). For example: :

        newtype Foo a = MkFoo (State Int a)

        deriving instance MonadState Int Foo

    GHC always treats the *last* parameter of the instance (`Foo` in
    this example) as the type whose instance is being derived.
