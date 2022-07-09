Data types with no constructors {#nullary-types}
===============================

::: {.extension shortdesc="Allow definition of empty ``data`` types."}
EmptyDataDecls

since

:   6.8.1

Allow definition of empty `data` types.
:::

With the `EmptyDataDecls`{.interpreted-text role="extension"} extension,
GHC lets you declare a data type with no constructors.

You only need to enable this extension if the language you\'re using is
Haskell 98, in which a data type must have at least one constructor.
Haskell 2010 relaxed this rule to allow data types with no constructors,
and thus `EmptyDataDecls`{.interpreted-text role="extension"} is enabled
by default when the language is Haskell 2010.

For example: :

    data S      -- S :: Type
    data T a    -- T :: Type -> Type

Syntactically, the declaration lacks the \"= constrs\" part. The type
can be parameterised over types of any kind, but if the kind is not
`Type` then an explicit kind annotation must be used (see
`kinding`{.interpreted-text role="ref"}).

Such data types have only one value, namely bottom. Nevertheless, they
can be useful when defining \"phantom types\".

In conjunction with the `EmptyDataDeriving`{.interpreted-text
role="extension"} extension, empty data declarations can also derive
instances of standard type classes (see
`empty-data-deriving`{.interpreted-text role="ref"}).
