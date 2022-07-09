Overloaded labels
=================

::: {.extension shortdesc="Enable overloaded labels."}
OverloadedLabels

since

:   8.0.1

Enable use of the `#foo` overloaded label syntax.
:::

GHC supports *overloaded labels*, a form of identifier whose
interpretation may depend both on its type and on its literal text. When
the `OverloadedLabels`{.interpreted-text role="extension"} extension is
enabled, an overloaded label can be written with a prefix hash, for
example `#foo`. The type of this expression is `IsLabel "foo" a => a`.

The class `IsLabel` is defined as:

    class IsLabel (x :: Symbol) a where
      fromLabel :: a

This is rather similar to the class `IsString` (see
`overloaded-strings`{.interpreted-text role="ref"}), but with an
additional type parameter that makes the text of the label available as
a type-level string (see `type-level-literals`{.interpreted-text
role="ref"}). Note that `fromLabel` had an extra `Proxy# x` argument in
GHC 8.0, but this was removed in GHC 8.2 as a type application (see
`visible-type-application`{.interpreted-text role="ref"}) can be used
instead.

There are no predefined instances of this class. It is not in scope by
default, but can be brought into scope by importing
`GHC.OverloadedLabels.`{.interpreted-text role="base-ref"}. Unlike
`IsString`, there are no special defaulting rules for `IsLabel`.

During typechecking, GHC will replace an occurrence of an overloaded
label like `#foo` with `fromLabel @"foo"`. This will have some type
`alpha` and require the solution of a class constraint
`IsLabel "foo" alpha`.

The intention is for `IsLabel` to be used to support overloaded record
fields and perhaps anonymous records. Thus, it may be given instances
for base datatypes (in particular `(->)`) in the future.

If `RebindableSyntax`{.interpreted-text role="extension"} is enabled,
overloaded labels will be desugared using whatever `fromLabel` function
is in scope, rather than always using `GHC.OverloadedLabels.fromLabel`.

When writing an overloaded label, there must be no space between the
hash sign and the following identifier. The
`MagicHash`{.interpreted-text role="extension"} extension makes use of
postfix hash signs; if `OverloadedLabels`{.interpreted-text
role="extension"} and `MagicHash`{.interpreted-text role="extension"}
are both enabled then `x#y` means `x# y`, but if only
`OverloadedLabels`{.interpreted-text role="extension"} is enabled then
it means `x #y`. The `UnboxedTuples`{.interpreted-text role="extension"}
extension makes `(#` a single lexeme, so when
`UnboxedTuples`{.interpreted-text role="extension"} is enabled you must
write a space between an opening parenthesis and an overloaded label. To
avoid confusion, you are strongly encouraged to put a space before the
hash when using `OverloadedLabels`{.interpreted-text role="extension"}.

When using `OverloadedLabels`{.interpreted-text role="extension"} (or
other extensions that make use of hash signs) in a `.hsc` file (see
`hsc2hs`{.interpreted-text role="ref"}), the hash signs must be doubled
(write `##foo` instead of `#foo`) to avoid them being treated as
`hsc2hs` directives.

Here is an extension of the record access example in
`type-level-literals`{.interpreted-text role="ref"} showing how an
overloaded label can be used as a record selector:

    {-# LANGUAGE DataKinds, KindSignatures, MultiParamTypeClasses,
                 FunctionalDependencies, FlexibleInstances,
                 OverloadedLabels, ScopedTypeVariables #-}

    import GHC.OverloadedLabels (IsLabel(..))
    import GHC.TypeLits (Symbol)

    data Label (l :: Symbol) = Get

    class Has a l b | a l -> b where
      from :: a -> Label l -> b

    data Point = Point Int Int deriving Show

    instance Has Point "x" Int where from (Point x _) _ = x
    instance Has Point "y" Int where from (Point _ y) _ = y

    instance Has a l b => IsLabel l (a -> b) where
      fromLabel x = from x (Get :: Label l)

    example = #x (Point 1 2)
