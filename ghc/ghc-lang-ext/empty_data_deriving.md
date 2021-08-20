Deriving instances for empty data types {#empty-data-deriving}
=======================================

::: {.extension shortdesc="Allow deriving instances of standard type classes for
empty data types." since="8.4.1"}
EmptyDataDeriving

Allow deriving instances of standard type classes for empty data types.
:::

One can write data types with no constructors using the
`EmptyDataDecls`{.interpreted-text role="extension"} flag (see
`nullary-types`{.interpreted-text role="ref"}), which is on by default
in Haskell 2010. What is not on by default is the ability to derive type
class instances for these types. This ability is enabled through use of
the `EmptyDataDeriving`{.interpreted-text role="extension"} flag. For
instance, this lets one write: :

    data Empty deriving (Eq, Ord, Read, Show)

This would generate the following instances: :

    instance Eq Empty where
      _ == _ = True

    instance Ord Empty where
      compare _ _ = EQ

    instance Read Empty where
      readPrec = pfail

    instance Show Empty where
      showsPrec _ x = case x of {}

The `EmptyDataDeriving`{.interpreted-text role="extension"} flag is only
required to enable deriving of these four \"standard\" type classes
(which are mentioned in the Haskell Report). Other extensions to the
`deriving` mechanism, which are explained below in greater detail, do
not require `EmptyDataDeriving`{.interpreted-text role="extension"} to
be used in conjunction with empty data types. These include:

-   `StandaloneDeriving`{.interpreted-text role="extension"} (see
    `stand-alone-deriving`{.interpreted-text role="ref"})
-   Type classes which require their own extensions to be enabled to be
    derived, such as `DeriveFunctor`{.interpreted-text role="extension"}
    (see `deriving-extra`{.interpreted-text role="ref"})
-   `DeriveAnyClass`{.interpreted-text role="extension"} (see
    `derive-any-class`{.interpreted-text role="ref"})
