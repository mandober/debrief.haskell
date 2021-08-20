Deriving mechanism {#deriving}
==================

Haskell 98 allows the programmer to add a deriving clause to a data type
declaration, to generate a standard instance declaration for specified
class. GHC extends this mechanism along several axes:

-   The derivation mechanism can be used separately from the data type
    declaration, using the `standalone deriving mechanism
    <stand-alone-deriving>`{.interpreted-text role="ref"}.
-   In Haskell 98, the only derivable classes are `Eq`, `Ord`, `Enum`,
    `Ix`, `Bounded`, `Read`, and `Show`. `Various
    language extensions <deriving-extra>`{.interpreted-text role="ref"}
    extend this list.
-   Besides the stock approach to deriving instances by generating all
    method definitions, GHC supports two additional deriving strategies,
    which can derive arbitrary classes:

    -   `Generalised newtype deriving <newtype-deriving>`{.interpreted-text
        role="ref"} for newtypes and
    -   `deriving any class <derive-any-class>`{.interpreted-text
        role="ref"} using an empty instance declaration.

    The user can optionally declare the desired `deriving strategy
    <deriving-strategies>`{.interpreted-text role="ref"}, especially if
    the compiler chooses the wrong one
    `by default <default-deriving-strategy>`{.interpreted-text
    role="ref"}.

::: {.toctree maxdepth="1"}
empty\_data\_deriving deriving\_inferred standalone\_deriving
deriving\_extra newtype\_deriving derive\_any\_class
deriving\_strategies deriving\_via
:::
k
