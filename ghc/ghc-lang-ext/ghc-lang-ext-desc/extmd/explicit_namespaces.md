Explicit namespaces in import/export {#explicit-namespaces}
====================================

::: {.extension shortdesc="Enable using the keyword ``type`` to specify the namespace of
entries in imports and exports (:ref:`explicit-namespaces`).
Implied by :extension:`TypeOperators` and :extension:`TypeFamilies`." since="7.6.1"}
ExplicitNamespaces

Enable use of explicit namespaces in module export lists.
:::

In an import or export list, such as :

    module M( f, (++) ) where ...
      import N( f, (++) )
      ...

the entities `f` and `(++)` are *values*. However, with type operators
(`type-operators`{.interpreted-text role="ref"}) it becomes possible to
declare `(++)` as a *type constructor*. In that case, how would you
export or import it?

The `ExplicitNamespaces`{.interpreted-text role="extension"} extension
allows you to prefix the name of a type constructor in an import or
export list with \"`type`\" to disambiguate this case, thus: :

    module M( f, type (++) ) where ...
      import N( f, type (++) )
      ...
    module N( f, type (++) ) where
      data family a ++ b = L a | R b

The extension `ExplicitNamespaces`{.interpreted-text role="extension"}
is implied by `TypeOperators`{.interpreted-text role="extension"} and
(for some reason) by `TypeFamilies`{.interpreted-text role="extension"}.

In addition, with `PatternSynonyms`{.interpreted-text role="extension"}
you can prefix the name of a data constructor in an import or export
list with the keyword `pattern`, to allow the import or export of a data
constructor without its parent type constructor (see
`patsyn-impexp`{.interpreted-text role="ref"}).
