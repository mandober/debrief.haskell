Multi-parameter type classes {#multi-param-type-classes}
============================

::: {.extension shortdesc="Enable multi parameter type classes.
Implied by :extension:`FunctionalDependencies`." implies=":extension:`ConstrainedClassMethods`" since="6.8.1"}
MultiParamTypeClasses

Allow the definition of typeclasses with more than one parameter.
:::

Multi-parameter type classes are permitted, with extension
`MultiParamTypeClasses`{.interpreted-text role="extension"}. For
example: :

    class Collection c a where
        union :: c a -> c a -> c a
        ...etc.
