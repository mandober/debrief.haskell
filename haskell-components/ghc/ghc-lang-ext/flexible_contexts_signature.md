The context of a type signature {#flexible-contexts}
===============================

The `FlexibleContexts`{.interpreted-text role="extension"} extension
lifts the Haskell 98 restriction that the type-class constraints in a
type signature must have the form *(class type-variable)* or *(class
(type-variable type1 type2 \... typen))*. With
`FlexibleContexts`{.interpreted-text role="extension"} these type
signatures are perfectly okay :

    g :: Eq [a] => ...
    g :: Ord (T a ()) => ...

The flag `FlexibleContexts`{.interpreted-text role="extension"} also
lifts the corresponding restriction on class declarations
(`superclass-rules`{.interpreted-text role="ref"}) and instance
declarations (`instance-rules`{.interpreted-text role="ref"}).
