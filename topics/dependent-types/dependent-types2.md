# Dependent Types

Dependent types are types which depend on the result of runtime values. Terms and types are on the different side of the phase barrier: normally, only terms exist at runtime because their types have already undegone the *type erasure* process (the types have already fulfilled their purpose and are not needed at run-time).

Dependent types can be approximated via *singletons*, which are types with only a single inhabitant value (besides ⟘). Their essential property is the isomorphism between the type and value, if you know one, you can easily deduce the other.

*Promotion of data ctors* via `DataKinds` will give us type-level representation for, e.g., Booleans by lifting the type ctor `Bool` into kinds (so we gain the new kind `Bool`), and its two data ctors `True` and `False` are lifted to the type level becoming `'True` and `'False` type ctors, both uninhabited and both of the kind `Bool`.

However, these data ctors promoted to type ctors are just types, and types are not around at runtime. This is why we still need singletons - although they go through type erasure as well, we can easily recover their types nevertheless.
