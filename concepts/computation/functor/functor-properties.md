# Functor properties

- The characteristic property of functors is that they are mappings that preserve structure.

- In CT, a structure-preserving mapping means that the identity morphisms and composition are preserved.

- In Haskell, a structure-preserving mapping implies that the type of the structure's elements may change - both change value and/or change type - but the structure must remain the same, meaning its type doesn't change (the type ctor `f`, whatever it is, remains the same); this is also reflected in the notion of "shape", characterized by the type and the size of the structure (the number of elements) and the requirement that they both remain the same.

- A functor may change the type of the structure's elements, but it must not change the type of the structure itself - the "size" of the structure must remain the same; in Haskell, the type ctor `f` remain the same.
