# Functor class




> Functor class is for types that can be *covariantly* mapped over.

Mapping a structure means applying a function to all its elements, while preserving the structure itself. Therefore, a functor is *structure-preserving transformation*.

An intuitive understanding of a functor could be that it represents a container of sorts, along with the mapping functionality.

Another view is that `fmap` lifts a function of values (normal function) into a *function of values in a particular functorial context*.

Functor wants a type constructor that takes one type and not a concrete type.
