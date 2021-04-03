# Dependent types

https://en.wikipedia.org/wiki/Dependent_type
https://www.idris-lang.org/
https://agda.readthedocs.io/en/latest/
https://en.wikipedia.org/wiki/Refinement_type
https://en.wikipedia.org/wiki/Substructural_type_system
https://en.wikipedia.org/wiki/Recursive_data_type
https://en.wikipedia.org/wiki/Walther_recursion
https://en.wikipedia.org/wiki/Turing_completeness
https://en.wikipedia.org/wiki/Machine_that_always_halts
https://en.wikipedia.org/wiki/Total_functional_programming
https://en.wikipedia.org/wiki/Category:Type_systems

## Dependent Types in Haskell
(book "Thinking with Types")

Dependent types are types which depend on the result of runtime values. Normally, terms exist at runtime, but types do not - they are not around once the compile-time is through.

Dependent types in Haskell can be approximated via singletons

**Singletons** are the types with a single inhabitant, e.g. `() :: ()`. They can be understood as an isomorphism between terms and values because of their invariant: if you know the type, you know the term and vice versa. Due to this 1:1 representation, we can think about singletons as being able to cross the term-type divide at will.

The *Singletons* concept (and package) is taking this idea to the extreme; for every inhabitant of a type, we create a singleton type capable of bridging the term-type divide. As a result, we are capable of moving types to terms, using them in regular term-level Haskell computations, and then lifting them back into types.

The `DataKinds` pragma gives us the type-level representation of terms. When enabled, it lifts *data-ctors to type-ctors* and *types to kinds*.

The datatype `Bool` has the nullary type-ctor `Bool` and two nullary data-ctors, `True` and `False`, that is, `data Bool = False | True`. With *-XDataKinds* enabled, both data-ctors become type-ctors of kind `Bool`:
- `True` data-ctor becomes `'True` type-ctor of kind `Bool`, `'True :: Bool`
- `False` data-ctor becomes `'False` type-ctor of kind `Bool`, `'False :: Bool`
- type ctor `Bool` becomes kind `Bool`
