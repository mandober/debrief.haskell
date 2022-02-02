# 8.2 Contexts

For a given Haskell system, we define the **Haskell context** to be the *execution context of the abstract machine* on which the Haskell system is based. This includes the heap, stacks, and the registers of the abstract machine and their mapping onto a concrete architecture. We call any other execution context an *external context*.

Generally, we cannot assume any compatibility between the data formats and calling conventions between the Haskell context and a given external context, except where Haskell explicitly prescribes a specific data format.

The principal goal of FFI is to provide a programmable interface between the Haskell context and external contexts.

As a result, Haskell threads can access data in external contexts and invoke functions that are executed in an external context, and vice versa.

External contexts are usually identified by a calling convention.

## 8.2.1 Cross Language Type Consistency

Given that many external languages support static types, the question arises whether the consistency of Haskell types with the types of the external language can be enforced for foreign functions.

Unfortunately, this is, in general, not possible without a significant investment on the part of the implementor of the Haskell system (i.e. without implementing a dedicated type checker).

For example, in the case of the C calling convention, the only other approach would be to generate a C prototype from the Haskell type, and leave it to the C compiler to match this prototype with the prototype that is specified in a C header file, for the imported function.

However, the Haskell type is lacking some information that would be required to pursue this route. In particular, the Haskell type does not contain any information as to when `const` modifiers have to be emitted.

As a consequence, this definition does not require the Haskell system to check consistency with foreign types.

Nevertheless, Haskell systems are encouraged to provide any cross language consistency checks that can be implemented with reasonable effort.
