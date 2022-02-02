# 8.1 Foreign Languages

The Haskell FFI currently only specifies the interaction between Haskell code and foreign code that follows the *C calling convention*.

However, the design of the FFI is such that it enables the modular extension of the present definition to include the calling conventions of other programming languages (C++, Java, etc.) as well. Pprecise definition of the support for those languages is expected to be included in later versions of the language.

The second major omission is the definition of the interaction with multithreading in the foreign language and, in particular, the treatment of thread-local state, and so these details are currently implementation-defined.

The core of the present specification is independent of the foreign language that is used in conjunction with Haskell. However, there are two areas where FFI specifications must become language specific:
1. the specification of external names
2. the marshalling of the basic types of a foreign language

As an example of (1), consider that in C a simple identifier is sufficient to identify an object, while Java, in general, requires a qualified name in conjunction with the argument and result types to resolve possible overloading.

As an example of (2), consider that many languages do not specify the exact representation of some basic types. For example the type `int` in C may be 16, 32, or 64 bits wide. Similarly, Haskell guarantees only that `Int` covers at least the range `[−2^29, 2^29 − 1]` (Section 6.4). As a consequence, to reliably represent values of C's int in Haskell, we have to introduce a new type `CInt`, which is guaranteed to match the representation of int.

- Section 8.5 describes the specification of external names that are dependent on a calling convention
- Section 8.6 describes the marshalling of the basic types in dependence on a foreign language
