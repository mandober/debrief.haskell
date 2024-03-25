# Standard Prelude

In Haskell, many data types are defined in Haskell itself (as opposed to being built-in) and made available via the standard library (stdlib). Unlike the builtin data types, the data types provided via the stdlib are provided as a matter of convenience (and standardization) but are otherwise user-definable.

The stdlib references and dances around many of the builtin types, especially in the module that makes up the *Standard Prelude*, which is made available via the `Prelude` module. The standard prelude exposes a set of data types, classes and functions. It is implicitly preloaded in every new module as if the statement to import it was given explicitly. It contains a curated collection of language items assumed useful to most users; alas, 'twas futile to make such assumptions. As a side effect, this has spawned a new trend of make your fully own customized prelude replacement. 'tis hard to strike a good balance.

There are also many predefined *library modules*, which provide less frequently used functions and types. Separating libraries from the Prelude has the advantage of reducing the size and complexity of the Prelude, allowing it to be more easily assimilated, and increasing the space of useful names available to the programmer.

Prelude and library modules differ from other modules in that their semantics (but not their implementation) are a fixed part of the Haskell language definition. This means, for example, that a compiler may optimize calls to functions in the Prelude without consulting the source code of the Prelude.
