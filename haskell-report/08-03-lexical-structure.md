# 8.3 Lexical Structure

The FFI reserves a single keyword `foreign`, and a set of special identifiers.

The latter have a special meaning only within *foreign declarations*, but may be used as ordinary identifiers elsewhere.

The special identifiers `ccall`, `cplusplus`, `dotnet`, `jvm`, and `stdcall` are defined to denote calling conventions. However, a concrete implementation of the FFI is free to support additional, system-specific calling conventions whose name is not explicitly listed here.

To refer to objects of an external C context, we introduce the following phrases:

```js bnf
chname ::= {chchar} . h                       (C header filename)
cid    ::= letter {letter | ascDigit}         (C identifier)
chchar ::= letter | ascSymbol⟨&⟩
letter ::= _ | ascSmall | ascLarge
```

The range of lexemes that are admissible for `chname` is a subset of those permitted as arguments to the `#include` directive in C. In particular, a file name `chname` must end in the suffix `.h`. The lexemes produced by `cid` coincide with those allowed as C identifiers.
