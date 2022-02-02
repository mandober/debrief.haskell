# 8.4 Foreign Declarations

8.4 Foreign Declarations
  - 8.4.1 Calling Conventions
  - 8.4.2 Foreign Types
  - 8.4.3 Import Declarations
  - 8.4.4 Export Declarations


```js bnf
topdecl  ::= foreign fdecl

fdecl    ::= import callconv [safety] impent var :: ftype  (define variable)
           | export callconv expent var :: ftype           (expose variable)

callconv ::= ccall                                         (calling convention)
           | stdcall
           | cplusplus
           | jvm
           | dotnet
           | system-specific calling conventions

impent ::= [string]
expent ::= [string]

safety ::= unsafe | safe
```

There are two flavours of foreign declarations:
- import declarations
- export declarations

An *import declaration* makes an external entity (i.e. a function or memory location defined in an external context) available in the Haskell context.

Conversely, an *export declaration* defines a function of the Haskell context as an external entity in an external context.

Consequently, the two types of declarations differ in that an import declaration defines a new variable, whereas an export declaration uses a variable that is already defined in the Haskell module.

The external context that contains the external entity is determined by the calling convention given in the foreign declaration. Consequently, the exact form of the specification of the external entity is dependent on both the calling convention and on whether it appears in an import declaration (as `impent`) or in an export declaration (as `expent`).

To provide syntactic uniformity in the presence of different calling conventions, it is guaranteed that the description of an external entity lexically appears as a Haskell string lexeme. The only exception is where this string would be the empty string (i.e. be of the form ""), in which case, the string may be omitted in its entirety.

## 8.4.1 Calling Conventions

The binary interface to an external entity on a given architecture is determined by a calling convention. It often depends on the programming language in which the external entity is implemented, but usually is more dependent on the system for which the external entity has been compiled.

As an example of how the calling convention is dominated by the system rather than the programming language, consider that an entity compiled to byte code for the Java Virtual Machine (JVM) [11] needs to be invoked by the rules of the JVM rather than that of the source language in which it is implemented (the entity might be implemented in Oberon, for example).

Any implementation of the Haskell FFI must at least implement the C calling convention denoted by ccall. All other calling conventions are optional. Generally, the set of calling conventions is open, i.e., individual implementations may elect to support additional calling conventions. In addition to ccall, Table 8.1 specifies a range of identifiers for common calling conventions.

## 8.4.2 Foreign Types



## 8.4.3 Import Declarations




## 8.4.4 Export Declarations
