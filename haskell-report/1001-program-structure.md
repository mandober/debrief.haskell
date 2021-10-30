# 1.1 Program Structure

https://www.haskell.org/onlinereport/haskell2010/haskellch1.html#x6-100001.1

Program Structure
- abstract syntactic structure
- semantic structure
- syntactic structure
- topmost (program) level: set of modules
- module (top) level: set of declarations
- term-level: expressions, terms, values at static types
- lowest-level: lexical structure, concrete representation

This report presents Haskell's *abstract syntactic structure* and *abstract semantic structure*.

1. The **top-most level** (program level) of a program consists of a *set of modules* that provide code reuse and a way of controlling the namespaces.

2. The **module level** (top level) consists of a *set of declarations*, which include declarations of datatypes, ordinary values, classes, instances, etc.

3. The **term level** (middle level) consists of *expressions* which denote values with static types.

4. The **lowest level** consists of *lexical structure* which captures the *concrete representation* of programs in source files.
