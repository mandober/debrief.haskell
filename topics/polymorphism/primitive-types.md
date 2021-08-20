## Language primitive types

A PL provides language primitives, which are the language constructs among which is the set of base types, along with the means of constructing new types. The set of base types commonly inlcudes multiple versions of integers, classified by their size, `2ⁿ` where `n = {8, 16, 32, 64}`; each further classified by signedness: signed integers are denoted as `Int`, unsigned by `Word`, e.g. `Int8` vs `Word8`.

There are also at least two versions of floating-point numbers: the single precision `Float` (4 bytes) and the double-precision `Double` (8 bytes).

Characters are frequently included in the set of base types as well as a language primitive type, `Char`; often along with strings, `String`.
