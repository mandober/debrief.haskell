# Unboxed types


https://wiki.haskell.org/Unboxed_type


**Unboxed types** are types that represent machine primitive values.

Unboxed types have kind `#`. Unboxed types of different storage behaviours (4b, 8b, etc.) are all lumped together under kind `#`; as a result, type variables must have kinds which are `#`-free.

Since Haskell values may contain unevaluated thunks in addition to specific values, in general, values must be represented by a pointer to a heap-allocated object. This is fairly slow, so compilers attempt to replace these boxed values with unboxed raw values when possible. Unboxed values are a feature of GHC compiler that allows directly manipulating these low level values. Since they behave differently than normal Haskell types, generally the type system is extended in a compiler-specific way to type these unboxed values.

By convention, unboxed values have a hash mark suffixed to their name. For instance, the unboxed reprsentation of `42` is `42#`. There are some restrictions to their use; in particular, you can't pass them to polymorphic functions (e.g. to `show` or `$`).



## 9.2. Unboxed types and primitive operations

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#unboxed-types-and-primitive-operations


GHC is built on a raft of primitive data types and operations; "primitive" in the sense that they cannot be defined in Haskell itself.

All these primitive data types and operations are exported by the library GHC.Prim, for which there is detailed online documentation *GHC.Prim*.

If you want to mention any of the primitive data types or operations in your program, you must first *import GHC.Prim* to bring them into scope. Many of them have names ending in `#`, and to mention such names you need the `MagicHash` extension.

The primops make extensive use of unboxed types and unboxed tuples.

## 9.2.1. Unboxed types

> Values of boxed types are represented by a pointer to a heap object.

For example, the representation of a `Int` is a two-word heap object. An unboxed type, however, is represented by the value itself, no pointers or heap allocation are involved.

Unboxed types correspond to the machine types you would use in C: `Int#` (long int), `Double#` (double), `Addr#` (void *), etc.

The primitive operations, **PrimOps**, on these types are what you might expect; e.g. `(+#)` is addition on `Int#`s, i.e and machine-addition which is usully one instruction.

Primitive (unboxed) types cannot be defined in Haskell, and are therefore built into the language and compiler. Primitive types are always *unlifted*; that is, a value of a primitive type cannot be bottom.

A boxed type means that a value is represented by a pointer to a heap object.

A lifted type means that terms of that type may be bottom.

We use the convention (but it is only a convention) that primitive types, values, and operations have a `#` suffix (The magic hash). For some primitive types we have special syntax for literals, also described in the same section.

Primitive values are often represented by a simple bit-pattern, such as `Int#`, `Float#`, `Double#`. But this is not necessarily the case: a primitive value might be represented by a pointer to a heap-allocated object.

Examples include `Array#`, the type of primitive arrays. Thus, `Array#` is an unlifted, boxed type. A primitive array is heap-allocated because it is too big a value to fit in a register, and would be too expensive to copy around; in a sense, it is accidental that it is represented by a pointer. If a pointer represents a primitive value, then it really does point to that value: no unevaluated thunks, no indirections. Nothing can be at the other end of the pointer than the primitive value. A numerically-intensive program using unboxed types can go a lot faster than its "standard" counterpart - we saw a threefold speedup on one example.


## 9.2.2. Unboxed type kinds

## 9.2.3. Unboxed tuples

## 9.2.4. Unboxed sums

## 9.2.5. Unlifted Newtypes
