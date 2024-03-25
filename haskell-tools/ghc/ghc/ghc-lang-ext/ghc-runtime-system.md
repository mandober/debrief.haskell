# GHC runtime system

Runtime system (RTS) handles:
- byte-code interpreter
- software transactional memory
- user space scheduler
- storage management
- thread scheduling
- profiling

There are 4 ways to set RTS options:
- on the command line between `+RTS ... -RTS`, when running the program
- at compile-time, using `-with-rtsopts=⟨opts⟩`
- with the environment variable `GHCRTS`
- by overriding "hooks" in the runtime system

Machine layers (Haskell code is executed in STG semantics):
- STG-machine (abstract machine)
- HEC - Haskell Execution Context (capability, virtual processor)
- Physical CPU (x86, ARM, …)


## Pointer tagging

A word on x64 architecture is 64 bits = 8 bytes = 8⨯8 bits. Word determines a bunch of things such as the size of registers, the size of machine integers, the amount of memory that is accessed per read, memory address alignment, etc.

Memory is read in 64 bit chunks, i.e. the "read head" is 64 bit wide, so on whatever address it is positioned, it will read the chunk of 8 bytes. Placing it on the address 0x00 means reading 8 bytes, from 0x00 to 0x3f inclusive.

It prefers to be positioned at memory addresses `x` such that `x % 64 = 0`, since this results in the most efficint, aligned, access to memory. If insisted it can also be placed at addresses `y` such that `y % 8 = 0`, even though this access is unaligned.

This starting address, at which to place the "read head", is specified in a PL in the form of a pointer. A pointer is just an integer (unsigned) interpreted as a memory address (allowing for pointer aritmetic in some PLs).

However, no matter whether the memory access in aligned or not, the last 3 bits of a pointer are always zero. So, since the last 3 bits are known to always be zero, they can be repurposed to store some relevant info about the pointed data, the tecnic called *pointer tagging*.

GHC employs a pointer tagging scheme:
- …000 *unevaluated closure*
- …001 evaluated closure: 1st ctor value, or *evaluated*; e.g. `Nothing`
- …010 evaluated closure: 2nd ctor value; e.g. `Just x`
- …011 evaluated closure: 3rd ctor value

GHC only needs to check a pointer's 3 LSBs to get some relevent info about a closure's evaluation state, thus being able to make a snappy decision, avoiding further unneeded evaluation.



```
0000:0000  0x00  0ᵗʰ byte: 0x00 - 0x08, the lowest memory address 0x00
0000:1000  0x08  1ˢᵗ byte: 0x08 - 0x10, 2³
0001:0000  0x10  2ⁿᵈ byte: 0x10 - 0x18, 2⁴
0001:1000  0x18  3ʳᵈ byte: 0x18 - 0x20
0010:0000  0x20  4ᵗʰ byte: 0x20 - 0x28, 2⁵
0010:1000  0x28  5ᵗʰ byte: 0x28 - 0x30
0011:0000  0x30  6ᵗʰ byte: 0x30 - 0x38
0011:1000  0x38  7ᵗʰ byte: 0x38 - 0x40

0100:0000  0x40  8ᵗʰ byte: 0x40 - 0x48, 2⁶
0100:1000  0x48
0101:0000  0x50
0101:1000  0x58
0110:0000  0x60
0110:1000  0x68
0111:0000  0x70
0111:1000  0x78

1000:0000  0x80  2⁷
1000:1000  0x88
```

- address ends in 0 or 8, e.g. 0x00, 0x08, 0x10, 0x18, 0x20, 0x28, 0x30, etc.
- the gaps in rows repr prefered positions for aligned access

## Boxed and unboxed types

- A boxed type is represented by a pointer to a heap-allocated object
- An unboxed type is represented by the value itself, `x = 7# :: Int#`


```
Boxed types
                  pointer-to-heap   | header | payload|
x = 7    :: Int ------------------> |   I#   |   7#   |
y = 1.3  :: Float ----------------> |   F#   |  1.3#  |
z = 3.14 :: Double ---------------> |   D#   |  3.14# |
s :: ByteArray# ------------------> |ef|81|de|10|ab|c8|

Unboxed types
x = 7#    :: Int# ---------> 7#
y = 1.3#  :: Float# -------> 1.3#
z = 3.14# :: Double# ------> 3.14#
```

- A *lifted type* is one that contains bottom (⟘), conversely an *unlifted type* does not contain ⟘. For example, `Array` is lifted, but `ByteArray#` is unlifted.

- A boxed type is represented by a pointer to an object in the heap, an unboxed object is represented by a value. For example, `Int` is boxed, but `Int#` is unboxed.
- The representation of ⟘ must be a pointer: it is an object that when evaluated throws an exception or enters an infinite loop. Therefore, *only boxed types may be lifted*.
- There are *boxed unlifted* types, e.g. `ByteArray#`. If you have a value of type `ByteArray#`, it definitely points to a heap object with type `ARR_WORDS`, rather than an unevaluated thunk.
- Unboxed tuples `(#...#)` are both *unlifted and unboxed*. They are represented by multiple values passed in registers or on the stack, according to the return convention.
- Unlifted types cannot represent terminating functions: *an unlifted type on the right of an arrow is implicitly lifted to include ⟘*.

## Heap Objects

All heap objects have the same basic layout, embodied by the type `StgClosure` in `Closures.h`.

https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects
