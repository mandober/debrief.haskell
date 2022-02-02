# GHC.Exts

GHC's primitive types and operations. Use `GHC.Exts` from the base package instead of importing `GHC.Prim` module directly.

See:
`prim bool - Implementing new primitive comparisons to allow branchless algorithms`
https://gitlab.haskell.org/ghc/ghc/-/wikis/prim-bool


## The word size

https://hackage.haskell.org/package/ghc-prim-0.5.3/docs/GHC-Prim.html

* Signed integer type
  - `Int`     lifted signed integer type (>= 30 bits)
  - `Int#` primitive signed integer type (`WORD_SIZE_IN_BITS` from `MachDeps.h`)
  - `WORD_SIZE_IN_BITS` is based on `SIZEOF_HSWORD` (from `config.h`)
  - `SIZEOF_HSWORD` is 32 @ 32-bit arch, 64 @ 64-bit arch
  - @32-bit arch, can be set to 30 or 31 bits to allow tag bits

* Unsigned integer type
  - `Word`     lifted unsigned integer type
  - `Word#` primitive unsigned integer type
  - `Word#` has the same number of bits as `Int#`



### Signed integer type

* Haskell98 specifies that the **lifted signed integer type** `Int` must contain **at least 30 bits**. GHC always implements `Int` using `Int#`.

* The size of the **primitive signed integer type** `Int#` is the constant `WORD_SIZE_IN_BITS` from `MachDeps.h`.

* This is normally set based on the parameter `SIZEOF_HSWORD` (32 bits on 32-bit machines, 64 bits on 64-bit machines) form `config.h`.

* However, it can also be explicitly set to a smaller number (30 or 31 bits) to allow the possibility of using *tag bits*. This is for x32 coz then the address align, or end in bits 0,4,8,C (in hex), which means that only the 2 lsb bits are always 00 thus available for as tag bits. But at x64, the last 3 bits are always 000 which GHC uses for tagging. So, ideally, to have a universal implementation, the x32 platform needs also to make 3 last bits available for tagging, and that's the reason [guess] for specs allowing the integer variants with 30 or 31 bits.

* Currently GHC itself has only 32-bit and 64-bit variants, but 30 or 31-bit code can be exported as an external core file for use in other back-ends.


### Unsigned integer type

* GHC also implements a **primitive unsigned integer type** `Word#`

* `Word#` always has the same number of bits as `Int#`, but a different range.


### Integer and word families

* In addition, GHC supports families of explicitly-sized integers and words at 8, 16, 32, and 64 bits, with the usual arithmetic operations, comparisons, and a range of conversions.

* The 8-bit and 16-bit sizes are always represented as `Int#` and `Word#`, and the operations implemented in terms of the primops on these types, with suitable range restrictions on the results (using the `narrow$n$Int#` and `narrow$n$Word#` families of primops.

* The 32-bit sizes are represented using `Int#` and `Word#` when `WORD_SIZE_IN_BITS $geq$ 32`; otherwise, these are represented using distinct primitive types `Int32#` and `Word32#`. These (when needed) have a complete set of corresponding operations; however, nearly all of these are implemented as *external C functions rather than as primops*.

* Exactly the same story applies to the 64-bit sizes. All of these details are hidden under the `PrelInt` and `PrelWord` modules, which use `#if-defs` to invoke the appropriate types and operators.

* Word size also matters for the families of primops for indexing/reading/writing fixed-size quantities at offsets from an array base, address, or foreign pointer.

* Here, a slightly different approach is taken. The names of these primops are fixed, but their types vary according to the value of `WORD_SIZE_IN_BITS`. For example, if word size is at least 32 bits then an operator like `indexInt32Array#` has type `ByteArray# -> Int# -> Int#`; otherwise it has type `ByteArray# -> Int# -> Int32#`.

* This approach confines the necessary `#if-defs` to this file; no conditional compilation is needed in the files that expose these primops.


* Finally, there are *strongly deprecated primops* for coercing between `Addr#`, the primitive type of machine addresses, and `Int#`. These are pretty bogus anyway, but will work on existing 32-bit and 64-bit GHC targets; they are completely bogus when tag bits are used in `Int#`, so are not available in this case.





## Char#

Operations on 31-bit characters.

…

## Small Arrays

Operations on SmallArray#. A SmallArray# works just like an Array#, but with different space use and performance characteristics (that are often useful with small arrays). The SmallArray# and SmallMutableArray# lack a `card table'. The purpose of a card table is to avoid having to scan every element of the array on each GC by keeping track of which elements have changed since the last GC and only scanning those that have changed. So the consequence of there being no card table is that the representation is somewhat smaller and the writes are somewhat faster (because the card table does not need to be updated). The disadvantage of course is that for a SmallMutableArray# the whole array has to be scanned on each GC. Thus it is best suited for use cases where the mutable array is not long lived, e.g. where a mutable array is initialised quickly and then frozen to become an immutable SmallArray#.

## Byte Arrays

Operations on ByteArray#. A ByteArray# is a just a region of raw memory in the garbage-collected heap, which is not scanned for pointers. It carries its own size (in bytes). There are three sets of operations for accessing byte array contents: index for reading from immutable byte arrays, and read/write for mutable byte arrays. Each set contains operations for a range of useful primitive data types. Each operation takes an offset measured in terms of the size of the primitive type being read or written.

## Addr#

`data Addr#`

An arbitrary machine address assumed to point outside the garbage-collected heap.

## Etc

Miscellaneous built-ins

`data Proxy# a`   
The type constructor Proxy# is used to bear witness to some type variable. It's used when you want to pass around proxy values for doing things like modelling type applications. A `Proxy#` is *not only unboxed, it also has a polymorphic kind, and has no runtime representation*, being totally free.

`proxy# :: Proxy# a`   
Witness for an unboxed Proxy# value, which has no runtime representation.

`seq :: a -> b -> b`   
The value of `seq a b` is bottom if `a` is bottom, otherwise equal to `b`. In other words, it evaluates the first argument `a` to weak head normal form (WHNF). seq is usually introduced to improve performance by avoiding unneeded laziness.

A note on evaluation order: the expression `seq a b` does not guarantee that `a` will be evaluated before `b`. **The only guarantee given by seq is that the both `a` and `b` will be evaluated before seq returns a value**. In particular, this means that `b` may be evaluated before `a`. If you need to guarantee a specific order of evaluation, you must use the function `pseq` from the "parallel" package.

`unsafeCoerce# :: a -> b`    
The function `unsafeCoerce#` allows you to side-step the typechecker entirely. That is, **it allows you to coerce any type into any other type**. If you use this function, you had better get it right, otherwise segmentation faults await. It is generally used when you want to write a program that you know is well-typed, but where Haskell's type system is not expressive enough to prove that it is well typed.

The following uses of `unsafeCoerce#` are supposed to work (i.e. not lead to spurious compile-time or run-time crashes):
- Casting any lifted type to `Any`
- Casting `Any` back to the real type
- Casting an unboxed type to another unboxed type of the same size. (Casting between floating-point and integral types does not work. See the `GHC.Float` module for functions to do work.)
- Casting between two types that have the same runtime representation. One case is when the two types differ only in "phantom" type parameters, for example `Ptr Int` to `Ptr Float`, or [Int] to [Float] when the list is known to be empty. Also, a newtype of a type `T` has the same representation at runtime as `T`.

Other uses of `unsafeCoerce#` are undefined. In particular, you should not use `unsafeCoerce#` to cast a `T` to an algebraic data type `D`, unless `T` is also an algebraic data type. For example, do not cast `Int -> Int` to `Bool`, even if you later cast that `Bool` back to `Int -> Int` before applying it. The reasons have to do with GHC's internal representation details (for the cognoscenti, data values can be entered but function closures cannot). If you want a safe type to cast things to, use `Any`, which is not an algebraic data type.

`traceEvent# :: Addr# -> State# s -> State# s`   
Emits an event via the RTS tracing framework. The contents of the event is the zero-terminated byte string passed as the first argument. The event will be emitted either to the .eventlog file, or to stderr, depending on the runtime RTS flags.

`traceMarker# :: Addr# -> State# s -> State# s`    
Emits a marker event via the RTS tracing framework. The contents of the event is the zero-terminated byte string passed as the first argument. The event will be emitted either to the .eventlog file, or to stderr, depending on the runtime RTS flags.

`getThreadAllocationCounter# :: State# RealWorld -> (#State# RealWorld, Int##)`   
Retrieves the allocation counter for the current thread.

`setThreadAllocationCounter# :: Int# -> State# RealWorld -> State# RealWorld`   
Sets the allocation counter for the current thread to the given value.

## Safe coercions

`coerce :: Coercible a b => a -> b#`

> The function `coerce` allows you to safely convert between values of types that have the same representation with no run-time overhead.

In the simplest case you can use it instead of a newtype constructor, to go from the newtype's concrete type to the abstract type. But it also works in more complicated settings, e.g. converting a list of newtypes to a list of concrete types.

## Prefetch

Prefetch operations: Note how every prefetch operation has a name with the pattern `prefetch*N#`, where `N` is either 0,1,2, or 3.

* This suffix number, `N`, is the "locality level" of the prefetch, following the convention in GCC and other compilers. Higher locality numbers correspond to the *memory being loaded in more levels of the CPU cache, and being retained after the initial use*. The naming convention follows the naming convention of the prefetch intrinsic found in the GCC and Clang C compilers.

* On the LLVM backend, `prefetch*N#` uses the LLVM prefetch intrinsic with locality level `N`. The code generated by LLVM is target architecture dependent, but should agree with the GHC NCG on x86 systems.

* On the Sparc and PPC native backends, `prefetch*N` is a No-Op.

* On the x86 NCG, N=0 will generate prefetchNTA, N=1 generates prefetcht2, N=2 generates prefetcht1, and N=3 generates prefetcht0.

For streaming workloads, the `prefetch*0` operations are recommended. For workloads which do many reads or writes to a memory location in a short period of time, `prefetch*3` operations are recommended.

For further reading about prefetch and associated systems performance optimization, the instruction set and optimization manuals by Intel and other CPU vendors are excellent starting place.

The "Intel 64 and IA-32 Architectures Optimization Reference Manual" is especially a helpful read, even if your software is meant for other CPU architectures or vendor hardware. The manual can be found at:

http://www.intel.com/content/www/us/en/architecture-and-technology/64-ia-32-architectures-optimization-manual.html

The `prefetch*` family of operations has the order of operations determined by passing around the `State#` token.

To get a "pure" version of these operations, use `inlinePerformIO` which is quite safe in this context.

It is important to note that while the prefetch operations will never change the answer to a pure computation, They CAN change the memory locations resident in a CPU cache and that may change the performance and timing characteristics of an application. The prefetch operations are marked `has_side_effects=True` to reflect that these operations have side effects with respect to the runtime performance characteristics of the resulting code. Additionally, if the `prefetchValue` operations did not have this attribute, GHC does a float out transformation that results in a let/app violation, at least with the current design.
