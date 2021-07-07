# Memory footprint of Haskell data types

Memory footprint of Haskell data types
https://stackoverflow.com/questions/3254758/memory-footprint-of-haskell-data-types?rq=1

How to find out GHC's memory representations of data types?
https://stackoverflow.com/questions/6574444/how-to-find-out-ghcs-memory-representations-of-data-types

How can I determine size of a type in Haskell?
https://stackoverflow.com/questions/9492801/how-can-i-determine-size-of-a-type-in-haskell?lq=1

The Haskell Execution Model
https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects

GHC.DataSize
http://hackage.haskell.org/package/ghc-datasize-0.1.2/docs/GHC-DataSize.html


* In GHC, the ctor cost
  - 1 word for a ctor header
  - +1 word for each field
  - *nullary ctors*
    `Nothing, True, False, LT, EQ, GT`, etc.
    take no space even when defined with the `data` keyword,
    because GHC shares around a single instance of these ctors;
    after all, they're semantically the same, only have different names


```hs
data Uno = Uno a      -- takes 2 words
data Due = Due a b    -- takes 3 words

-- Int# takes 1 word, so Int takes 2 words
data Int = I# Int#
```

* Most unboxed types take 1 word (with the exception of `Int64#`, `Word64#` and `Double#` @x32 which take 2 words).

* GHC actually has a cache of small values of type `Int` and `Char`, so in many cases these take no heap space at all. **Thanx to cache, a String only requires space for the cons cells**, unless you use code points > 255 (i.e. larger then a byte, i.e. above the extended-ASCII range).

* An `Int8` has identical representation to `Int`.

* The unbounded `Integer` is defined like this:

```hs
data Integer
  = S# Int#                -- small integers
  | J# Int# ByteArray#     -- large integers
```

* A small Integer (`S#`) takes 2 words, but a large integer (`J#`) takes a variable amount of space depending on its value.

* A `ByteArray#` takes 2 words (header + size) + space for the array itself.

* A ctor defined using `newtype` is free; `newtype` is purely a compile-time construct, taking no extra space.

(More details in *The Layout of Heap Objects* in the GHC Commentary)
