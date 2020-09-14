# Memory footprint of Haskell data types

Memory footprint of Haskell data types
https://stackoverflow.com/questions/3254758/memory-footprint-of-haskell-data-types?rq=1

How to find out GHC's memory representations of data types?
https://stackoverflow.com/questions/6574444/how-to-find-out-ghcs-memory-representations-of-data-types

How can I determine size of a type in Haskell?
https://stackoverflow.com/questions/9492801/how-can-i-determine-size-of-a-type-in-haskell?lq=1

http://hackage.haskell.org/package/ghc-datasize-0.1.2/docs/GHC-DataSize.html

https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects

The Haskell Execution Model
https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects


In GHC, the rule of thumb is: a constructor costs 1 word (4B @x32 or 8B @x64) for a header, and 1 word for each field. The exceptions are nullary ctors (e.g. Nothing or True) which take no space at all because GHC creates a single instance of these ctors, then shares it around.

```hs
data Uno = Uno a      -- an Uno takes 2 words
data Due = Due a b    -- a Due takes 3 words

-- Int is defined as
data Int = I# Int#   -- Int# takes 1 word, so Int takes 2 words
```

Most unboxed types take 1 word, with the exception of `Int64#`, `Word64#` and `Double#` @x32 which take 2 words.

GHC actually has a cache of small values of type `Int` and `Char`, so in many cases these take no heap space at all. A `String` only requires space for the list cells, unless you use code points > 255.

An `Int8` has identical representation to `Int`.

The unbounded `Integer` is defined like this:

```hs
data Integer
  = S# Int#                            -- small integers
  | J# Int# ByteArray#                 -- large integers
```

so a small Integer (S#) takes 2 words, but a large integer takes a variable amount of space depending on its value.

A `ByteArray#` takes 2 words (header + size) + space for the array itself.

Note that a constructor defined with newtype is free. newtype is purely a compile-time construct, taking no extra space.

More details in The Layout of Heap Objects in the GHC Commentary.
