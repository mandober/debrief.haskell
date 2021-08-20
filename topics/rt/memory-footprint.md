# Memory footprint of Haskell data types in GHC

A constructor costs
- 1 word for a header
- 1 word for each constructor field
- except: nullary data constructors take (`Nothing`, `True`) no space (even if defined with `data`) since GHC shares around a single instance of these ctors; after all, apart from their names, they are semantically the same.

```hs
-- nullary type ctor (Bool) = 1 word for a header?
-- nullary data ctors (True, False) = take no space
data Bool = True | False

-- unary data ctor takes 2 words: 1 header + 1 field
data Uno = Uno a

-- binary data ctor takes 3 words: 1 header + 2 fields
data Due = Due a b


-- Int type definition
data Int = I# Int#
-- Int# takes 1 word
-- Int takes 2 words: 1 header + 1 field
```

- `Int#` field takes 1 word
- `I#`   data ctor takes 2 words: 1 for header and 1 for `Int#` field
- `Int`  type ctor takes 2 words total (1 for header and 1 for `Int#` field), so it is equal to the sum of RHS cost? i.e. equal to `I#` cost?

- Most unboxed types take 1 word
- exceptions: `Int64#`, `Word64#` and `Double#` (@32-bit) which take 2

GHC keeps a cache of small values of type `Int` and `Char`, so in many cases these take no heap space at all.

A `String` only requires space for the list cells, unless you use Char > 255 (Unicode i.e. larger then a byte Chars from non-ASCII range).

- An `Int8` has identical representation to `Int`

- The unbounded `Integer` is defined like this:

```hs
data Integer
  = S# Int#                -- small integers
  | J# Int# ByteArray#     -- large integers
```

- small Integer, `S#`, takes 2 words
- large integer, `J#`, takes a variable amount of space depending on its value

- `ByteArray#` takes 2 words (header + size) + space for the array itself

- A ctor defined using `newtype` is free since `newtype` is purely a compile-time construct, taking no extra RT space.

- More details in *The Layout of Heap Objects* in the GHC Commentary

- every heap object is guaranteed to be at least two 2 words in size.


## FiniteBits class

```hs
import Data.Bits

-- actual value is ignored
sb = finiteBitSize (undefined :: Bool) -- 1
sn = finiteBitSize (undefined :: Int)  -- 64
sw = finiteBitSize (undefined :: Word) -- 64

-- works only for types that are instances of FiniteBits class...
class Bits b => FiniteBits b where
  finiteBitSize      :: b -> Int
  countLeadingZeros  :: b -> Int
  countTrailingZeros :: b -> Int
  {-# MINIMAL finiteBitSize #-}

-- ...which are Int, Bool, Word
instance FiniteBits Word  -- Defined in Data.Bits
instance FiniteBits Int   -- Defined in Data.Bits
instance FiniteBits Bool  -- Defined in Data.Bits
instance FiniteBits a => FiniteBits (Identity a)
```


## Can't the header overhead for one-constructor data types be optimised away?

No, it cannot be optimized away. For example, the GC does not know in advance the type of a heap object it reaches via a pointer from another heap object, so there needs to be some indication in each object of its size and which words in it are pointers to other objects.

Also, regular Haskell evaluation (the "mutator") needs to be able to represent both evaluated values and thunks, which it couldn't do if the representation of an evaluated value could be arbitrary (having no header word).

## How big a closure is
by Simon Marlow, 2009
https://ghcmutterings.wordpress.com/2009/02/12/53/

How to find out the size of the runtime representation of a type. 
I hacked this up using the GHC intrinsic `unpackClosure#` primitive:

```hs
{-# LANGUAGE MagicHash,UnboxedTuples #-}
module Size where

import GHC.Exts
import Foreign

unsafeSizeof :: a -> Int
unsafeSizeof a = 
  case unpackClosure# a of 
    (# x, ptrs, nptrs #) -> 
      sizeOf (undefined::Int) + -- one word for the header
        I# (sizeofByteArray# (unsafeCoerce# ptrs) 
             +# sizeofByteArray# nptrs)
```

Try it in GHCi:

```hs
-- compile it
Prelude> :!ghc -c Size.hs
-- load it
Prelude> :l Size
Ok, modules loaded: Size.

Prelude Size> unsafeSizeof True                       --  8
Prelude Size> unsafeSizeof 3.3                        -- 16
Prelude Size> unsafeSizeof "a"                        -- 24
Prelude Size> unsafeSizeof (1,2,3,4)                  -- 40

Prelude Size> unsafeSizeof $! Data.Complex.(:+) 2 3   -- 24

Prelude Size> unsafeSizeof $! 3                       -- 16
Prelude Size> unsafeSizeof $! (3::Int)                -- 16
Prelude Size> unsafeSizeof $! (3::Integer)            -- 16
Prelude Size> unsafeSizeof $! (2^64::Integer)         -- 24
```

This is on a 64-bit machine. It doesn't always do the right thing, but for ordinary algebraic types, it should work most of the time. Remember to use `$!` as the size returned for an unevaluated thunk is always 1 word, and `unpackClosure#` doesn't work for thunks.



## References

* Memory footprint of Haskell data types
https://stackoverflow.com/questions/3254758/memory-footprint-of-haskell-data-types?rq=1

* Memory footprint of Haskell data types
https://newbedev.com/memory-footprint-of-haskell-data-types
