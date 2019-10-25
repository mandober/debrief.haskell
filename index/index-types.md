# Datatypes

* Standard types, classes and related functions
  * Basic data types
    - Bool = False | True
      - (&&), (||)
      - not
      - otherwise (True)
    - Maybe = Nothing | Just a, maybe
    - Either = Left | Right), either
    - Ordering = LT | EQ | GT
    - Char, String
    * Tuples
      - fst/snd
      - curry/uncurry
  * Basic type classes
    - Eq: (==), (/=)
    - Ord: compare, (<), (<=), (>=), (>), max, min
    - Enum(succ, pred, toEnum, fromEnum
      enumFrom, enumFromThen, enumFromTo, enumFromThenTo)
    - Bounded(minBound, maxBound)
  * Numbers
    - Numeric types
      - Int, Integer
      - Float, Double
      - Rational, Word
    - Numeric type classes
      - Num((+), (-), (*), negate, abs, signum, fromInteger),
      - Real(toRational),
      - Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
      - Fractional((/), recip, fromRational),
      - Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
              asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
      - RealFrac(properFraction, truncate, round, ceiling, floor),
      - RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
                encodeFloat, exponent, significand, scaleFloat, isNaN,
                isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),
    - Numeric functions
      - subtract, even, odd, gcd, lcm, (^), (^^),
      - fromIntegral, realToFrac,
  - Semigroups and Monoids
    - Semigroup((<>)),
    - Monoid(mempty, mappend, mconcat),
  - Monads and functors
    - Functor(fmap, (<$)), (<$>),
    - Applicative(pure, (<*>), (*>), (<*)),
    - Monad((>>=), (>>), return, fail),
    - mapM_, sequence_, (=<<),
  - Folds and traversals
    - Foldable(elem,      -- :: (Foldable t, Eq a) => a -> t a -> Bool
              -- fold,   -- :: Monoid m => t m -> m
              foldMap,   -- :: Monoid m => (a -> m) -> t a -> m
              foldr,     -- :: (a -> b -> b) -> b -> t a -> b
              -- foldr', -- :: (a -> b -> b) -> b -> t a -> b
              foldl,     -- :: (b -> a -> b) -> b -> t a -> b
              -- foldl', -- :: (b -> a -> b) -> b -> t a -> b
              foldr1,    -- :: (a -> a -> a) -> t a -> a
              foldl1,    -- :: (a -> a -> a) -> t a -> a
              maximum,   -- :: (Foldable t, Ord a) => t a -> a
              minimum,   -- :: (Foldable t, Ord a) => t a -> a
              product,   -- :: (Foldable t, Num a) => t a -> a
              sum),      -- :: Num a => t a -> a
              -- toList) -- :: Foldable t => t a -> [a]
    - Traversable(traverse, sequenceA, mapM, sequence),
  - Miscellaneous functions
    - id, const, (.), flip, ($), until,
    - asTypeOf, error, errorWithoutStackTrace, undefined,
    - seq, ($!),
* Lists
  * List operations
    * List ops
      - map, (++), filter,
      - head, last, tail, init, null, length, (!!),
      - reverse,
    * Special folds
      - and, or, any, all,
      - concat, concatMap,
  * Building lists
    * Scans
    scanl, scanl1, scanr, scanr1,
    * Infinite lists
    iterate, repeat, replicate, cycle,
  * Sublists
    take, drop, splitAt, takeWhile, dropWhile, span, break,
  * Searching lists
    notElem, lookup,
  * Zipping and unzipping lists
    zip, zip3, zipWith, zipWith3, unzip, unzip3,
  * Functions on strings
    lines, unlines, words, unwords,
* Converting to and from @String@
  * Converting to @String@
    ShowS,
    Show(showsPrec, showList, show),
    shows,
    showChar, showString, showParen,
  * Converting from @String@
    ReadS,
    Read(readsPrec, readList),
    reads, readParen, read, lex,
* Basic Input and output
  - IO,
  * Simple I/O operations
    All I/O functions defined here are character oriented.
    The treatment of the newline character will vary on different systems.
    For example, two characters of input, return and linefeed, may
    read as a single newline character.
    These functions cannot be used portably for binary I/O.
  * Output functions
    - putChar,
    - putStr, putStrLn, print,
  * Input functions
    - getChar,
    - getLine, getContents, interact,
  * Files
    - FilePath,
    - readFile, writeFile, appendFile, readIO, readLn,
  * Exception handling in the I/O monad
    - IOError, ioError, userError,
:
