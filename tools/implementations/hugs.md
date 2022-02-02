# Hugs

- Hugs - Haskell User's Gofer System
- Hugs Homepage: https://www.haskell.org/hugs/
- Hugs User Guide: https://www.haskell.org/hugs/pages/users_guide/
- Wikipedia: https://en.wikipedia.org/wiki/Hugs
- Final release: September 2006
- Developer: Mark P. Jones et al.



`Hugs 98` is a functional programming system based on `Haskell 98`, the de facto standard for non-strict functional programming languages.

Hugs 98 provides an almost complete implementation of Haskell 98 including:
* Lazy evaluation
* higher order functions
* pattern matching
* builtin types, from chars to bignums, and lists to functions
* comprehensive facilities for defining new datatypes and type synonyms
* Advanced polymorphic type system with type and constructor class overloading
* All of the features of the Haskell 98 expression and pattern syntax including lambda, case, conditional and let expressions, list comprehensions, do-notation, operator sections, and wildcard, irrefutable and `as` patterns.
* An implementation of the Haskell 98 primitives for monadic I/O, with support for simple interactive programs, access to text files, handle-based I/O, and exception handling.
* An almost complete implementation of the Haskell module system.
* Hugs 98 also supports a number of advanced and experimental extensions including: *multi-parameter classes*, *extensible records*, *rank-2 polymorphism*, *existentials*, *scoped type variables*, *restricted type synonyms*.

**Hugs 98**, is a bytecode interpreter for Haskell.
* Hugs is the successor to `Gofer`, originally derived from `Gofer v. 2.30b`
* Hugs and Gofer were originally developed by Mark P. Jones
* Hugs comes with a simple graphics library
* Hugs deviates from the Haskell 98 specification in several minor ways. For example, Hugs does not support mutually recursive modules. A list of differences exists.
* The Hugs REPL accepts expressions for evaluation, but not module, type or function definitions. Hugs can load Haskell modules at start-up.

## Extensible records

Extensible records is a non standard Haskell feature unique to Hugs.

Typed records with extensibility (example):

```hs
-- file: test.hs

module Main where

import Hugs.Trex

type Coord = Double

type Point2D = Rec (x :: Coord, y :: Coord)
type Point3D = Rec (x :: Coord, y :: Coord, z ::Coord)
 
point2D = (x = 1, y = 1) :: Point2D

-- predefined:
-- emptyRec :: Rec EmptyRow

-- record extension:
-- (x = 1 | (y = 1))

-- record value decomposition, pattern fields must be non empty:
-- (x=v | rec)

-- record type decomposition:
-- (x :: type | rec)

-- (rec\z) in the context means 'rec' doesn't contain field 'z'

-- add a field `z` with the same type as field `x`
addZCoord :: (r\z, r\x) => t -> Rec ( x :: t | r) -> Rec ( x :: t, z :: t | r)
addZCoord z ( x = x | other) = (x = x, z = z | other)

point3D = addZCoord 3 point2D -- :: Point3D

-- admit any record with 'showable' fields x and y
printXY :: (Show t, r\x, r\y) => Rec (x :: t, y :: t | r) -> IO ()
printXY point = putStrLn xy
  -- with SML style field accessors ('#' prefix)
  where xy = show (#x point) ++ ", " ++ show (#y point)

incrementX :: (Num t, r\x) => Rec (x :: t | r) -> Rec (x :: t | r)
incrementX (x = v | rest) = (x = v + 1 | rest)

main = do
  let point3D' = incrementX point3D
  printXY point2D
  printXY point3D'
```

Run this with H98 compatibility turned off in order to activate the language extension for Extensible records:

`runhugs -98 test.hs`
