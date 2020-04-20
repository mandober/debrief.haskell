-- Base

id :: a -> a
id x = x

const :: a -> b -> a
const x _ = x

assert :: Bool -> a -> a
assert _pred r = r

{-
Function composition
====================
Make sure it has TWO args only on the left, so that it inlines
when applied to two functions, even if there is no final argument
-}

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)


{-
Swapping arguments
==================
flip takes its (first) two arg in the reverse order of f
>>> flip (++) "hat" "top"
"tophat"
-}

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x


{-
Application operator
===================

Ordinary application f x means the same as f $ x
However, $ has low, right-associative binding precedence, so it sometimes allows parentheses to be omitted.
For example:

f $ g $ h x = f (g (h x))

It is also useful in higher-order situations, such as:

map $ 0 xs

Data.List.zipWith $ fs xs

Note that $ is levity-polymorphic in its result type, so that

foo $ True
-- where foo :: Bool -> Int#

is well-typed

-}

($) :: forall r a (b :: TYPE r). (a -> b) -> a -> b
f $ x =  f x


-- Strict (call-by-value) application operator. It takes a function and an
-- argument, evaluates the argument to weak head normal form (WHNF), then calls
-- the function with that value.
($!) :: forall r a (b :: TYPE r). (a -> b) -> a -> b
f $! x = let !vx = x in f vx  -- see #2273


-- until p f yields the result of applying f until p holds.
until :: (a -> Bool) -> (a -> a) -> a -> a
until p f = go where
    go x | p x       = x
         | otherwise = go (f x)


-- 'asTypeOf' is a type-restricted version of 'const'. It is usually
-- used as an infix operator, and its typing forces its first argument
-- (which is usually overloaded) to have the same type as the second.
asTypeOf :: a -> a -> a
asTypeOf = const


data Maybe a = Nothing | Just a deriving (Eq, Ord)

-----------------------------------------------------------------------------
module GHC.Enum(
        Bounded(..), Enum(..),
        boundedEnumFrom, boundedEnumFromThen,
        toEnumError, fromEnumError, succError, predError,
        -- Instances for Bounded and Enum: (), Char, Int
   ) where
-----------------------------------------------------------------------------
class Bounded a where
  minBound, maxBound :: a

class Enum a where
  -- successor of a value. For numeric types, succ adds 1
  succ :: a -> a
  succ = toEnum . (+ 1) . fromEnum

  -- predecessor of a value. For numeric types, pred subtracts 1
  pred :: a -> a
  pred = toEnum . (subtract 1) . fromEnum

  -- Convert from an Int
  toEnum :: Int -> a

  -- Convert to an Int
  -- It is implementation-dependent what fromEnum returns when
  -- applied to a value that is too large to fit in an Int
  fromEnum :: a -> Int

  enumFrom :: a -> [a]
  enumFrom x = map toEnum [fromEnum x ..]

  enumFromThen :: a -> a -> [a]
  enumFromThen x y = map toEnum [fromEnum x, fromEnum y ..]

  enumFromTo :: a -> a -> [a]
  enumFromTo x y = map toEnum [fromEnum x .. fromEnum y]

  enumFromThenTo :: a -> a -> a -> [a]
  enumFromThenTo x1 x2 y = map toEnum [fromEnum x1, fromEnum x2 .. fromEnum y]

------------------------------------------------------------------------
-- Tuples
------------------------------------------------------------------------

-- @since 2.01
deriving instance Bounded ()

-- @since 2.01
instance Enum () where
  succ _      = errorWithoutStackTrace "Prelude.Enum.().succ: bad argument"
  pred _      = errorWithoutStackTrace "Prelude.Enum.().pred: bad argument"

  toEnum x | x == 0 = ()
    | otherwise = errorWithoutStackTrace "Prelude.Enum.().toEnum: bad arg"

  fromEnum () = 0

  enumFrom        () = [()]
  enumFromTo   () () = [()]
  enumFromThen      () () = let many = ():many in many
  enumFromThenTo () () () = let many = ():many in many

deriving instance (Bounded a, Bounded b)
        => Bounded (a,b)
deriving instance (Bounded a, Bounded b, Bounded c)
        => Bounded (a,b,c)
deriving instance (Bounded a, Bounded b, Bounded c, Bounded d)


------------------------------------------------------------------------
-- Bool
------------------------------------------------------------------------
-- @since 2.01
deriving instance Bounded Bool

-- @since 2.01
instance Enum Bool where
  succ False = True
  succ True  = errorWithoutStackTrace "Prelude.Enum.Bool.succ: bad argument"

  pred True  = False
  pred False  = errorWithoutStackTrace "Prelude.Enum.Bool.pred: bad argument"

  toEnum n | n == 0    = False
           | n == 1    = True
           | otherwise = errorWithoutStackTrace "Prelude.Enum.Bool.toEnum: bad argument"

  fromEnum False = 0
  fromEnum True  = 1

  -- Use defaults for the rest
  enumFrom     = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

```
