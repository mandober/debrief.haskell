module QuickReference where
-- Haskell Quick Reference

-- ============================================================================
-- List
-- ============================================================================
module List(
    []([], (:))    -- export type ctor [], incl. both data ctors [] and (:)
) where

-- ============================================================================
-- List is intrinsic compiler type, fake-defined (in GHC.Types) as:
data [] a = [] | a : [a]

-- my List definition
data List a = Nil | Cons a (List a) deriving (Eq, Ord, Read, Show)
-- replace [] with Nil, (:) with Cons
_ = Cons 1 (Cons 2 (Cons 3 Empty))




-- ============================================================================
-- EXPRESSIONS
-- ============================================================================

-- where expression in functions
printInc n = print plusTwo
    where plusTwo = n + 2

-- let expression in functions
printInc2 n = let plusTwo = n + 2
              in  print plusTwo

printInc2 n =
    let
        plusTwo = n + 2
        plusThree = n + 3
    in
        print plusTwo
        print plusThree


-- binding to symbolic vars, USING SYMBOLS AS BINDING PATERNS:
let f (%) x y = x % y in f (*) 4 12
-- or...(in fact above desugars to)
(\(%) x y -> x % y) (*) 4 12    -- 4*12
(\(%) x y -> x % y) (:) 6 []    -- 6:[] == [6]

-- can also use SECTIONING with infix fns:
(\(%) x y -> (x %) y) (^) 2 5   -- 2^5
(\(%) x y -> (% x) y) (^) 2 5   -- 5^2




-- ============================================================================
-- Classes
-- ============================================================================

class Num a where
    (+) :: a -> a -> a -- (Num is defined in the Prelude)
    negate :: a -> a

instance Num Int where
    x + y = addInt x y
    negate x = negateInt x

instance Num Float where
    x + y = addFloat x y
    negate x = negateFloat x
