# Data.List

Functions from `Data.List` that work on String (as a list of Char).

```hs
import Data.List

lines   :: String -> [String]
unlines :: [String] -> String

words   :: String -> [String]
unwords :: [String] -> String




-- Takes a String and returns a list of Strings such that the concatenation of the result is equal to the arg. Moreover, each substring in the result contains only equal Chars.
--
-- >>> group "Mississippi" -- ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows supplying a predicate.
-- >>> group = groupBy (==)
--
group :: Eq a => [a] -> [[a]]
group :: String -> [String]

groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy :: (Char -> Char -> Bool) -> String -> [String]


take :: Int -> [a] -> [a]
takeWhile :: (a -> Bool) -> [a] -> [a]

sort     :: Ord a => [a] -> [a]
sortOn   :: Ord b => (a -> b) -> [a] -> [a]
sortBy   :: (a -> a -> Ordering) -> [a] -> [a]

nub   :: Eq a => [a] -> [a]
nubBy :: (a -> a -> Bool) -> [a] -> [a]

stripPrefix  :: Eq a => [a] -> [a] -> Maybe [a]

(++) :: [a] -> [a] -> [a]
uncons  :: [a] -> Maybe (a, [a])
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]

break   :: (a -> Bool) -> [a] -> ([a], [a])
span    :: (a -> Bool) -> [a] -> ([a], [a])
splitAt :: Int         -> [a] -> ([a], [a])

reverse :: [a] -> [a]
filter :: (a -> Bool) -> [a] -> [a]

concat    :: Foldable t => t [a] -> [a]
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
map :: (a -> b) -> [a] -> [b]
mapAccumL    :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
mapAccumR    :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)

length :: Foldable t => t a -> Int

sum     :: (Foldable t, Num a) => t a -> a
product :: (Foldable t, Num a) => t a -> a

tails :: [a] -> [[a]]
inits :: [a] -> [[a]]


minimum   :: (Foldable t, Ord a) => t a -> a

-- The least elem of non-empty structure with respect to the given predicate.
-- >>> minimumBy (compare `on` length) ["Hello","World","!","Longest","bar"]
-- >>> -- "!"
minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximum   :: (Foldable t, Ord a) => t a -> a
maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a


-- ----------------------------------------------------------------------------
-- insert
-- ----------------------------------------------------------------------------
-- | O(n). Takes an element and a list and intersperses that element between the elements of the list.
-- >>> intersperse ',' "abcde" -- "a,b,c,d,e"
intersperse :: a -> [a] -> [a]

-- | Inserts the list xs in between the lists in xss and concates the result:
-- >>> intercalate xs xss = concat (intersperse xs xss)
--
-- >>> intercalate ", " ["Lorem", "ipsum", "dolor"] -- "Lorem, ipsum, dolor"
intercalate :: [a] -> [[a]] -> [a]


-- | O(n). Takes an element and a list and inserts the element into the list at the first position where it is LE to the next element. In particular, if the list is sorted before the call, the result will also be sorted. It is a special case of 'insertBy', which allows the programmer to supply own predicate.
-- >>> insertBy (<=) = insert
--
-- >>> insert 4 [1,2,3,5,6,7] -- [1,2,3,4,5,6,7]
insert :: Ord a => a -> [a] -> [a]
insert :: Char -> String -> String

insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy :: (Char -> Char -> Ordering) -> Char -> String -> String

-- ----------------------------------------------------------------------------
-- set ops
-- ----------------------------------------------------------------------------
-- | >>> "abcd" \\ "cd" -- "ab"
(\\) :: Eq a => [a] -> [a] -> [a]
(\\) :: String -> String -> String

union :: Eq a => [a] -> [a] -> [a]
unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersect :: Eq a => [a] -> [a] -> [a]
intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
transpose :: [[a]] -> [[a]]
permutations :: [a] -> [[a]]
partition :: (a -> Bool) -> [a] -> ([a], [a])

-- Returns the list of all subsequences (subsets) of the arg (like powerset)
-- >>> subsequences "abc" -- ["","a","b","ab","c","ac","bc","abc"]
subsequences :: [a] -> [[a]]

-- ----------------------------------------------------------------------------
-- generate
-- ----------------------------------------------------------------------------
cycle     :: [a] -> [a]
repeat    :: a -> [a]
replicate :: Int -> a -> [a]

iterate :: (a -> a) -> a -> [a]

-- Strict version of iterate that forces the result of each application of the function to WHNF before proceeding.
iterate' :: (a -> a) -> a -> [a]

-- ----------------------------------------------------------------------------
-- delete
-- ----------------------------------------------------------------------------
-- | Deletes the first ocurence of a Char in a String.
-- >>> delete 'b' "bcdabdf" -- "cdabdf"
delete :: Eq a => a -> [a] -> [a]
delete :: Char -> String -> String

-- | Deletes the first ocurence of a Char in a String,
-- using a predicate to identify the Char.
-- >>> deleteBy (==) 'a' "abacad" -- "bacad"
deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy :: (Char -> Char -> Bool) -> Char -> String -> String

-- | Takes a predicate and two Strings and returns the first Strings with the first occurrence of each Char of the second Strings removed.
-- >>> deleteFirstsBy (==) "axbyczafbh" "abc"  -- "xyzafbh"
-- >>> deleteFirstsBy (==) "axbyczafbh" "aabc" -- "xyzfbh"
deleteFirstsBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy :: (Char -> Char -> Bool) -> String -> String -> String

-- | `drop n xs` returns the suffix of xs after the first n Chars,
-- or `""` if `n > length x`.
drop :: Int -> [a] -> [a]
drop :: Int -> String -> String

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile :: (Char -> Bool) -> String -> String

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd :: (Char -> Bool) -> String -> String

-- ----------------------------------------------------------------------------
-- basics
-- ----------------------------------------------------------------------------
head :: [a] -> a
last :: [a] -> a
tail :: [a] -> [a]
init :: [a] -> [a]

-- ----------------------------------------------------------------------------
-- generics
-- ----------------------------------------------------------------------------
genericLength    :: Num i => [a] -> i
genericIndex     :: Integral i => [a] -> i -> a
genericReplicate :: Integral i => i -> a -> [a]
generic
Drop      :: Integral i => i -> [a] -> [a]
genericTake      :: Integral i => i -> [a] -> [a]
genericSplitAt   :: Integral i => i -> [a] -> ([a], [a])


-- ----------------------------------------------------------------------------
-- predicates
-- ----------------------------------------------------------------------------
and :: Foldable t => t Bool -> Bool
or  :: Foldable t => t Bool -> Bool

all :: Foldable t => (a -> Bool) -> t a -> Bool
any :: Foldable t => (a -> Bool) -> t a -> Bool

elem    :: (Foldable t, Eq a) => a -> t a -> Bool
notElem :: (Foldable t, Eq a) => a -> t a -> Bool

null :: Foldable t => t a -> Bool

isPrefixOf       :: Eq a => [a] -> [a] -> Bool
isInfixOf        :: Eq a => [a] -> [a] -> Bool
isSuffixOf       :: Eq a => [a] -> [a] -> Bool

-- | Subsequence need not be contiguous:
-- >>> isSubsequenceOf "be" "abcde" -- True
-- >>> isSubsequenceOf "bc" "abcde" -- True
--
isSubsequenceOf  :: String -> String -> Bool
isSubsequenceOf  :: Eq a => [a] -> [a] -> Bool

-- ----------------------------------------------------------------------------
-- search
-- ----------------------------------------------------------------------------
(!!) :: [a] -> Int -> a

lookup :: Eq a => a -> [(a, b)] -> Maybe b


-- | The 'elemIndex' function returns the index of the first element in the given list which is equal (by '==') to the query element, or 'Nothing' if there is no such element.
--
-- >>> elemIndex 4 [0..] -- Just 4
elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex :: Char -> String -> Maybe Int

-- | The 'elemIndices' function extends 'elemIndex', by returning the
-- indices of all elements equal to the query element, in ascending order.
-- >>> elemIndices 'o' "Hello World" -- [4,7]
elemIndices :: Eq a => a -> [a] -> [Int]

-- | The 'find' function takes a predicate and a structure and returns the leftmost element of the structure matching the predicate, or 'Nothing' if there is no such element.
-- >>> find (> 42) [0, 5..] -- Just 45
-- >>> find (> 12) [1..7]   -- Nothing
find :: Foldable t => (a -> Bool) -> t a -> Maybe a

-- | The 'findIndex' function takes a predicate and a list and returns the index of the first element in the list satisfying the predicate, or 'Nothing' if there is no such element.
-- >>> findIndex isSpace "Hello World!" -- Just 5
findIndex      :: (a -> Bool) -> [a] -> Maybe Int

-- | The 'findIndices' function extends 'findIndex', by returning the indices of all elements satisfying the predicate, in ascending order.
-- >>> findIndices (`elem` "aeiou") "Hello World!" -- [1,4,7]
findIndices    :: (a -> Bool) -> [a] -> [Int]

-- ----------------------------------------------------------------------------
-- zip, zipWith, unzip
-- ----------------------------------------------------------------------------
zip :: [a] -> [b] -> [(a, b)]
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]

unzip :: [(a, b)] -> ([a], [b])
unzip3 :: [(a, b, c)] -> ([a], [b], [c])

-- ----------------------------------------------------------------------------
-- Folds and scans
-- ----------------------------------------------------------------------------
scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl1 :: (a -> a -> a) -> [a] -> [a]
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr1 :: (a -> a -> a) -> [a] -> [a]

-- Left-associative fold of a structure but with strict application of the operator. This ensures that each step of the fold is forced to WHNF before being applied, avoiding the collection of thunks that would otherwise occur. This is often what you want to strictly reduce a finite structure to a single strict result (e.g. 'sum'). For a general 'Foldable' structure this should be semantically identical to
-- >>> foldl' f z = List.foldl' f z . toList
foldl' :: Foldable t => (b -> a -> b) -> b -> t a -> b

-- A strict version of foldl1
foldl1' :: (a -> a -> a) -> [a] -> a

{-| Left-associative fold of a structure, lazy in the accumulator. This is rarely what you want, but can work well for structures with efficient RTL sequencing and an operator that is lazy in its left arg. In the case of lists, foldl, when applied to a binary operator, a starting value (typically the left-identity of the operator), and a list, reduces the list using the binary operator, from left to right:

>>> foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn

Note that to produce the outermost application of the operator the entire input list must be traversed. Like all left-associative folds, foldl will diverge if given an infinite list.

If you want an efficient strict left-fold, you probably want to use foldl'. The reason for this is that the latter does not force the inner results (e.g. z `f` x1 in the above example) before applying them to the operator (e.g. to (`f` x2)). This results in a thunk chain O(n) elements long, which then must be evaluated from the outside-in. For a general 'Foldable' structure this should be semantically identical to:

>>> foldl f z = List.foldl f z . toList

A strict fold, which in practice is best performed with foldl':

>>> foldl (+) 42 [1,2,3,4] -- 52

Though the result below is lazy, the input is reversed before prepending it to the initial accumulator, so corecursion begins only after traversing the entire input string:

>>> foldl (\acc c -> c : acc) "abcd" "efgh" -- "hgfeabcd"

A left fold of a structure that is infinite on the right cannot terminate, even when for any finite input the fold just returns the initial accumulator:

>>> foldl (\a _ -> a) 0 $ repeat 1 -- (Hangs forever)
-}
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b

{-| A variant of 'foldl' that has no base case, and thus may only be applied to non-empty structures. This function is non-total and will raise a runtime exception if the structure happens to be empty.

>>> foldl1 f = List.foldl1 f . toList

>>> foldl1 (+) [1..4]   -- 10
>>> foldl1 (+) []       -- *** Exception: Prelude.foldl1: empty list
>>> foldl1 (+) Nothing  -- *** Exception: foldl1: empty structure
>>> foldl1 (-) [1..4]   -- -8
>>> foldl1 (&&) [True, False, True, True]  -- False
>>> foldl1 (||) [False, False, True, True] -- True
>>> foldl1 (+) [1..]  -- (Hangs forever)
-}
foldl1 :: Foldable t => (a -> a -> a) -> t a -> a

{-| Right-associative fold of a structure, lazy in the accumulator.

In the case of lists, 'foldr', when applied to a binary operator, a starting value (typically the right-identity of the operator), and a list, reduces the list using the binary operator, from right to left:

>>> foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)

Note that since the head of the resulting expression is produced by an application of the operator to the first element of the list, given an operator lazy in its right argument, 'foldr' can produce a terminating expression from an unbounded list. For a general 'Foldable' structure this should be semantically identical to:

>>> foldr f z = List.foldr f z . toList

>>> foldr (||) False [False, True, False] -- True
>>> foldr (||) False [] -- False
>>> foldr (\c acc -> acc ++ [c]) "foo" ['a', 'b', 'c', 'd'] -- "foodcba"

⚠️ Applying 'foldr' to infinite structures usually doesn't terminate.

It may still terminate under one of the following conditions:
* the folding function is short-circuiting
* the folding function is lazy on its second argument

⚠️ Short-circuiting

(||) short-circuits on 'True' values, so the following terminates because there is a 'True' value finitely far from the left side:

>>> foldr (||) False (True : repeat False) -- True

But the following doesn't terminate:

>>> foldr (||) False (repeat False ++ [True]) -- (Hangs forever)

⚠️ Laziness in the second argument

Applying 'foldr' to infinite structures terminates when the operator is lazy in its second arg (the initial accumulator is never used in this case, and so could be left 'undefined', but [] is more clear):

>>> take 5 $ foldr (\i acc -> i : fmap (+3) acc) [] (repeat 1) -- [1,4,7,10,13]
-}
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

{-| A variant of 'foldr' that has no base case, and thus may only be applied to non-empty structures. This function is non-total and will raise a runtime exception if the structure happens to be empty.

>>> foldr1 (+) [1..4]  -- 10
>>> foldr1 (+) []      -- Exception: Prelude.foldr1: empty list
>>> foldr1 (+) Nothing -- *** Exception: foldr1: empty structure
>>> foldr1 (-) [1..4]  -- -2
>>> foldr1 (&&) [True, False, True, True]  -- False
>>> foldr1 (||) [False, False, True, True] -- True
>>> foldr1 (+) [1..] -- (Hangs forever)
-}
foldr1 :: Foldable t => (a -> a -> a) -> t a -> a
```
