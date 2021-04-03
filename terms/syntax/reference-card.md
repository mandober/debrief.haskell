# Reference card

https://wiki.haskell.org/Reference_card

Syntax
Comments
Function pattern matching
guards
intermediate and temporary values (saving work)
where
let-in
Declaring types
data
type (type synonymns)
class
instance
Calling functions
Named functions
Operators (and sections)
Useful functions
Lambda Expressions
List Expressions
List Comprehensions
ghc --make
shebang



```hs
-- | Local bindings

-- | where
f x = i * i
    where i = g x

-- | let-in
f x = let i = g x
      in i * i

-- | Declaring types

-- | data
data List = Cons Int List
          | Nil
          deriving (Eq, Show, Ord)


-- | type synonymn
type String = [Char]
type Name = TypeValue


-- | class
class Check a where
    test :: a -> Bool
    force :: a -> a

-- | instance
instance Show List where
    show x = "No show"


-- | Calling functions

-- | Named functions
myFunc :: Int -> Int -> Int
result = myFunc 1 2
result = 1 `myFunc` 2
result = (myFunc 1) 2
result = (`myFunc` 2) 1


-- | Operators and sections
(+) :: Int -> Int -> Int
result = 1 + 2
result = (+) 1 2
result = (1 +) 2
result = (+ 2) 1


-- | Useful functions
myFunc 1 2 == (flip myFunc) 2 1
(f . g) x == f (g x)
f (a+b) == f $ a+b

-- | Lambda Expressions
myFunc = (\ a b -> a + b)
result = map (\x -> head x) xs


-- | Enumerations
[1..] = [1,2,3,4,5,6...]
[1..5] = [1,2,3,4,5]
[1,3..5] = [1,3,5]


-- | List Comprehensions
[ x*x | x <- [1..3] ] ==> [1,4,9]
[ (x, y) | x < - [1..3], y <- "ab"] ==> 
    [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]
[ (x,y) | x <- [1..4], even x, y <- "ab" ] ==>
    [(2,'a'),(2,'b'),(4,'a'),(4,'b')]
map f xs ==> [ f x | x <- xs ]
filter p xs ==> [ x | x <- xs, p x ]



-- | Hello World
main = putStrLn "Hello World"

-- | Snippets
fst3 :: (a, b, c) -> a
snd3 :: (a, b, c) -> b
thd3 :: (a, b, c) -> c
fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x


ordPair :: Ord a => a -> a -> (a, a)
ordPair x y = if x < y then (x, y) else (y, x)


lenXX# :: [a] -> Bool
lenEq0 = null
lenNe0 = not . null
lenEq1 [x] = True
lenEq1  _  = False
lenGt1 [x] = False
lenGt1 [ ] = False
lenGt1  _  = True


sortUnique :: Ord a => [a] -> [a]
sortUnique []  = []
sortUnique [x] = [x]
sortUnique xs = mergeUnique (sortUnique a) (sortUnique b)
    where (a,b) = split xs


split :: [a] -> ([a], [a])
split []  = ([], [])
split [a] = ([a], [])
split (a:b:xs) = (a:as, b:bs)
    where (as,bs) = split xs


mergeUnique :: Ord a => [a] -> [a] -> [a]
-- | Precondition:
-- | isUnique(#1) && isUnique(#2)
nergeUnique a  [] = a
mergeUnique [] b  = b
mergeUnique (a:as) (b:bs) =
    case compare a b of
        EQ -> a: mergeUnique as bs
        LT -> a: mergeUnique as (b:bs)
        GT -> b: mergeUnique (a:as) bs


fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x'
    where x' = f x



{-
Command lines

For hmake
    hmake Test
    hmake -realclean Test
Where Test is the name of the executable you want to build
i.e. where Test.hs contains the main function.



For ghc --make
    ghc --make MainModule.hs -o ModuleExec
    ghc --make Module.hs
Where ModuleExec is the name of the output binary you want to make (if main
is exported). Module.o will be output for Module.hs, if main is not exported


Others
    runhaskell Test.hs
    echo main | ghci -v0 Test.hs
    #! notation
You can also just make single Haskell main modules executable
using a combination of runhaskell and #! notation:

    #!/usr/bin/env runhaskell
    main = putStrLn "hello"
Save this to a .hs file and then make this file executable:

    chmod +x Test.hs
You can now execute this as a normal script file:
    $ ./Test.hs
-}
```
