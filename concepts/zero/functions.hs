{-
    Functions
    =========

    Functions are given as (multipart) equations.
-}


-- functions are functors too
instance Functor ((->) r) where
  fmap f g = f . g
-- fmap === composition
instance Functor ((->) r) where
  fmap = (.)


-- The `->` operator is RIGHT associative:
add :: Int -> (Int -> Int)
-- so it is enough to just write:
add :: Int -> Int -> Int
add x y = x + y

-- fn application is LEFT associative:
(add 5) 3
-- is the same as
add 5 3


-- if..else
head' :: [a] -> a
head' xs = if   null (tail xs)
           then error "list too short"
           else head (tail xs)


-- if..else
'drop :: Int -> [a] -> [a]
'drop n xs = if   n <= 0 || null xs
             then xs
             else 'drop (n - 1) (tail xs)


-- guards + pattern mathing
niceDrop :: (Ord t, Num t) => t -> [a] -> [a]
niceDrop n xs | n <= 0 = xs
niceDrop _ []          = []
niceDrop n (_:xs)      = niceDrop (n - 1) xs


-- guards + where clause
lend :: Int -> Int
lend a b
    | a <= 0      = Nothing
    | a > r * 0.5 = Nothing
    | otherwise   = Just n
   where r = 100
         n = b - a

-- let..in expr + if..else
lend z b = let r = 100
               n = b - z
           in  if b < r
               then Nothing
               else Just n






-- comp2 is binary func taking 2 args f g, returning z
-- comp2 :: f -> g -> z
-- comp2 :: f -> (g -> z)
--    f :: (a -> b)         unary
--    g :: (b -> b -> c)    binary
--out z :: (a -> a -> c)    binary

comp2 :: (a -> b) -> (b -> b -> c) -> (a -> a -> c)
comp2 f g = (\x y -> g (f x) (f y))

-- is this a partial definition of comp2'
comp2' f = (\x y -> add (f x) (f y))

-- not really, this is another func, but to get
-- the desired result just partially apply comp2:
comp2' f = comp2 f add



(+1) 3 -- 4
(3+) 5 -- 8

(/2) 1 -- 0.5
(2/) 1 -- 2

2 - 1 -- 1
(-) 2 1 -- 1
(-2) 1 -- error


curry :: ((a,b)->c) -> a->b->c
curry f a b = f (a,b)

uncurry :: (a->b->c) -> ((a,b)->c)
uncurry f (a,b)= f a b



-- LC:
λx.x            -- I
λab.a           -- K
(\a -> a) b     -- applying func I to arg b: I b

-- Haskell
\x -> x         -- I == id
\a b -> a       -- K == const
let a = b in c  -- let analog of: I b


-- In (older) ghci use let:
let k = (\x y -> x)

-- or define a func using expr:
let id x = x




-- Lambdas beneath let

-- From let expressions to lambda expressions


let a = b in c
-- means: bind b to a in the expr c
-- so in lamnda expr, b is arg, a is param, and
-- c is lambda body, which is here just a
(\a -> a) b

let x = 10 in x + 9001
(\x -> x + 9001) 10



-- From where to lambda

c where a = b
(\a -> a) b

-- so:
let x = 10 in x + 9001
-- is eq:
identifier = x + 9001
   where x = 10
-- is eq:
(\x -> x + 9001) 10





let x = 3; y = 1000 in x * 3 + y
op1 = x * 3 + y where
  x = 3
  y = 1000

let y = 10; x = 10 * 5 + y in x * 5
op2 = x * 5 where
  y = 10
  x = 10 * 5 + y

let x = 7; y = negate x; z = y * 10 in z / x + y
op3 = z / x + y where
  x = 7
  y = negate x
  z = y * 10
