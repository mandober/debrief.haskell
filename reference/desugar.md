# Desugaring Haskell to Core

http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html

Haskell's surface syntax is first desugared then transformed into a small set of basic forms, in the end being entirely converted into the core language. Haskell is based on lambda calculus, so its Core language is lambda calculus extended with types (augmented typed lambda calculus); namely, it is similar to the polymorphic System F.

Haskell's Core language is small, but all Haskell forms can be desugared to it.

Haskell                             | Core
------------------------------------|-------------------------------------
function definition                 | lambda abstraction
sections (3 forms)                  | lambda abstraction
polyadic function                   | curryied lambda abstraction
polyadic lambda                     | curryied lambda abstraction
infixed functions and operators     | all to prefix position
if-then-else construct              | case expression
pattern matching                    | case expression
type class                          | implict dictionary passing
infixed operators                   | prefixed section form
infixed ticked functions            | prefixed (sans tick) form



- lambdas
- function application
- algebraic data types
- case expressions
- recursive let bindings
- type classes and specialization
- foreign function calls




## if-then-else

* if-then-else construct is equivalent to a `case` expression:
* this construct is not needed in Haskell at all, moreover since you can define you own conditionals. Although it complicates fixity resolution, in the end it was decided it should be kept as a familiar straw for beginners' sake.

```hs
if b then e1 else e2

-- equivalent to
case b of
  True  -> e1
  False -> e2
```


## Functions

Lambda abstraction:
- form in LC: `(λa.λb.a)`
- in Haskell: `(\ a -> \ b -> a)`
  - `λ` is replaced with the backslash char, `\` (being kind of similar)
  - space after the backslash is optional, but the compiler prefers it
    - `(\a -> \b -> a)` ≡ `(\ a -> \ b -> a)`
  - shorthand parameter form is allowed
    - in LC: `(λa.λb.a)` ≡ `(λab.a)`
    - in HS: `(\a -> \b -> a)` ≡ `(\a b -> a)`




* Polyadic functions (including lambda forms) are auto-curryied
* Functions are equivalent (converted) to lambdas
* Lambda abstraction allows param declaration shorthand and PM
* Infix operators desugar to their prefix (section) forms
* Infix (ticked) functions desugar to their prefix (sans tick) form
* operators are distinct from functions by their symbolic names


```hs
-- Haskell's lambda abstraction form
\ x -> \ (a,b) -> BODY
-- ...allows shorthand parameter form (space after \ is optional)
\x \(a,b) -> BODY

-- function definition (with PM)
f x y z = BODY
-- ...is equivalent to
f = \ x y z -> BODY
-- ...which in turn desugars to
f = \ x -> \ y -> \ z -> BODY

-- polyadic lambda abstraction
\x y z -> BODY
-- ...is equivalent to a curried lambda abstraction
\x -> \y -> \z -> BODY

-- polyadic function
f x y z = BODY
-- ...is equivalent to
f = \x y z -> BODY
-- ...which in turn desugars to
f = \x -> \y -> \z -> BODY

-- sections are desugared to lambdas
(+)  == \x y -> x + y
(3*) == \y -> 3 * y
(^5) == \x -> x ^ 5

-- sections with infixed functions
(`f` 1)
-- ...desugars to
\x -> x `f` 1
-- ...then to
\x -> f x 1

-- functions in infix position must use backticks
x `op` y
-- ...desugars to
op x y

-- operators
x + y
-- ...desugar to the prefix position form
(+) x y
```


## Operators

Operators are just infix functions of two arguments that don't need backticks. You can write them in prefix form by surrounding them with parentheses:

x + y

-- ... desugars to:
(+) x y

The compiler distinguishes operators from functions by reserving a special set of punctuation characters exclusively for operators.

## Operator parameters

The parentheses trick for operators works in other contexts, too. You can bind parameters using operator-like names if you surround them with parentheses:

let f (%) x y = x % y
in  f (*) 1 2

-- ... desugars to:
(\(%) x y -> x % y) (*) 1 2

-- ... reduces to:
1 * 2

## Operator sections

You can partially apply operators to just one argument using a section:

(1 +)

-- desugars to:
\x -> 1 + x

This works the other way, too:

(+ 1)

-- desugars to:
\x -> x + 1

This also works with infix functions surrounded by backticks:

(`f` 1)

-- desugars to:
\x -> x `f` 1

-- desugars to:
\x -> f x 1

## Pattern matching
Pattern matching on constructors desugars to case statements:

f (Left  l) = eL
f (Right r) = eR

-- ... desugars to:
f x = case x of
    Left  l -> eL
    Right r -> eR

Pattern matching on numeric or string literals desugars to equality tests:

f 0 = e0
f _ = e1

-- ... desugars to:
f x = if x == 0 then e0 else e1

-- ... desugars to:
f x = case x == 0 of
    True  -> e0
    False -> e1

## Non-recursive let and where

Non-recursive lets are equivalent to lambdas:

let x = y in z

-- ... is equivalent to:
(\x -> z) y

Same thing for where, which is identical in purpose to let:

z where x = y

-- ... is equivalent to:
(\x -> z) y

Actually, that's not quite true, because of let generalization, but it's close to the truth.

Recursive let / where cannot be desugared like this and should be treated as a primitive.

## Top-level functions

Multiple top-level functions can be thought of as one big recursive let binding:

f0 x0 = e0

f1 x1 = e1

main = e2


-- ... is equivalent to:
main = let f0 x0 = e0
           f1 x1 = e1
       in  e2


In practice, Haskell does not desugar them like this, but it's a useful mental model.

## Imports

Importing modules just adds more top-level functions. Importing modules has no side effects (unlike some languages), unless you use Template Haskell.

## Type-classes

Type classes desugar to records of functions under the hood where the compiler implicitly threads the records throughout the code for you.

class Monoid m where
    mappend :: m -> m -> m
    mempty :: m

instance Monoid Int where
    mappend = (+)
    mempty  = 0

f :: Monoid m => m -> m
f x = mappend x x


-- ... desugars to:
data Monoid m = Monoid
    { mappend :: m -> m -> m
    , mempty  :: m
    }

intMonoid :: Monoid Int
intMonoid = Monoid
    { mappend = (+)
    , mempty  = 0
    }

f :: Monoid m -> m -> m
f (Monoid p z) x = p x x

... and specializing a function to a particular type class just supplies the function with the appropriate record:

g :: Int -> Int
g = f

-- ... desugars to:
g = f intMonoid


## do notation

* A two-line do block desugars to the infix (>>=) operator:

do x <- m
   e

-- ... desugars to:
m >>= (\x ->
e )


* For a one-line do block, you can just remove the do:

main = do putStrLn "Hello, world!"

-- ... desugars to:
main = putStrLn "Hello, world!"


* do notation of more than two lines is equivalent to multiple nested dos:

do x <- mx
   y <- my
   z

-- ... is equivalent to:
do x <- mx
   do y <- my
      z

-- ... desugars to:
mx >>= (\x ->
my >>= (\y ->
z ))


* Non-recursive let in a do block desugars to a lambda:

do let x = y
   z

-- ... desugars to:
(\x -> z) y


## ghci

The ghci interactive REPL is analogous to one big do block (with lots and lots of caveats):

$ ghci
>>> str <- getLine
>>> let str' = str ++ "!"
>>> putStrLn str'

-- ... is equivalent to the following Haskell program:
main = do
    str <- getLine
    let str' = str ++ "!"
    putStrLn str'

-- ... desugars to:
main = do
    str <- getLine
    do let str' = str ++ "!"
       putStrLn str'

-- ... desugars to:
main =
    getLine >>= (\str ->
    do let str' = str ++ "!"
       putStrLn str' )

-- ... desugars to:
main =
    getLine >>= (\str ->
    (\str' -> putStrLn str') (str ++ "!") )

-- ... reduces to:
main =
    getLine >>= (\str ->
    putStrLn (str ++ "!") )


## List comprehensions

List comprehensions are equivalent to do notation:

[ (x, y) | x <- mx, y <- my ]

-- ... is equivalent to:

do x <- mx
   y <- my
   return (x, y)

-- ... desugars to:
mx >>= (\x -> my >>= \y -> return (x, y))

-- ... specialization to lists:
concatMap (\x -> concatMap (\y -> [(x, y)]) my) mx

The real desugared code is actually more efficient, but still equivalent.

The *MonadComprehensions* language extension generalizes list comprehension syntax to work with any Monad. For example, you can write an IO comprehension:

>>> :set -XMonadComprehensions
>>> [ (str1, str2) | str1 <- getLine, str2 <- getLine ]
Line1<Enter>
Line2<Enter>
("Line1", "Line2")

## Numeric literals

Integer literals are polymorphic by default and desugar to a call to fromIntegral on a concrete Integer:

1 :: Num a => a

-- desugars to:
fromInteger (1 :: Integer)

Floating point literals behave the same way, except they desugar to fromRational:

1.2 :: Fractional a => a

-- desugars to:
fromRational (1.2 :: Rational)


## IO

You can think of IO and all foreign function calls as analogous to building up a syntax tree describing all planned side effects:

main = do
    str <- getLine
    putStrLn str
    return 1


-- ... is analogous to:
data IO r
    = PutStrLn String (IO r)
    | GetLine (String -> IO r)
    | Return r

instance Monad IO where
    (PutStrLn str io) >>= f = PutStrLn str (io >>= f)
    (GetLine k      ) >>= f = GetLine (\str -> k str >>= f)
    Return r          >>= f = f r

main = do
    str <- getLine
    putStrLn str
    return 1

-- ... desugars and reduces to:
main =
    GetLine (\str ->
    PutStrLn str (
    Return 1 ))


This mental model is actually very different from how IO is implemented under the hood, but it works well for building an initial intuition for IO.

For example, one intuition you can immediately draw from this mental model is that order of evaluation in Haskell has no effect on order of IO effects, since evaluating the syntax tree does not actually interpret or run the tree. The only way you can actually animate the syntax tree is to define it to be equal to main.




## References

How to desugar Haskell code
http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html

Where Do Monads Come From?
https://golem.ph.utexas.edu/category/2012/09/where_do_monads_come_from.html

Critique of Mark Weber's paper on Nerves of categories
https://golem.ph.utexas.edu/category/2008/01/mark_weber_on_nerves_of_catego.html

Conservative groupoids recognize only regular languages
http://www.frontiersinai.com/turingfiles/March/03Tessonconservatives-LATA2012.pdf

Monads with arities and their associated theories
Clemens Berger, Paul-andre Mellies, Mark Weber
2011
https://arxiv.org/pdf/1101.3064.pdf
