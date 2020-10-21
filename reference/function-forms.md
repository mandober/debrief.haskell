# Callables

Callables
- functions (named functions, functions with alphabetic names)
- operators (symbolic functions, functions with symbolic names)
- lambda expressions

*Functions*
- named functions, functions with alphabetic names
- function definition
- function application
  - the highest precedence (among common operations)
- default to prefix position
- for infix form, a function needs to be wrapped in ticks
- for infix form, a function needs to be at least binary


*Operators*
- symbolic functions, functions with symbolic names

*Lambdas*
- anonymous functions
- lambda abstraction
- lambda expression

*Sections*
- sections are special forms with parenthesised callables
- operators are sectioning-friendly
- backticked functions may also be sectioned
- binary 
- sometimes hard to tell apart parenthesised callable from a section

Forms:
- named function definition
- anonymous lambda abstraction/expression
- section
- parenthesised callable
- hard to tell apart section and parenthesised callable

Syntactic concerns:
- position: prefix suffix
- wraping a function in backticks
- wraping an operator in parens

Position
- prefix:
  - default for functions
  - prefixed operators use parens (section form)
- infix:
  - default for sections
  - infixed functions use backticks



```hs
-- canonical, top-level, function definitions
add :: Int -> Int -> Int
add x y = x + y

sum3 :: Int -> Int -> Int -> Int
sum3 x y z = x + y + z

-- ----------------------------------------------------------------------------
-- infixed functions need to be at least binary
--
-- sectioning a unary function gives impossible types:
> :t (`succ` 7)   :: (Enum (t1 -> t2), Num t1) => (t1 -> t2) -> t2
> :t (4 `succ`)   :: (Enum (t1 -> t2), Num (t1 -> t2)) => t1 -> t2
> :t (4 `succ` 7) :: (Enum (t1 -> t2), Num t1, Num (t1 -> t2)) => t2
> :t (`succ`)     :: error: parse error on input ')'


-- sectioning a ternary function is fine:
foldr (+) 0         [0..5] :: (Num b, Enum b) => b -- 15

-- 1st param goes left, 2nd goes right, 3rd is left outside
((+) `foldr` 0)     [0..5] :: (Num b, Enum b) => b -- 15

-- fixing the 2nd arg
(`foldr` 0) (+)     [0..5] :: (Num b, Enum b) => b -- 15

-- fixing the 1st arg
((+) `foldr`) 0 (+) [0..5] :: (Num b, Enum b) => b -- 15
-- somewhat peculiar: the add fn remains tickless
(add `foldr`) 0 (+) [0..5] :: (Num b, Enum b) => b -- 15

-- fixing both 1st and 2nd arg
((`sum3` 4) `foldr` 0) [0..5] :: (Num b, Enum b) => b -- 35


-- partial application...
> :t foldr           :: Foldable t => (a -> b -> b) -> b -> t a -> b
> :t (`foldr`)       :: error: parse error on input ')'
-- ...1st param goes left, 2nd goes right:
> :t ((+) `foldr`)   :: (Foldable t, Num b) => b -> t b -> b
> :t (`foldr` 0)     :: (Foldable t, Num b) => (a -> b -> b) -> t a -> b
> :t (`foldr` 0) (+) :: (Foldable t, Num b) => t b -> b
> :t ((+) `foldr` 0) :: (Foldable t, Num b) => t b -> b
```
