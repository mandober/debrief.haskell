# Functions

<!-- TOC -->

- [Concepts](#concepts)
- [Debrief](#debrief)
- [Function type ctor](#function-type-ctor)
- [Infix operators](#infix-operators)
- [Associativity and precedence](#associativity-and-precedence)
- [Partial application](#partial-application)
  - [Example with composition](#example-with-composition)
- [Sectioning](#sectioning)
- [HOF](#hof)
- [Laws for quotients and remainders](#laws-for-quotients-and-remainders)
- [Lambdas](#lambdas)
- [Lambdas beneath let](#lambdas-beneath-let)
- [From where to lambda](#from-where-to-lambda)
- [The $ operator](#the--operator)

<!-- /TOC -->

## Concepts

Function concepts:
- purity
- unary
- currying, auto-currying, uncurrying
- segmentation
- function definition i.e. abstraction
- signature
- input/output value/type, vs tuples


## Debrief

* functions are pure, unary (auto-curried), referentially transparent
* *function abstraction* (func IO)
  - every function has a single input and a single output value
  - tuples blur this concept
  - func's type is given by its **signature**:     
    `flip :: (a -> b -> c) -> b -> a -> c`
  - tuple is a single value but repr multiple; `curry` and `uncurry` are 2 hofs that convert to/from functions accepting tuples and unary funcs.
    - builtin (curried) func: `const :: a -> b -> a`
    - uncurried const funcs : `uncurry const :: (c, b) -> c`
    - func taking a tuple   : `snd :: (a, b) -> b`
    - curried snd func      : `curry snd :: a -> c -> c`
  - input one param, output one value, both of certain type:    
    `(a -> b)` although the type may be the same (a -> a)
  - funcs are unary, but n-ary funcs are repr as a seq of unary funcs:  
    `(a -> b -> c)` is a func that takes arg a and returns func that takes arg b and returns c (where a, b, c are types of values, same or diff) 
- *function application*
  - function application is  (denoted as space-separated juxtaposition)
  - func application has the highest precedence, 0, `infixr 0` (levels: 0-9)
  - function application associates to the right, `infixr 0`
  - `a -> a -> a` is in fact `a -> (a -> a)`
- all functions are in fact *unary* but they are *auto-curried*
- functions are *1st class values* (may be passed to and returned from funcs)
- function may be named i.e. bound to an identifier
- function may be a value if an expr; funcs may be members of lists and tuples
- the arrow, `(->)`, is the *type ctor* for funcs (they lack data ctors)




## Function type ctor

```hs
> :info (->)

data (->) t1 t2                       -- Defined in ‘GHC.Prim’
infixr 0 `(->)`

instance Monad       ((->) r)         -- Defined in ‘GHC.Base’
instance Functor     ((->) r)         -- Defined in ‘GHC.Base’
instance Applicative ((->) a)         -- Defined in ‘GHC.Base’
instance Monoid b => Monoid (a -> b)  -- Defined in ‘GHC.Base’
```



<!-- #region Functions -->

In ghci use `let`, but not in source file.
> let triple x = x * 3

In a source file, def a func like this:
```hs
triple x = x * 3
```
Func name must be lowercase.

Apply a func to the arg:

```hs
triple 4
```

The expression `triple 4` is a *redex*, it is an expr in *canonical or normal form* when it reaches the number 12 (because the value 12 cannot be reduced further).


## Infix operators
Func default to being in prefix position. You can sometimes use functions in an infix or prefix style by wrapping its name in acute quotes.
> div 10 4
> 10 `div` 4

<!-- #endregion -->

## Associativity and precedence
<!-- #region Associativity -->
When you ask GHCi for the :info about an operator or function, it provides the type information and tells you whether it's an infix operator with precedence.

> :info (*)
infixl 7 *

- infixl means infix operator, left associative
- 7 is the precedence: higher is applied first, on a scale of 0-9.

infixl is left-ass so    
> 2 * 3 * 4    
is the same as     
> (2 * 3) * 4


Comma is used to assign left-associativity and precedence (6) for an inquiry about addition and subtraction operators:
> :info (+) (-)
infixl 6 +, -

Exponentiation associate to the right, but it is not associative and thus makes a prime candidate for demonstrating left vs. *right-associativity*:

> :info (^)
infixr 8 ^

> 2 ^ 3 ^ 4
is eval as
> 2 ^ (3 ^ 4)


> 2 ^ 3 ^ 4
2417851639229258349412352
> 2 ^ (3 ^ 4)
2417851639229258349412352
> (2 ^ 3) ^ 4
4096
<!-- #endregion -->


## Partial application
<!-- #region Sectioning -->

https://wiki.haskell.org/Partial_application

motto: **Functions are not partial, but you can partially apply a function**

Partial application in Haskell involves passing less than the full number of arguments to a function that takes multiple arguments.

```hs
add :: Int -> Int -> Int
add x y = x + y
addOne = add 1
```

The `->` operator is right associative, and function application is left associative, meaning the type signature of `add` actually looks like this:

```hs
add :: Int -> Int -> Int
-- The `->` operator is right associative:
add :: Int -> (Int -> Int)

add 5 3
-- function application is left associative:
(add 5) 3
```
This means that add actually takes one argument and returns a function that takes another argument and returns an Int.


### Example with composition

```hs

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
```





## Sectioning
Sectioning allows you to pass around partially-applied functions. With commutative functions, such as addition, it makes no difference if you use (+1) or (1+) because the order of the arguments won’t change the result. If you use sectioning with a function that is not commutative, the order matters.

Subtraction, (-), is a special case. Enclosing a value inside the parentheses with the minus indicates to GHCi that it’s the argument of a function. Because the minus function represents *negation*, not *subtraction*, when it’s applied to a single argument, GHCi does not know what to do with that, and so it returns an error message. Here, minus is a case of syntactic overloading disambiguated by how it is used.


```hs
(+1) 3 -- 4
(3+) 5 -- 8

(/2) 1 -- 0.5
(2/) 1 -- 2

2 - 1 -- 1
(-) 2 1 -- 1
(-2) 1 -- error
```

To use sectioning for subtraction, only the first argument may be partially applied:
> let x = 5
> let y = (1 -)
> y x
-4


## HOF
A higher-order function is a function that takes other functions as arguments or returns a function as result.

Two other common ones are curry, uncurry. A possible implementation of these is:

curry :: ((a,b)->c) -> a->b->c
curry f a b = f (a,b)

uncurry :: (a->b->c) -> ((a,b)->c)
uncurry f (a,b)= f a b


<!-- #endregion -->



## Laws for quotients and remainders

<!-- #region Laws -->

The quot/rem, div/mod class methods satisfy these laws if y is non-zero:

```hs
(x `quot` y)*y + (x `rem` y) == x
(x `div`  y)*y + (x `mod` y) == x
```

* `quot` is integer division truncated toward zero
* `div` is integer division truncated toward negative infinity

The `div` function is often the more natural one to use, whereas the `quot` function corresponds to the machine instruction on modern machines, so it's somewhat more efficient.


## Lambdas

Lambda expressions are similar to their form in Lambda Calculus, only Haskell uses backslash `\` instead of `λ`; also Haskell's standard function ctor, `->`, is used in the place of LC's dot, `.`. The LC's shorthand, i.e. writting `λab.a` instead of `λa.λb.a`, also has a parallel in Haskell.


```hs
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
```


## Lambdas beneath let

From let expressions to lambda expressions

```hs
let a = b in c
-- means: bind b to a in the expr c
-- so in lamnda expr, b is arg, a is param, and
-- c is lambda body, which is here just a
(\a -> a) b

let x = 10 in x + 9001
(\x -> x + 9001) 10
```


## From where to lambda

We can do a similar translation with where, although there are minute fifferences that we will not address. 

again, a is param, b is arg and c is the body.


```hs
c where a = b
(\a -> a) b

-- so:
let x = 10 in x + 9001
-- is eq:
identifier = x + 9001
   where x = 10
-- is eq:
(\x -> x + 9001) 10
```


```hs
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
```

<!-- #endregion -->


## The $ operator

The dollar operator is defined as an infix operator with the lowest possible precedence:

> :info ($)
($) :: (a -> b) -> a -> b
infixr 0 $

The ($) operator is a convenience for when you want to express something with fewer parentheses:

> (2^) $ 2 + 2
16
> (2^) (2 + 2)
16

*Evaluate everything to the right of ($) first*.

    f      g      x
> (2^) $ (+2) $ 3 * 2
  (2^) $ (+2) 6
  (2^) 8
  256

but:

    f      g  x1  x2
> (2^) $ (+2) 3 * 2 
  (2^) $  5     * 2
  (2^) 10
  1024
  
Function application has precedence over mult.


> (2^) $ 2 + 2 $ (*30)
A rather long and ugly type error about trying to
use numbers as if they were functions follows.
