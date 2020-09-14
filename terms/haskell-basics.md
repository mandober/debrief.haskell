# Haskell Basics

<!-- TOC -->

- [Functions](#functions)
- [Types](#types)
- [Values](#values)
- [Pattern matching](#pattern-matching)
- [Recursion](#recursion)
- [Laziness](#laziness)
- [Higher-Kinded Types](#higher-kinded-types)
- [Typeclasses](#typeclasses)
- [Operators](#operators)
- [Monads](#monads)
- [Applicatives](#applicatives)
- [Monoids](#monoids)
- [Deriving](#deriving)
- [IO](#io)
- [Monad Transformers](#monad-transformers)
- [Text](#text)
- [Cabal & Stack](#cabal--stack)
- [Resources](#resources)

<!-- /TOC -->


## Functions


Although it is not required, functions are commonly given explicit *signatures* by programmers, which show their type clearly. Letting the compiler infer their signature may sometimes lead to the selection of much broader types then intended.

- -> used in type annotations for functions: a -> b -> c
- means func takes value of type `a` and `b` and returns value of type `c`
- -> has right precedence: a -> b -> c is actually a -> (b -> c)


- `() :: ()` unit type that has only one value: `()`
- `IO` is a monad that manages side-effects (print to stdout)
- shared type sig: `(+), (-), (*) :: a -> a -> a`


```hs
add :: Integer -> Integer -> Integer
add x y = x + y

intdiv :: Integer -> Integer -> Integer
intdiv _ 0 = error "Division by zero"
intdiv n d = n `div` d

intdiv :: Integer -> Integer -> Maybe Integer
intdiv _ 0 = Nothing
intdiv n d = Just (n `div` d)
```

Functions are *auto-curried* so that when a poliadic function receives less arguments, a partially applied function is returned, and so on until all arguments are collected and the computation can be performed.

```hs
-- ternary function that accepts 3 (bounded) integers and sums them
ternary :: Int -> Int -> Int -> Int
ternary x y z = x + y + z

-- partially applied unary function
partial :: Int -> Int
partial = ternary 2 3
```

Functional *composition* is the primary tool to process data and perform computation; most functions are *higher-order functions* (HOFs). Since composition is so often performed, it is identified by a single symbol, `.` (dot). Composing functions g and f is denoted as `g . f`.

```hs
succ :: Integer -> Integer
succ n = n + 1

sq :: Integer -> Integer
sq n = n*n

-- composing: succ . sq === \n -> succ (sq n)
succsq :: Integer -> Integer
succsq = succ . sq

-- produces an infinite list
iterate :: (a -> a) -> a -> [a]
iterate f x = x : (iterate f (f x))
```



## Types

Haskell has algebraic types, primarily, sum types and product types.

A **sum type** is a compound type that defines several subtypes of which only one can be active at the time (somewhat like C's `union`, the same as Rust's `enum`). Each subtype has a *type constructors* under the encompassing *type* that is also *data constructor* (and introduced using the keyword `data`). Data ctors and type ctors live in different namespaces so they can have the same name. To discriminate between the subtypes to determine which one is active, use *pattern matching* on type constructors.

Boolean is an ordinary non-special (sum) type in Haskell, that has two nullary ctors, thus degrading the sum type to a C enum-like type. To see what an unknown variable `b :: Bool` holds, match it against either type ctor (writing the code for both code paths).





```hs
-- Boolean is a sum type with 2 nullary ctors
data Bool = True | False

-- "Sum" is a sum type with 3 ctors: nullary, unary and binary
data Sum = Nevermind | Offset Float | Point Int Int

-- Product type
data Product = Product Int Bool

-- Record product type
data Record = Person { name :: String , age :: Int, sex :: Bool }

-- name :: Record -> String
-- age :: Record -> Int
```

A **product type** combines multiple fields into the same type. Unlike a sum type, all fields have to be initialized when defining the product type. They are like objects in JS or structs in C or Rust. Subtypes of sum types are in fact product types, from nullary to polyadic.

**Records** are a variant of the product type that has a convenient set of functions called *selectors* that provide a name for a field, but also to extract a field's value.

```hs
data Prod = Prod { a :: Int , b :: Bool }

-- a :: Prod -> Int
-- b :: Prod -> Bool
```

Sums and products can be combined.

```hs
data T1
  = A Int Int
  | B Bool Bool
```

The fields of a datatype may be *parameterized*, in which case the type depends
on the specific types the fields are instantiated with.

```hs
data Maybe a = Nothing | Just a
```


## Values

A list is a homogeneous, inductively defined sum type of linked cells parameterized over the type of its values.

```hs
data List a = Nil | Cons a (List a)

a = [1,2,3]
a = Cons 1 (Cons 2 (Cons 3 Nil))
```

List have special value-level syntax:

```hs
(:) = Cons
[]  = Nil
```

```hs
(1 : (2 : (3 : []))) = [1,2,3]
```

A tuple is a heterogeneous product type parameterized over the types of its two values.

Tuples also have special value-level syntax.

```hs
data Pair a b = Pair a b
```

```hs
a = (1,2)
a = Pair 1 2
```

```hs
(,) = Pair
```


## Pattern matching

Pattern matching allows us to discriminate on the constructors of a datatype,
mapping separate cases to separate code paths and binding variables for each of
the fields of the datatype.

```hs
data Maybe a = Nothing | Just a

maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f Nothing  = n
maybe n f (Just a) = f a
```

Top-level pattern matches can always be written identically as case statements.

```hs
maybe :: b -> (a -> b) -> Maybe a -> b
maybe n f x = case x of
  Nothing -> n
  Just a  -> f a
```

Wildcards can be placed for patterns where the resulting value is not used.

```hs
const :: a -> b -> a
const x _ = x
```

Subexpression in the pattern can be explicitly bound to variables scoped on the
right hand side of the pattern match.

```hs
f :: Maybe (Maybe a) -> Maybe a
f (Just x @ (Just _)) = x
```

List and tuples have special pattern syntax.

```hs
length :: [a] -> Int
length []     = 0
length (x:xs) = 1 + (length xs)
```

```hs
fst :: (a, b) -> a
fst (a,b) = a
```

Patterns may be guarded by predicates (functions which yield a boolean). Guards
only allow the execution of a branch if the corresponding predicate yields True.

```hs
filter :: (a -> Bool) -> [a] -> [a]
filter pred []     = []
filter pred (x:xs)
  | pred x         = x : filter pred xs
  | otherwise      =     filter pred xs

```

## Recursion

In Haskell all iteration over data structures is performed by recursion.
Entering a function in Haskell does not create a new stack frame, the logic of
the function is simply entered with the arguments on the stack and yields result
to the register. In the case where a function returns an invocation of itself
invoked in the *tail position* the resulting logic is compiled identically to
``while`` loops in other languages, via a ``jmp`` instruction instead of a
``call``.

```hs
sum :: [Int] -> Int
sum ys = go ys 0
  where
    go (x:xs) i = go xs (i+x)
    go [] i = i
```

Functions can be defined to recurse mutually on each other.

```hs
even 0 = True
even n = odd (n-1)

odd 0 = False
odd n = even (n-1)
```


## Laziness

A Haskell program can be thought of as being equivalent to a large directed
graph. Each edge represents the use of a value, and each node is the source of a
value. A node can be:

* A *thunk*, i.e., the application of a function to values that have not been
  evaluated yet
* A thunk that is currently being evaluated, which may induce the evaluation of
  other thunks in the process
* An expression in *weak head normal form*, which is only evaluated to the
  outermost constructor or lambda abstraction

The runtime has the task of determining which thunks are to be evaluated by the
order in which they are connected to the main function node. This is the essence
of all evaluation in Haskell and is called *graph reduction*.

Self-referential functions are allowed in Haskell. For example, the following
functions generate infinite lists of values. However, they are only evaluated
up to the depth that is necessary.

```hs
-- Infinite stream of 1's
ones = 1 : ones

-- Infinite count from n
numsFrom n = n : numsFrom (n+1)

-- Infinite stream of integer squares
squares = map (^2) (numsfrom 0)
```

The function ``take`` consumes an infinite stream and only evaluates the values
that are needed for the computation.

```hs
take :: Int -> [a] -> [a]
take n _  | n <= 0 =  []
take n []          =  []
take n (x:xs)      =  x : take (n-1) xs
```

```hs
take 5 squares
-- [0,1,4,9,16]
```

This also admits diverging terms (called *bottoms*), which have no normal form.
Under lazy evaluation, these values can be threaded around and will never diverge
unless actually forced.

```hs
bot = bot
```

So, for instance, the following expression does not diverge since the second
argument is not used in the body of ``const``.

```hs
const 42 bot
```

The two bottom terms we will use frequently are used to write the scaffolding
for incomplete programs.

```hs
error :: String -> a
undefined :: a
```


## Higher-Kinded Types

The "type of types" in Haskell is the language of kinds. Kinds are either an
arrow (``k -> k'``) or a star (``*``).


The kind of Int is ``*``, while the kind of ``Maybe`` is ``* -> *``. Haskell
supports higher-kinded types, which are types that take other types and
construct a new type. A type constructor in Haskell always has a kind which
terminates in a ``*``.

```hs
-- T1 :: (* -> *) -> * -> *
data T1 f a = T1 (f a)
```

The three special types ``(,)``, ``(->)``, ``[]`` have special type-level
syntactic sugar:

```hs
(,) Int Int   =  (Int, Int)
(->) Int Int  =  Int -> Int
[] Int        =  [Int]
```

## Typeclasses

A typeclass is a collection of functions which conform to a given interface.  An implementation of an
interface is called an instance. Typeclasses are effectively syntactic sugar for records of functions and
nested records (called *dictionaries*) of functions parameterized over the instance type.  These
dictionaries are implicitly threaded throughout the program whenever an overloaded identifier is used. When a
typeclass is used over a concrete type, the implementation is simply spliced in at the call site. When a
typeclass is used over a polymorphic type, an implicit dictionary parameter is added to the function so that
the implementation of the necessary functionality is passed with the polymorphic value.

Typeclasses are "open" and additional instances can always be added, but the defining feature of a typeclass is
that the instance search always converges to a single type to make the process of resolving overloaded identifiers globally unambiguous.

For instance, the Functor typeclass allows us to "map" a function generically
over any type of kind (``* -> *``) and apply it to its internal structure.

```hs
class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap f []     = []
  fmap f (x:xs) = f x : fmap f xs

instance Functor ((,) a) where
  fmap f (a,b) = (a, f b)
```

## Operators

In Haskell, infix operators are simply functions, and quite often they are used in place of alphanumerical names when the functions involved combine in common ways and are subject to algebraic laws.

```hs
infixl 6 +
infixl 6 -
infixl 7 /
infixl 7 *

infixr 5 ++
infixr 9 .

-- Operators can be written in section form:
(x+) =	\y -> x+y
(+y) =	\x -> x+y
(+)  =	\x y -> x+y
```

Any binary function can be written in infix form by surrounding the name in
backticks.

```hs
(+1) `fmap` [1,2,3] -- [2,3,4]
```


## Monads

A monad is a typeclass with two functions: `bind` and `return`.

```hs
class Monad m where
  bind   :: m a -> (a -> m b) -> m b
  return :: a -> m a
```

The bind function is usually written as an infix operator, `>>=`, or flipped, `=<<`.

```hs
infixl 1 >>=

class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a

-- outside a class definition, the sig of the (flipped) bind operator:
(=<<) :: Monad m => (a -> m b) -> m a -> m b
```

This defines the structure, but the monad itself also requires **3 monadic laws** (axioms) that all monad instances must satisfy:

```hs
-- Law 1
return a >>= f = f a

-- Law 2
m >>= return = m

-- Law 3
(m >>= f) >>= g = m >>= (\x -> f x >>= g)
```

Haskell has a level of syntactic sugar for monads known as *do-notation*. In this form, binds are written sequentially in block form which extract the variable from the binder.

```hs
do { a <- f ; m } = f >>= \a -> do { m }
do { f ; m } = f >> do { m }
do { m } = m

-- So, for example, the following are equivalent:
do
  a <- f
  b <- g
  c <- h
  return (a, b, c)


f >>= \a ->
  g >>= \b ->
    h >>= \c ->
      return (a, b, c)
```


## Applicatives

Applicatives allow sequencing parts of some contextual computation, but do not bind variables therein. Strictly speaking, applicatives are less expressive than monads.

```hs
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap
```


**Applicatives satisy these axioms**:

```hs
pure id <*> v = v                             -- Identity
pure f <*> pure x = pure (f x)                -- Homomorphism
u <*> pure y = pure ($ y) <*> u               -- Interchange
u <*> (v <*> w) = pure (.) <*> u <*> v <*> w  -- Composition
```


For example:

```hs
example1 :: Maybe Integer
example1 = (+) <$> m1 <*> m2
  where
    m1 = Just 3
    m2 = Nothing
```

Instances of the ``Applicative`` typeclass also have available the functions
``*>`` and ``<*``. These functions sequence applicative actions while
discarding the value of one of the arguments. The operator ``*>`` discards the
left argument, while ``<*`` discards the right. For example, in a monadic
parser combinator library, the ``*>`` would discard the value of the first
argument but return the value of the second.


## Monoids

Monoids provide an interface for structures which have an associative operation
(``mappend``, there is also the synonym ``<>``) and a neutral
(also: unit or zero) element (``mempty``) for that operation.

```hs
class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
```

The canonical example is the list type with concatenation as the operation
and the empty list as zero.

```hs
import Data.Monoid

a :: [Integer]
a = [1,2,3] <> [4,5,6]

b :: [Integer]
b = ([1,2,3] <> mempty) <> (mempty <> [4,5,6])
```


## Deriving

Instances for typeclasses like ``Read``, ``Show``, ``Eq`` and ``Ord`` can be
derived automatically by the Haskell compiler.

```hs
data PlatonicSolid
  = Tetrahedron
  | Cube
  | Octahedron
  | Dodecahedron
  | Icosahedron
  deriving (Show, Eq, Ord, Read)
```

```hs
example = show Icosahedron
example = read "Tetrahedron"
example = Cube == Octahedron
example = sort [Cube, Dodecahedron]
```


## IO

A value of type ``IO a`` is a computation which, when performed, does some I/O
before returning a value of type ``a``. The notable feature of Haskell is that
IO is still functionally pure; a value of type ``IO a`` is simply a value which
stands for a computation which, when invoked, will perform IO. There is no way
to peek into its contents without running it.

For instance, the following function does not print the numbers 1 to 5 to the
screen. Instead, it builds a list of IO computations:

```hs
fmap print [1..5] :: [IO ()]
```

We can then manipulate them as an ordinary list of values:

```hs
reverse (fmap print [1..5]) :: [IO ()]
```

We can then build a composite computation of each of the IO actions in the list
using ``sequence_``, which will evaluate the actions from left to right. The
resulting ``IO`` computation can be evaluated in ``main`` (or the GHCi repl,
which effectively is embedded inside of ``IO``).

```hs
>> sequence_ (fmap print [1..5]) :: IO ()
1
2
3
4
5

>> sequence_ (reverse (fmap print [1..5])) :: IO ()
5
4
3
2
1
```

The IO monad is wired into the runtime with compiler support. It is a special
case and most monads in Haskell have nothing to do with effects in this sense.

```hs
putStrLn :: String -> IO ()
print    :: Show a => a -> IO ()
```

The type of ``main`` is always ``IO ()``.

```hs
main :: IO ()
main = do
  putStrLn "Enter a number greater than 3: "
  x <- readLn
  print (x > 3)
```

The essence of monadic IO in Haskell is that *effects are reified as first class
values in the language and reflected in the type system*. This is one of
foundational ideas of Haskell, although it is not unique to Haskell.


## Monad Transformers

Monads can be combined together to form composite monads. Each of the composite
monads consists of *layers* of different monad functionality. For example, we
can combine an error-reporting monad with a state monad to encapsulate a certain
set of computations that need both functionalities. The use of monad
transformers, while not always necessary, is often one of the primary ways to
structure modern Haskell programs.

```hs
class MonadTrans t where
  lift :: Monad m => m a -> t m a
```

The implementation of monad transformers is comprised of two different
complementary libraries, ``transformers`` and ``mtl``. The ``transformers``
library provides the monad transformer layers and ``mtl`` extends this
functionality to allow implicit lifting between several layers.

To use transformers, we simply import the *Trans* variants of each of the
layers we want to compose and then wrap them in a newtype.

```hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

newtype Stack a = Stack { unStack :: StateT Int (WriterT [Int] IO) a }
  deriving (Monad)

foo :: Stack ()
foo = Stack $ do
  put 1                  -- State layer
  lift $ tell [2]        -- Writer layer
  lift $ lift $ print 3  -- IO Layer
  return ()

evalStack :: Stack a -> IO [Int]
evalStack m = execWriterT (evalStateT (unStack m) 0)
```

As illustrated by the following stack diagram:

![](img/stack.png)

Using ``mtl`` and ``GeneralizedNewtypeDeriving``, we can produce the same stack
but with a simpler forward-facing interface to the transformer stack. Under the
hood, ``mtl`` is using an extension called ``FunctionalDependencies`` to
automatically infer which layer of a transformer stack a function belongs to and
can then lift into it.

```hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.Trans
import Control.Monad.State
import Control.Monad.Writer

newtype Stack a = Stack { unStack :: StateT Int (WriterT [Int] IO) a }
  deriving (Monad, MonadState Int, MonadWriter [Int], MonadIO)

foo :: Stack ()
foo = do
  put 1             -- State layer
  tell [2]          -- Writer layer
  liftIO $ print 3  -- IO Layer
  return ()

evalStack :: Stack a -> IO [Int]
evalStack m = execWriterT (evalStateT (unStack m) 0)
```

**StateT**

The state monad allows functions within a stateful monadic context to access and
modify shared state.

```hs
put    :: s -> State s ()          -- set the state value
get    :: State s s                -- get the state
gets   :: (s -> a) -> State s a    -- apply a function over the state, and return the result
modify :: (s -> s) -> State s ()   -- set the state, using a modifier function
```

Evaluation functions often follow the naming convention of using the prefixes
``run``, ``eval``, and ``exec``:

```hs
execState :: State s a -> s -> s         -- yield the state
evalState :: State s a -> s -> a         -- yield the return value
runState  :: State s a -> s -> (a, s)    -- yield the state and return value
```

For example:

```hs
import Control.Monad.State

test :: State Int Int
test = do
  put 3
  modify (+1)
  get

main :: IO ()
main = print $ execState test 0
```

**ReaderT**

The Reader monad allows a fixed value to be passed around inside the monadic
context.

```hs
ask   :: Reader r r                            -- get the value
asks  :: (r -> a) -> Reader r a                -- apply a function to the value, and return the result
local :: (r -> r) -> Reader r a -> Reader r a  -- run a monadic action, with the value modified by a function
```

For example:

```hs
import Control.Monad.Reader

data MyContext = MyContext
  { foo :: String
  , bar :: Int
  } deriving (Show)

computation :: Reader MyContext (Maybe String)
computation = do
  n <- asks bar
  x <- asks foo
  if n > 0
    then return (Just x)
    else return Nothing

ex1 :: Maybe String
ex1 = runReader computation $ MyContext "hello" 1

ex2 :: Maybe String
ex2 = runReader computation $ MyContext "haskell" 0
```

**WriterT**

The writer monad lets us emit a lazy stream of values from within a monadic
context. The primary function ``tell`` adds a value to the writer context.

```hs
tell :: (Monoid w) => w -> Writer w ()
```

The monad can be evaluated returning the collected writer context and
optionally the returned value.

```hs
execWriter :: (Monoid w) => Writer w a -> w
runWriter  :: (Monoid w) => Writer w a -> (a, w)
```

```hs
import Control.Monad.Writer

type MyWriter = Writer [Int] String

example :: MyWriter
example = do
  tell [1..5]
  tell [5..10]
  return "foo"

output :: (String, [Int])
output = runWriter example
```

**ExceptT**

The Exception monad allows logic to fail at any point during computation with a
user-defined exception. The exception type is the first parameter of the monad
type.

```hs
throwError :: e -> Except e a
runExcept  :: Except e a -> Either e a
```

For example:

```hs
import Control.Monad.Except

type Err = String

safeDiv :: Int -> Int -> Except Err Int
safeDiv a 0 = throwError "Divide by zero"
safeDiv a b = return (a `div` b)

example :: Either Err Int
example = runExcept $ do
  x <- safeDiv 2 3
  y <- safeDiv 2 0
  return (x + y)
```

**Kleisli Arrows**

The additional combinators for monads (``(>=>)``, ``(<=<)``) compose two different monadic actions in
sequence. ``(<=<)`` is the monadic equivalent of the regular function composition
operator ``(.)`` and ``(>=>)`` is just ``flip (<=<)``.

```hs
(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
```

The monad laws can be expressed equivalently in terms of Kleisli composition.

```hs
(f >=> g) >=> h   =   f >=> (g >=> h)
return >=> f      =   f
f >=> return      =   f
```


## Text

The usual ``String`` type is a singly-linked list of characters, which,
although simple, is not efficient in storage or locality. The letters of the
string are not stored contiguously in memory and are instead allocated across
the heap.

The ``Text`` and ``ByteString`` libraries provide alternative efficient
structures for working with contiguous blocks of text data. ``ByteString`` is
useful when working with the ASCII character set, while ``Text`` provides a
text type for use with Unicode.

The ``OverloadedStrings`` extension allows us to overload the string type in
the frontend language to use any one of the available string representations.

```hs
class IsString a where
  fromString :: String -> a

pack :: String -> Text
unpack :: Text -> String
```

So, for example:

```hs
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T

str :: T.Text
str = "bar"
```

## Cabal & Stack

To set up an existing project with a sandbox, run:

```bash
$ cabal sandbox init
```

This will create the ``.cabal-sandbox`` directory, which is the local path GHC
will use to look for dependencies when building the project.

To install dependencies from Hackage, run:

```bash
$ cabal install --only-dependencies
```

Finally, configure the library for building:

```bash
$ cabal configure
```

Now we can launch a GHCi shell scoped with the modules from the project in
scope:

```bash
$ cabal repl
```


## Resources

If any of these concepts are unfamiliar, there are some external resources that
will try to explain them. The most thorough is the Stanford course lecture
notes.

* [Stanford CS240h](http://www.scs.stanford.edu/14sp-cs240h/) by Bryan O'Sullivan, David Terei
* [Real World Haskell](http://www.amazon.com/Real-World-Haskell-Bryan-OSullivan/dp/05965149800) by Bryan O'Sullivan, Don Stewart, and John Goerzen

There are some books as well, but your mileage may vary with these. Much of the
material is dated and only covers basic programming and not "programming in the
large".

* [Introduction to Functioanl Programming](http://www.amazon.com/Introduction-Functional-Programming-International-Computing/dp/0134841891) by Richard Bird and Philip Wadler
* [Learn you a Haskell](http://learnyouahaskell.com/) by Miran Lipovača
* [Programming in Haskell](http://www.amazon.com/gp/product/0521692695) by Graham Hutton
* [Thinking Functionally](http://www.cambridge.org/us/academic/subjects/computer-science/programming-languages-and-applied-logic/thinking-functionally-haskell) by Richard Bird
