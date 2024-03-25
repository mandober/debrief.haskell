# Continuation Passing Style

<!-- TOC -->

- [About continuations](#about-continuations)
  - [Handling a continuation as a type](#handling-a-continuation-as-a-type)
- [Mapping a continuation](#mapping-a-continuation)
  - [The Goal](#the-goal)
  - [The Plan](#the-plan)
  - [The Solution](#the-solution)
- [Applicative functor operator for continuations](#applicative-functor-operator-for-continuations)
  - [Flow diagram](#flow-diagram)
  - [Logical deduction](#logical-deduction)
- [Monadic operator for continuations](#monadic-operator-for-continuations)
  - [Logical deduction](#logical-deduction-1)

<!-- /TOC -->


## Summary

```js
a -> b, (a -> r) -> r, b -> r |- r
┌──┬─────────────────────────────┬──────────────────┐
│1 │ a -> b                    f │ proposition      │
│2 │ (a -> r) -> r             k │ proposition      │
│3 │ b -> r                    g │ proposition      │
╞══╪═════════════════════════════╪══════════════════╡
│0 │ r                           │ conclusion       │
╞══╪═════════════════════════════╪══════════════════╡
│4 │ ┌ a¹                      x │ assumption [¹]   │
│5 │ │ b                         │ ->E 1,4          │
│6 │ │ r                         │ ->E 3,5          │
│7 │ (a¹ -> r)                   │ ->I 4-6    [¹]   │
│8 │ r                           │ ->E 2,7          │
└──┴─────────────────────────────┴──────────────────┘
fmap :: (a -> b) -> ((a -> r) -> r) -> ((b -> r) -> r)
fmap f k g = k \x -> g $ f x


((a -> b) -> r) -> r, (a -> r) -> r, b -> r |- r
┌──┬─────────────────────────────┬──────────────────┐
│1 │ ((a -> b) -> r) -> r      h │ proposition      │
│2 │ (a -> r) -> r             k │ proposition      │
│3 │ b -> r                    g │ proposition      │
╞══╪═════════════════════════════╪══════════════════╡
│0 │ r                           │ conclusion       │
╞══╪═════════════════════════════╪══════════════════╡
│4 │ ┌ (a -> b)¹               f │ assumption [¹]   │
│5 │ │ ┌ a²                    x │ assumption   [²] │
│6 │ │ │ b                       │ ->E 4,5          │
│7 │ │ │ r                       │ ->E 3,6          │
│8 │ │ a² -> r                   │ ->I 5-7      [²] │
│9 │ │ r                         │ ->E 2,8          │
│10│ (a -> b)¹ -> r              │ ->I 4-9    [¹]   │
│11│ r                           │ ->E 1,11         │
└──┴─────────────────────────────┴──────────────────┘
(<*>) :: (((a -> b) -> r) -> r) -> ((a -> r) -> r) -> ((b -> r) -> r)
(<*>) h k g = h \f -> k \x -> g $ f x


(a -> r) -> r, a -> (b -> r) -> r, b -> r |- r
┌──┬─────────────────────────────┬──────────────────┐
│1 │ (a -> r) -> r             k │ proposition      │
│2 │ a -> (b -> r) -> r        h │ proposition      │
│3 │ b -> r                    g │ proposition      │
╞══╪═════════════════════════════╪══════════════════╡
│0 │ r                           │ conclusion       │
╞══╪═════════════════════════════╪══════════════════╡
│4 │ ┌ a¹                      x │ assumption [¹]   │
│5 │ │ (b -> r) -> r             │ ->E 4,2          │
│6 │ │ r                         │ ->E 5,3          │
│7 │ (a¹ -> r)                   │ ->I 4-6    [¹]   │
│8 │ r                           │ ->E 1,7          │
└──┴─────────────────────────────┴──────────────────┘
(>>=) :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
(>>=) k h g = k \a -> h a g
```



## Topics

- Introduction to CPS
- About
  - continuations
  - callbacks
  - CPS
  - continuation-passing style
  - delimited continuations
  - shift and reset
  - conversion to CPS
  - continuation type signature
- Handling a continuation as a type
  - continuation as a function type, `(a -> r) -> r`
  - continuation as a newtype, `Cont r a`
  - continuation as a monad transformer, `ContT r m a`
- FAM-like methods as standalone functions
  - fmap: Functorial mapping a continuation, `<$>`
  - star: Applicative mapping for continuations, `<*>`
  - bind: Monadic mapping for continuations, `>>=`
- Constructing functions that involve continuations
  - The goal
  - The plan
  - The solution
  - Flow diagram
  - Symbolic logic
    - logical deduction
    - using symbolic logic to derive function implementations
    - Curry-Howard correspondence
    - implication elimination
    - implication introduction
    - discharging assumptions
  - Implementation of FAM instances for Cont
  - Implementation of FAM instances for ContT


## Diverging introduction

Comprehending the details of:
- continuations as a function type `(a -> r) -> r`
- continuations as a newtype `Cont r a`
- implementations of FAM instances for `Cont r`
- implementations of FAM-like functions for `(a -> r) -> r`

*Continuation Passing Style (CPS)* is a style of programming which makes the control flow explicit. Normally, when a caller invokes a function `f`, the control is passed to `f`, which usually runs from top to bottom and when finished, it yields the control back to the caller along with some kind of return value.

In imperative programming, the `return` keyword, that ends a function's execution, is explicit, and can be used in each potential branch inside a function. It can also be used to exit the function prematurely, which is a lot like an exception. Returning early from a function is somewhat more engaging in Haskell and only possible inside IO-tainted functions (which is the same environment required for exceptions).

A truly pure function cannot possibly have two ways to go about its execution. That is, disregarding those functions that use a branch to `error` out, which is a lot like unchecked exceptions unwarned about in the function's signature. But, relying on a function's signature to always tell us the truth, we know that an honest function can have several exit points due to branching, but they all must agree on returning the same type. Anyway, continuations can be used to implement interesting things, especially those related to control flow, which includes implementing the imperative `return` or even `goto` commands in Haskell (or so I've heard).

In the common *run-to-completion model*, functions run until completion, at which time they yield the control back to their caller, along with some kind of return value. On the other hand, in CPS, a function `f` does the computation, but instead of returning to the caller, it invokes a callback function, passing it the computed value.

On the other hand, in CPS, a function `f` would take an additional parameter, `k`, intended to bind a callback function; then, when `f` finishes, instead of returning, it calls the callback `k` passing it the would-be-returned value.

In CPS, functions usually do not return since they pass their would-be-returned value to their callbacks, which eventually yield to the caller. CPS is always tail-recursive - since functions don't return everything must be pack "to go" like in the tail-recursive calls. It can also be said that the recursive CPS is recursion with an accumulator that is disguised as a callback.

Continuations are identified by their distinguishing signature, `(a -> r) -> r`.


## About continuations

Messing around with ordinary function application is enough to arrive at the notion of continuations. The most straightforward higher-order function that embodies function application, something like `($) :: (a -> b) -> a -> b`, takes a function argument `(a -> b)` and a value argument of type `a`, and produces a value of type `b`. Just by flipping its parameters we arrive at that distinct signature of continuations, `f :: a -> (a -> b) -> b`.

Conventionally, the signature of continuations is presented with `r`'s instead of the `b`'s, i.e. as `(a -> r) -> r`. This is to emphasize the fact that `r` type variables are fixed, while the type vars such as `a` or `b` are usually made to vary. Namely, since we cannot make this standalone continuation type an instance of classes, we wrap it in a newtype and declare its type parameters in the order suitable for partial type application.

```hs
($) :: (a -> b) -> a -> b
($) ab a = ab a

-- f = flip ($)
f :: a -> (a -> b) -> b
f a = \ab -> ab a

-- wrapped in a newtype
newtype Cont r a = Cont ((a -> r) -> r)
```

Partially applied, `f` can be considered a function that awaits for its function argument to finish with the computation - the passed in function then acts as a continuation of the computation.

More genereally and more precise, a plain function `h` that returns some value of type `a`, is converted into a continuation-taking function `h'` by
1. adding to it an extra parameter, `k`, that will bind the callback function
2. having it apply the callback to the original output value, `k a`

The function `h`, that was originally `n`-ary becomes `n+1`-ary, declaring an additional parameter `k` which reflects itself in the `h`'s signature by inserting `... -> (a -> r) -> ...` in the appropriate place; also, the entire return type is changed from `... -> a` to  `... -> r`. By way of example:

```hs
-- unremarkable function: for emphasis, the output
-- is redundantly captured by y such that f(n) = y
ps :: Int -> Int
ps n = let y = n + 9 in y

-- CPS conversion:
-- 1) add an extra cont-taking param k
-- 2) pass original output to k
cps :: Int -> (Int -> r) -> r
cps n k = let y = n + 9 in k y

-- the type of callback can still play Int-in-Int-out game
ex1 :: Int
ex1 = cps 7 (*2)

-- or do something else
ex2 :: IO ()
ex2 = cps -5 print -- () is returned and stdout displays "4"

-- id is often used to recover the original output
ex3 = cps 6 id                                        -- 15 :: Int

-- the original output is captured by a param
ex5 = cps 6 $ \a -> cps a (*a) + a  + cps a (+a)      -- 414 :: Int
```

The output type of the original function was `Int`, but now that it takes a callback, the original output type has become an input type to the callback. When invoked, the callback will eventually return a type about which we cannot know anything at this time - only the (future) caller will know this. This reflects in the function's signature as `... -> (Int -> r) -> ... -> r` part, where `(Int -> r)` is the type of the callback, which need not be (although it usually is) the last parameter. Also, the function's overall output type becomes the type that the callback returns i.e. `r`.

Passing in `id` as the callback is the usual way to recover original would-be-returned value. However, we can now pass in a function like `print` and the output would be `()` along with the result displayed in the console.

### Handling a continuation as a type

```hs
-- bare cont signature
cont :: (a -> r) -> r

-- as a type synonym
type Cont r a = (a -> r) -> r

-- as a newtype
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

-- in a monad transformer skin
newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

-- which means a type synonym can also be
type Cont r a = ContT r Identity a

-- or, sans the last point
type Cont r = ContT r Identity
```

Continuations are usually handled as the newtype `Cont`, not in their "naked" function type form, i.e. `(a -> r) -> r)`. But before introducing the newtype (which implies a lot of unwrapping and rewrapping), we can still use the bare type signature to see how would we go about constructing the class' methods as the standalone functions. After all, successfully constructing these functions will make it a lot easier to implement the proper methods later on.

### FAM methods

It is tricky to derive what the signature of our `kmap` function should look like considering the type of `fmap :: Functor f => (a -> b) -> f a -> f b`.

Assuming (in fact, knowing fo sho, by peeking into the Haskell's std) that a cont is a functor, how are we to derive the similar signature in term of cont's bare type? Seems we need to reverse-engineer the fmap's impl for Cont newtype.

We see that the newtype `Cont r a` has the declaration occurrences of type vars in reverse order in comparison to their application occurrences. This is the only way that permits a partial type-application such that the type param `r` is fixed, while the type param `a` varies.


```hs
-- Cont as a newtype
newtype Cont r a = Cont ((a -> r) -> r)

-- Cont's kind
type Cont :: * -> * -> *

-- Functor's kind
-- f :: * -> *

-- Cont newtype's fmap sig
fmap @(Cont r) :: (a -> b) -> Cont r a -> Cont r b

-- bare cont signature
cont :: (a -> r) -> r

kmap :: 
```

## Mapping a continuation

Starting with the Functor's `fmap` method, we'll make a similar, but standalone, function called `kmap`. 

To map a continuation `(a -> r) -> r)` means to get at the `a`, which represents a function's original return type (before it was made to take a callback). We need to get at the `a` so we can apply the function `a -> b` (supplied as an arg) and get a `b`. However, the complication is that the said mapping will have to be done in a way that allows us to buld the return type, i.e. `(b -> r) -> r`.

In summary, we need to go from     
`(a -> r) -> r`    
to    
`(b -> r) -> r`     
by turning that `a` to `b` with the supplied function     
`(a -> b)`

Or, in term of the function's type signature:

```hs
fmap :: (a -> b)
     -> f a
     -> f b

-- f is Cont r
kmap :: (a -> b)            -- ab
     -> ((a -> r) -> r)     -- ar_r
     -> ((b -> r) -> r)     -- br_r
```


### The Goal

```
get at this 'a'
     ↓
    (a -> r) -> r
     ↓
     a -> b     turn 'a' into 'b'
          ↓
          b -> r      turn 'b' into 'r'
                 ↘
                   r
reassembling the cont (b -> r) -> r without raising suspicion
```

### The Plan


What do we have?
* ab   ::  a -> b
* ar_r :: (a -> r) -> r)
* br   ::  b -> r

The last function was actually hiding in the parenthesis.

```hs
kmap :: (a -> b) -> ((a -> r) -> r) -> ((b -> r) -> r) -- is the same as
kmap :: (a -> b) -> ((a -> r) -> r) ->  (b -> r) -> r
```

We need to assume an (a :: a) as (\a -> ...) but where exactly do we plug it in? Well, the fn 'ab', assuming we can feed it an 'a' will produce a 'b'. And then we can apply the fn 'br' to that 'b' to get an 'r'. This is exactly like in logic: if we assume an 'a' and use it prove an 'r', then we can conclude that the implication a -> r holds.

   [a]
    ⁝
    a  a -> b
  --⁝----------------------- [MP]
    ⁝        b  b -> r
  --⁝----------------------- [MP]
    ⁝                r
  --⁝----------------------- ->i [discharging assumption]
    a -> r


So, we are now here:
  \a -> br (ab a)
which results in the type (a -> r). And we've got just the thing left, the fn ar_r which begs for an fn (a -> r) to produce an 'r' (what? another r?):
  ar_r (\a -> br (ab a))

which has the resulting type r. But what is the overall type? Fuck if I know.
The ghci says it's the right type (!) and that we have solved it...(whahuh?well, how about that?!). Not terribly satisfying but we've worked the types and there it is. Hmm, if all we needed was an 'r', we had one before, from applying br to b; aha, that 'r' wasn't stanalone, it was tied to the assumption a -> r; and the final 'r' was (oh, well). Hey, but we did wanted an 'r' as the overall return type! So it's all good (and there I was thinking that we needed a cont (b -> r) -> r). On the other hand, if we did need that cont type as the output type, then we could demote the 'br' from param to the lambda, i.e. from this:
  contmap ab ar_r br = ar_r (\a -> br (ab a))
to this:
  contmap ab ar_r = \br -> ar_r (\a -> br (ab a))

After all, when we represent the cont as a newtype:
  newtype Cont r a = Cont ((a -> r) -> r)
we will loose the ability to have the 'br' function as a param;
it will have to be taken as an assumption then, yielding the overall type
  (b -> r) -> r
That is, mapping a newyped (Cont r a) to (Cont r b) will be typed as:
  fmap :: (a -> b)    -- (a -> b)
        -> Cont r a    -- (a -> r) -> r
        -> Cont r b    -- (b -> r) -> r
  fmap ab (Cont ar_r) = Cont $ \br -> ar_r (\a -> br (ab a))

And the two cont fns will have types:
  Cont r a = Cont ((a -> r) -> r)
  Cont r b = Cont ((b -> r) -> r)

### The Solution

The solved cont mapping is:
    contmap :: (a -> b) -> ((a -> r) -> r) -> (b -> r) -> r
    contmap ab ar_r br = ar_r (\a -> br (ab a))

If we walk throught the types again, we see that it fits:

    ab ar_r = \br -> ar_r (\a -> br (ab a))
                                      b
                                  r
                           \a ->  r
        ((a -> r) -> r)   (\a ->  r)
                     r
    ----------------------------------------
         (b -> r) -> r

Aha! If we also take the fn (b -> r) as an assumption that lead us to the final standalone 'r' then we can conclude that the overall type indeed is `(b -> r) -> r` Just what we're after!

kmap ::  (a -> b)                    -- ab
     -> ((a -> r) -> r)              -- ar_r
     ->  (b -> r)                    -- br
     -> r

kmap ab ar_r br = ar_r (\a -> br (ab a))
kmap f  k = \br -> k   (\a -> br (f  a))


## Applicative continuations

This is an implementation similar to Applicative's `pure` and `ap` methods, except both are given as the standalone functions with signatures that are adjusted to free-form continuations, starting with and developed from the distinct continuation signature `(a -> r) -> r`.

```hs
kpure :: a -> (a -> r) -> r
kpure a ar = ar a

kapp :: (((a -> b) -> r) -> r)      -- abrr
     -> ((a -> r) -> r)             -- arr
     -> (b -> r)                    -- br
     -> r
kapp abrr arr br = abrr $ \ab -> arr (\a -> br (ab a))
-- or...
kapp abrr arr br = abrr $ \ab -> arr (\a -> (br . ab $ a))
-- ...in point-free form:
kapp abrr arr br = abrr $ \ab -> arr (br . ab)
kapp abrr arr br = abrr $ \ab -> arr $ (. ab) br
```

### Flow diagram

Reasoning through the process in a graphical form.

```hs
kapp :: (((a -> b) -> r) -> r)      -- abrr
     -> ((a -> r) -> r)             -- arr
     -> (b -> r)                    -- br
     -> r
kapp abrr arr br = abrr $ \ab -> arr (\a -> br (ab  a))
                            |          |      ↙     ↓
                            |       (ab :: a -> b) (a :: a)
                            |          |        ↓
                            |    br :: (b -> r) b
                            |          |     ↓
                            |         \a ->  r
                            |          ↓     ↓
               arr :: ((a -> r) -> r) (a ->  r)
                            |      ↓
                            |      r
                            |     ↙
                          \ab -> r
                            ↓      ↘
                         (a -> b) -> r
                          ↓    ↓     ↓
(((a -> b) -> r) -> r)  ((a -> b) -> r)
                    ↓
                    r
```
