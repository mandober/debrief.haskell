---
url: https://rebeccaskinner.net/posts/2021-06-09-getting-to-the-fixed-point.html
article-title:    The Fixed Point
date-posted:      2021-07-06
date-downloaded:  2022-04-17
tags: fixpoint, definedness order, flat types
---
# Fixpoints in Haskell

In this post, we'll work through the fixed point function in Haskell, building several examples along the way. At the end of the post we'll come away with a deeper understanding of recursion and how Haskell's lazy evaluation changes the way you can think about writing programs.

## Fix function

The `fix` function from `Data.Function` gives us another way to approach recursion. The docs say that `fix f` is the least fixed point of `f`, i.e. the least defined `x` such that `f x = x`.

We can write the factorial using `fix`

```hs
-- recursive
fact = \ n -> if n <= 1 then 1 else n * fact (n - 1)

-- fix'ed
fac = fix (\ff n -> if n <= 1 then 1 else n * ff (n - 1))
```

Instead of making a recursive call, we introduce a new dummy parameter `ff`; when used within `fix`, this parameter will be bound to the `fix`'s first argument (which is the entire anonymous function on its right), thereby reintroducing recursion. The `fac` function itself is not recursive.

## The type of fix

Whenever we want to understand something new in haskell, a good first instinct is to start by looking at the types, as this tells us quite a bit about what a function can, and - often more importantly - can't do.

The signature `fix :: (a -> a) -> a` tells us that `fix` is a unary function that takes a single function argument, `a -> a`, returning some value `a`.

```hs
g :: a -> a
fix g :: a
```

At first look, this might not look all that difficult at all. `fix` just needs to call `g` with a value to get a value back to return. Any number of similar functions would work for a specific type, like `Int`, but that relies on the fact that the type is concrete. However, since `a` could be anything, there's no value we can pick to pass into `g` to get back a value. We can see this play out if we try to pass some function, like `(+1)` into `fix`: it never gives back a value because it can't.

The point is that `fix` can sometimes give back a result. It can do that when the returned value doesn't depend on any particular input value - which is the case with `const`ant functions:

```hs
fix $ const "fix this"
-- "fix this"
```

The definition of a fixpoint is that it's an input value that, when passed into a function, causes the function to return it. And this is exactly what `const` does - it ignores the second argument and just returns the first. So, by partially applying the first argument we get a function that has a fixpoint:

```hs
λ :t const
const :: a -> b -> a

-- partially applying the first arg
λ f = const "foo"

-- f has a fixpoint: f "foo" = "foo"
λ f 1
"foo"
```

Outside of the mathematical definition of a fixpoint, the behavior of `fix` also makes sense if we think about it in terms of laziness, and computability.

Because `fix` is polymorphic, it cannot ever find a value that is a fixpoint of the argument function.

Since `fix` is polymorphic, it cannot ever get a value to pass into the function it is trying to find the fixed point of. In a strictly evaluated language, this would be a problem, but thanks to laziness, "a value we can't ever actually compute" is still something we can work with.

In the case of `fix`, the parameter it passes into the arg function might be a value that cannot ever be computed, but it turns out that is fine as long as that value is never evaluated.

In other words, if the function we pass to `fix` is lazy in its argument, then the impossible calculation of the incomputable value never happens.

## Binary function argument

The signature `fix :: (a -> a) -> a` shows that `fix` expects a unary function, say, `g :: a -> a`, but the function we pass to `fix` below obviously has two arguments, `ff` and `n`:

```hs
λ fix (\ff n -> if n <= 1 then 1 else n * ff (n - 1))

-- making fix's arg a standalone function
λ fac = \ff n -> if n <= 1 then 1 else n * ff (n - 1)

λ :t fac
fac :: (Ord a, Num a) => (a -> a) -> a -> a
```

Let's specialize it to some specific type as we're thinking about this.

```hs
fac :: (Int -> Int) -> Int -> Int
```

To see how this works we need to recall that all functions are auto-curryied.

This signature shows that a function takes two args, a function `Int -> Int` and an `Int`, and it returns an `Int`.

Since functions are curryied, we can look at `fac` from another aspect - as a function that takes a function from Int to Int, and returns a function from Int to Int.

```hs
fac :: (Int -> Int) -> (Int -> Int)
fac = \ff -> \n -> if n <= 1 then 1 else n * ff (n - 1)
```

Another thing to keep in mind is that the type parameter `a` of a polymorphic functions can be instantiated into any type, including a function.

```hs
fix :: (a -> a) -> a

fix @(Int -> Int) :: ((Int -> Int) -> (Int -> Int)) -> (Int -> Int)

fix @(Int -> Int) :: ((Int -> Int) -> Int -> Int) -> Int -> Int
```

Once we see how the `fix` is implemented, we'll come back to the issues of its type and laziness.


## Implementing fix

The source code for `fix` is available on hackage, and it's quite short:

```hs
fix :: (a -> a) -> a
fix¹ f = let x = f x in x

fix² f = x where x = f x

fix³ f = f (fix³ f)
```

We start with a parameter `f`, which is whatever function we want to find the fixpoint for. Next, we create a recursive `let` binding where we define `x` to be the result of applying `f` to `x`. This recursive let binding is the magic behind how the fixed point calculation works.

When we first call `fix` and create the let binding where we define `x`, we know that it must have the type `a`, and a value that, when it's needed, will be computed by the expression `f x`.

The `x` in that computation, likewise, isn't a value yet. It's a thunk that, if evaluated, will be computed by calling `f x`.

In other words, we start with:

```hs
fix f = let x = {- <some unevaluated thunk> -} in x
```

If whoever calls this function decides they need the value of `x`, they will get:

```hs
fix f = let x = f {- <some unevaluated thunk> -} in x
```

If `f` is a function like `const` which always returns a value without ever looking at its input, then `x` will be bound to that value and can be evaluated without any issues.

On the other hand, if `f` needs to evaluate `x`, we'll end up with a computation that can never complete because each time we try to look at `x` we'll get back another layer of some unevaluated thunk.

On the surface, this might seem to be a bit limited. After all, if we need to pass in a function that always returns a value and never looks at its input, we're limited to `const` and not much else, unless we can get some data to work with from somewhere else...

## Tying the knot

In fact, `fix` doesn't require a function that never evaluates its argument in order to eventually give us back a value. Instead, we need to give it a function that **eventually doesn't evaluate its argument**.

The one-word difference here between never and eventually is the difference between a computation that terminates and is well-defined, and one that is undefined. This is where passing a function of two parameters into `fix` comes into play.

When we have a function like `Int -> Int` there's no option except for the input value that we're given to decide when to terminate, so we always have to evaluate it.

On the other hand, a function with the type `(Int -> Int) -> Int -> Int` has much more flexibility.

```hs
fac :: (Int -> Int) -> (Int -> Int)
fac = \ff -> \n -> if n <= 1 then 1 else n * ff (n-1)
```

In this function, we're taking a parameter, `ff :: Int -> Int`, but we only ever evaluate it if `n` is greater than `1`. Since `n` decreases with each step, we know that it will eventually reach 1, and so we know that `ff` will eventually not be evaluated, enabling the `fac` to return with a value.

This is actually a really interesting approach - we're taking advantage of laziness, so that we can return a function that only causes a value in its closure to be evaluated when the input to the returned function is sufficiently high. It's almost like we're *passing information backwards in time*, but in fact we're simply making use of lazy evaluation and the call stack to propagate information back and eventually resolve some thunks that have been hanging out patiently waiting for us to allow them to be computed.


## Fixing The Factorial

As a final exercise, let's walk through the example step by step to get a much better idea of what's happening when we make use of fix.

We'll start our manual evaluation by defining two functions

```hs
factor :: (Int -> Int) -> (Int -> Int)
factor = \ff \n -> if n <= 1 then 1 else n * ff (n - 1)

fac :: Int -> Int
fac = fix factor


-- In ghci we'll start by calling fac with 5
λ fac 5

-- which expands to
fac 5
(fix factor) 5
(fix (\ff n -> if n <= 1 then 1 else n * ff (n - 1))) 5

-- and by the definition of fix
fix :: (a -> a) -> a
fix :: ((Integer -> Integer) -> Integer -> Integer) -> Integer -> Integer
fix f = let x = f x in x

(fix factor) 5
(fix     (factor)       ) 5
(let x = (factor) x in x) 5
(let x = (factor                                        ) x in x) 5
(let x = (\ff n -> if n <= 1 then 1 else n * ff (n - 1))) x in x) 5


-- applying it to 5 (n ⟼ 5) we get...
-- hmm, how the fuck did 5 latch onto `n` (and not `ff`)?!
-- obviously, this is not how this gets evaluated step by step
(let x = (\ff 5 -> if 5 <= 1 then 1 else 5 * ff (5 - 1)) x
 in  x) 5


-- Following the pattern until we get to the base case
let x = (\ff    5 -> if 5 <= 1 then 1 else 5 * ff    (5 - 1)    )
      $ (\ff'   4 -> if 4 <= 1 then 1 else 4 * ff'   (4 - 1)    )
      $ (\ff''  3 -> if 3 <= 1 then 1 else 3 * ff''  (3 - 1)    )
      $ (\ff''' 2 -> if 2 <= 1 then 1 else 2 * ff''' (2 - 1)    )
      $ (\_ff   1 -> if 1 <= 1 then 1 else {- never evaluated -})
in x $ 5
```

Once we finally hit the base case `n == 1`, we stop evaluating `ff` and we start resolving the stack of calls in reverse order, so `rec'''` becomes 1 and we get:

```hs
let x = (\rec 5 ->
             if 5 <= 1 then 1 else 5 * rec (5 - 1)
          ) $ (\rec' 4 ->
             if 4 <= 1 then 1 else 4 * rec' (4 - 1))
          ) $ (\rec'' 3 ->
             if 3 <= 1 then 1 else 3 * rec'' (3 - 1))
          ) $ (\rec''' 2 ->
             if 2 <= 1 then 1 else 2 * 1)
          )
in x $ 5

-- which becomes
let x = (\rec 5 ->
             if 5 <= 1 then 1 else 5 * rec (5 - 1)
          ) $ (\rec' 4 ->
             if 4 <= 1 then 1 else 4 * rec' (4 - 1))
          ) $ (\rec'' 3 ->
             if 3 <= 1 then 1 else 3 * 2
          )
in x $ 5

-- and so on, until we finally get
let x = (\_ 5 ->
             if 5 <= 1 then 1 else 5 * 4 * 3 * 2
        )
in x $ 5
```
