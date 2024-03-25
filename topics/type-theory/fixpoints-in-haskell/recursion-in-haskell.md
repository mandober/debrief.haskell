---
url: https://rebeccaskinner.net/posts/2021-06-09-getting-to-the-fixed-point.html
article-title:    The Fixed Point
date-posted:      2021-07-06
date-downloaded:  2022-04-17
tags: fixpoint, definedness order, flat types
---
# Recursion in Haskell

## Recursion in general

In Haskell, recursion usually occurs at the term level, but it may also be employed at the type (or even kind) level. At the term level, functions are the language elements that frequently use recursion, but there are also recursive data types (recursive data structures), and there is even polymorphic recursion.

In FP, looping and iteration is achieved through recursion, so recursive functions are more than common. A *recursive function* is a function that refers back to itself. A function's definition consists of one or more equations, with the function's name (its identifier) on the lhs of the equals sign, followed by any number of formal parameters; then, on the rhs, a definition of the function (function's body) is given. If the function's name is mentioned on the rhs, that identifier refers the function that is currently being defined - making it a recursive function (a function with a self-reference), e.g. `f = … f …`.

In a simple case of a function with a single equation, we just get rid of these ellipses, which amounts to this:

```hs
func :: a
func = func
--     ^^^^ recursive call
```

## Recursion and corecursion

```hs
-- | Infinite list of natural numbers
nats :: Integer -> [Integer]
nats x = x : nats (x + 1)
```

## Types of recursion

Types of recursion
- well-founded recursion
- mutual recursion
- tail recursion

The recursion is *well-formed* if the *recursive parameter* is somehow getting smaller and smaller with each recursive call, so it eventually hits the *base case* and terminates.

Although the function diverges, it is actually *corecursive*; e.g. given 0, it produces an infinite list of natural numbers.


## Different ways to accomplish recursion

- manual recursion
- automatic recursion
- recursive binding

## Manual Recursion

The situation when the programmer directly makes a recursive call may be called *manual recursion*.

Unlike manual recursion, where we can see the recursive structure of the function (by looking for the place in our code where a function calls itself), *automatic recursion* is done indirectly by using a hof that manages the recursion (e.g. `foldr`).

We can use `foldr` to write a factorial:

```hs
factorial :: Integer -> Integer
factorial n =
  let handleStep currentNum currentFact = currentNum * currentFact
  in  foldr handleStep 1 [1..n]
```

The `foldr` itself may be defined as

```hs
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f accum items =
  case items of
    []          -> accum
    (next:rest) -> f next (foldr f accum rest)
```



Whenever we're writing a recursive function, it helps to see how the problem can be broken down into smaller problems, which resamble the overall problem.

**Well-formed recursion**: recursion is well-formed if the *recursive parameter* is somehow getting smaller and smaller with each recursive call, so it eventually hits the *base case* and terminates.

## Base case and recursive case

The important thing when writing a recursive function is figuring out the terminating conditions. This may be one or more conditions, called the base cases, toward which the iteration should progress, and which, when are finally met, terminate recursion.

For a factorial, the base case is the smallest number that we can calculate a factorial for, i.e. 0. With this information, we can write the factorial using direct recursion:

```hs
factorial :: Integer -> Integer
factorial n =
  case n of
    0 -> 1
    n -> n * (factorial (n - 1))
```




## Automatic Recursion

Unlike manual recursion, where we can see the recursive structure of our function by looking for the place in our code where a function calls itself, a function that is using automatic recursion does so indirectly, using a function that manages the recursion for us automatically.

One example of automatic recursion that you're likely familiar with are the fold functions: foldl and foldr. These two functions, and others like them, allow you to work on data that can naturally be traversed recursively while only having to implement the code to deal with the current element and any state that you want to carry over across calls.

We can use a function like foldr to write a factorial by letting it do the recursion for us:

```hs
factorial :: Integer -> Integer
factorial n =
  let
    handleStep currentNum currentFact =
      currentNum * currentFact
  in foldr handleStep 1 [1..n]
```

Even if you've used foldr before, it will be helpful as we're framing the problem to build a version of it ourselves, so that we can think through how these sorts of recursive functions work.

```hs
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f accum items =
  case items of
    [] ->
      accum
    (next:rest) ->
      f next (foldr f accum rest)
```

Looking at this function, you can see how the shape of the function, with the case statement, the base case with an empty list, and the recursive call that each time moves one element forward in the list. Our implementation of foldr is more generic to be sure- we've replaced the knowledge that factorial 0 is 1 with a more general statement that the value of our fold at our base case is the initial accumulator value that was provided, and now instead of doing multiplication directly in our recursive call we hand off the details to a function that is passed in, but if you squint a bit you can see how similar the two functions really are.

Using functions like folds that deal with the shape of the data and handle the recursion for us has a number of benefits. First, it removes some unnecessary duplication of code. Traversing data structures and doing something on all of the elements is quite common in functional programming, and if we were to implement it from scratch each time it would take us much longer to write programs, and there are many more opportunities for errors. Second, it makes our code a bit more readable by letting us center the "business logic" of our function. In most cases, the fact that our data is represented as a list, a binary tree, etc. is incidental to the problem at hand. By separating out the logic for dealing with individual elements from the logic for traversing data structures, we center the relevant bits of our code. Finally, and perhaps most importantly, functions like folds give us a common language for talking about the structure of our programs. For someone who has been programming for some time, saying that something is "simply a fold over some data" can convey a good deal of information about the general idea of how a program is implemented without the need to bog them down in too many extraneous details.

## Recursive Let Bindings

This type of recursion is not so much a specific technique as is a feature of Haskell that allows us to use manual recursion more easily.

Recursive let bindings mean that the `let…in` language construct can refer to the thing it is being defined (like ML's `reclet`, which also has a non-recursive, plain `let`).


A simple example of this would be, continuing with our factorial example, a function that computers the double factorial, that is to say, the factorial of the factorial of an input number:

```hs
doubleFactorial :: Integer -> Integer
doubleFactorial n =
  let
    factorial a =
      case a of
        0 -> 1
        a -> a * factorial (a - 1)
  in factorial (factorial n)
```
