# Folding

```hs
-- | in f: acc on the left, element on the right
-- self recursive call relatively on the left (first; starts rec call)
-- recursive call in terms of `foldl` (tail call)
-- TAIL RECURSIVE ✔
-- acc arg (f e x) sends progress (passed on in the recursive call)
-- recursive call: app of ternary foldl³
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f e []     = e
foldl f e (x:xs) = foldl f (f e x) xs

-- | in f: acc on the right, element on the left
-- self recursive call relatively last (as the second arg to f)
-- recursive call in terms of `f` (not tail call)
-- NOT TAIL RECURSIVE ✘
-- recursive call: app of binary f²
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f e []     = e
foldr f e (x:xs) = f x (foldr f e xs)
```


## Summary

This article explores the folding functions, the difference between `foldl` and `foldr`, making sure the way in which they deffer is clearly stated for easy recall.

It is often said that Haskell's two principal folding functions, `foldl` and `foldr`, differ in the way they perform the folding of a list. It is often implied that foldl processes elements from the front of a list, while foldr does it from the rear. This is not entirely correct as they both take list elements from the front. It can only be said that their fully expanded applications on a list display left associativity (in case of foldl), or right associativity (in case of foldr) due to the way their recursive case equations are defined.

```hs
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl step acc []     = acc
foldl step acc (x:xs) = foldl step (step acc x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr step acc []     = acc
foldr step acc (x:xs) = step x (foldr step acc xs)
```

The main difference is that foldl is defined (in the recursive case) in terms of itself, while foldr is defined in term of the `step` function. Also, foldl's `step` function takes the accumulator and an element, while foldr's `step` function takes an element then accumulator.

foldl
- `step` takes the acc and an element
- recursive case equation is defined in terms of the `foldl` function itself
- recursive call is in tail position (tail call recursive)
- expanded equations associate to the left
- more efficient then foldr for it doesn't generate thunks

foldr
- `step` takes an element and the acc
- recursive case equation is defined in terms of the `step` function
- not a tail-call recursion
- expanded equations associate to the right
- works by replacing structure's data ctors
- can generate a large amount of thunks


## Overview

Folding a structure can generalize to many things, so it's easier at first to just focus on one particular structure like lists, which are very foldable.

Folding a list can mean reducing it to one value, but it doesn't have to be just that. Fold goes under the name `reduce` in JS, which strongly suggest reduction, although it can be used there to produce more things then just a single value.

In Haskell, there are two basic folding functions, `foldl` and `foldr`, differentiated by their approach to folding. It may be said that a `foldl` folds a list from left i.e. from the beginning, and that `foldr` does it from the right i.e. from the end, but that's not the whole story.


## Parameters

First, let's consider writing a single folding function, `fold`, and the params it should take. The `fold` is a higher-order function and as such it should be as general as possible; to that end, it should only set up the "production line" without doing the actual work itself. It should declare what it needs in the form of parameters then arrange them along the production line, each one to a designated place.

To fold a list, there should be a parameter that binds a list, and like always with structures which are operated on, that parameter should be declared last, thereby facilitating easier composition or point-free expression. A list has the type `[a]` if and only if the list's elements have the type `a`.

- `element :: a` <=> `list :: [a]`

We now need to consider different things the `fold` does, so that we can come up with the return type. Considering the popular case of folding a list into a single value tells us that the returned value has a different type then the list itself; that is, if list is `[a]`, then the return type is `a`. For example, folding a list of integers, `[Int]`, returns an `Int`; so, more generally, a list of `[a]` returns an `a`. However, this is not the only thing the `fold` can do, *since all the functions that operate on lists can be defined in terms of fold*. The `fold` can also return a list, either of the same type, `[a]`, which would be the case if we define, e.g. `tail`, `filter` or `drop` in terms of fold, or a list of some different type, `[t]`, in the case of `map`.

Already, with these 3 types (`a`, `[a]`, `[t]`), we can see that we'll need a "fresh" (new, previously unmentioned) type variable as the return type. So, we'll represent all the different return values with a type variable `b` that will stand for both scalars and compound values as well (so `b` can be a scalar like `Int`, a list like `[Int]`, or something else entirely).

For now we have
- function name: `fold`
- parameter to bind a list: `[a]`
- list elements: `a`
- return type parameter: `b`

The function that performs the actual work will be passed to `fold` as an argument. We'll referred to it as the `step` function. Before discussing its signature, we must first consider what happens if the empty list is passed to `fold`. What value should we return from `fold` in that case?

The only thing we'd have is the empty list and we can't simply return it because the types wouldn't match: we can't return an `[a]` when the supplied list is empty and a `b` when its not (`[a]` will not unify with `b`).

f1 :: [a] -> b
f1 [] = []
f1 (x:xs) = x


For example, if we have a list of integers, `[Int]`, that we want to reduce by summing them up, we'll return an `Int` in case the integer list is non-empty; but if it is empty, we can't return the empty list since its type is `[Int]`, not `Int`.

fold :: (...) -> [a] -> b

This means, the `fold` needs another parameter that will bind the "default" argument. We already know that that paramater must have the same type as the return type of fold i.e. `b`.

fold :: (...) -> b -> [a] -> b

So, the first parameter of `fold` is for the step function, the second binds the default argument of type `b`, the third is for the list of type `[a]`, and the return is a `b` type of value.

We now consider the step function. Since the `fold`'s return type is `b` that must also be the return type of the step.

step :: (...) -> b

Also, it must have access to list's elements so one of its parameters must have the type `a`.

step :: (...) -> a -> b

The step cannot be a unary function since it must somehow combine list's elements, and "combining" involves at least two parameters. If the other parameter is also an `a`, we'd have a step that takes two `a`'s and returns a `b`.

step :: a -> a -> b

But that would be a nonsense function: it only has two things of `a` type so there's no way to return something of a different type, i.e. of type `b`. Trying to return something arbitrary will provoke the type checker to complain about the rigid type variable `b`; a rigid type is a type entirely specified by the user via a type signature.

```hs
step :: a -> a -> b
step x y = ()    -- Couldn't match expected type 'b' with actual type '()'
step x y = [x,y] -- Couldn't match expected type 'b' with actual type '[a]'
step x y = (x,y) -- Couldn't match expected type 'b' with actual type '(a,a)'
```

If we make the return type also an `a`, we'd get a function that takes two `a`'s and returns an `a`, which won't work in the broader context of `fold` if we don't also change the `b`'s to `a`'s in the `fold`'s signature.

```hs
-- before
fold :: (a -> a -> b) -> b -> [a] -> b
fold :: (a -> a -> a) -> b -> [a] -> b
-- after
fold :: (a -> a -> a) -> a -> [a] -> a
```

The last signature type-checks ok, but it restrict the utility of the `fold` to a function that can only reduce a list of values, of type `[a]`, to a single value of type `a` (which is the type of list's elements).

We need to backtrack.

(...)

And now we have all the ingredients for a fold, orientation still unknown.

When discussing foldl and foldr functions in terms of lists, it is usually said that they differ in the preferred end from which they process a list. In line with their names, it is said that `foldl` devours a list from the front (L->R, L2R, front-to-back), while `foldr` engages a list from the rear and proceeds towards the front (L<-R, R2L, back-to-front).

That is completely ridiculous and not entirely accurate (not at the ground level, not severals abstraction levels up); foldr cannot, all of a sudden, mount a list from the rear just because its has a name that ends in `r` (for ridiculous). No siree uncle Bob!


The fact is, both foldl and foldr can only start consumming a list from the only accessible end - the front end, and also - in the left to right direction only!


- function
  - name  : fold
  - arity : 3
  - return: `b`
- parameters
  1. parameter
    - entity : function
    - name   : step, reducer
    - arity  : 2
    - return : `b`
    - params :
      * `x`: el, element, listElement
      * `z`: acc, accumulator, initial, defaultValue
  2. parameter
    - type: `b`
    - name: `z`
    - desc: accumulator, initial or default value
- type of list          : `[a]`
- type of list elements : `a`
- type #1 of step       : `(a -> b -> b)`
- type #2 of step       : `(b -> a -> b)`
