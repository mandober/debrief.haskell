# Folding a structure

https://www.haskellforall.com/2021/02/folds-are-constructor-substitution.html
https://www.cs.cornell.edu/courses/cs3110/2017fa/l/06-hop/notes.html


## Overview

Folds are not limited only to lists and trees, in fact, *any inductively defined type is foldable*, including `Maybe`, `Either`, `(,)`, `Array`.

> Every data structure has the canonical folding function that takes as its arguments expressions where each corresponds to a particular data ctor of the structure. To fold a data structure means to replace its data constructors with the corresponding expression (usually a function).

Haskell's has a family of functions that perform folding of structures, but only `foldr` is the canonical folding function, because `foldl` doesn't work by replacing structure's data ctors.

Both `foldr` and `foldl` take a binary function, `step`, that does the actual work, an initial value, and a list, but while their base case equation is the same, they differ in the equation for the recursive case.

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


## Folds

The two basic folding functions are `foldl` and `foldr`, which are often considered in terms of lists, and it is said they differ in the way each processes a list: foldl processes elements from the front of a list, while foldr does it from the rear. This is not entirely correct as they both take list elements from the front. The real difference between them is in the definitions of their recursive cases.

Both `foldl` and `foldr` are ternary functions taking a binary function (which does the actual work), an initial value (aka accumulator or default value), and a list.

They are defined recursively with identical base-case equations, which account for what happens when the supplied list is empty; in that case, the default value, bound to the param `z`, is returned (and the binary function can be ignored by matching it against the `_` pattern).

```hs
-- base case
fold f z [] = z
```

The second equation that deals with the recursive case is distinct for each function. It begins with the LHS pattern matching that is the same for both: binding the binary function as `f`, the accumulator as `z`, and the list by matching it against the usual `(x:xs)` deconstructing pattern.

```hs
-- rec case
fold f z (x:xs) = rhs where
```


## Functions as data structures

Data structures can be represented as functions, commonly in the shape of a fold which replaces the structure's data ctors with one of the functions it receives as an argument. Each data ctor corresponds to a specific expression that the fold acepts.

For example, the common list has 2 data ctors, Cons and Nil. `Nil` is a nullary data ctor that constructs the empty list, while `Cons` is a binary ctor that constructs a new list by prepending the supplied element to the supplied (old) list.

```hs
-- common list
data List a = Nil | Cons a (List a)

-- nil data ctor
Nil :: List a
nil :: b

-- cons data ctor
Cons :: a -> List a -> List a
cons :: a -> b -> b

b ~ List a

data SList a = SList (forall b. (a -> b -> b) -> b        -> b)
--                               c  o  n  s     nil  list   ret
foldr :: Foldable t =>          (a -> b -> b) -> b -> t a -> b
```

This is the same data type as list, so it can implement all the same library functions for lists. The key difference from a list (`[a]`) is that you get elimination (pattern-matching) for free (because that's what a fold does), but not construction (you'll have to write them); and for `[a]` it is the other way around (you get DS introduction, but you have to write elimination).

I'm not sure why does the foldr have an extra arg, or why doesn't SList have one more?


```hs
data Maybe a = Nothing | Just a

fold :: maybe -> (a -> maybe) -> Maybe a -> maybe
fold nothing just  Nothing  = nothing
fold nothing just (Just x ) = just x
```
