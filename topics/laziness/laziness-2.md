# Laziness

https://en.wikibooks.org/wiki/Haskell/Laziness

<!-- TOC -->

- [Introduction](#introduction)
- [Non-strictness and laziness](#non-strictness-and-laziness)
- [Evaluation forms](#evaluation-forms)
  - [Refutable pair pattern](#refutable-pair-pattern)
  - [Irrefutable plain patterns](#irrefutable-plain-patterns)
  - [Refutable list pattern](#refutable-list-pattern)
  - [Evaluation forms](#evaluation-forms-1)
  - [Bang patterns](#bang-patterns)

<!-- /TOC -->


Laziness
- Introduction
- Nonstrictness vs laziness
- Weak head normal form
- Lazy and strict functions
  - Black-box strictness analysis
  - In the context of nonstrict semantics
  - The denotational view on things
- Lazy pattern matching
  - When does it make sense to use lazy patterns?
- Benefits of nonstrict semantics
  - Separation of concerns without time penalty
  - Improved code reuse
  - Infinite data structures
- Common nonstrict idioms
  - Tying the knot
  - Memoization, Sharing and Dynamic Programming



## Introduction

Haskell uses *lazy evaluation* meaning that nothing is evaluated until necessary.

Intuitivelly, we might think that lazy evaluation makes programs more efficient, and it does, but sometimes it can also introduce an overhead that leads programmers to search for places where they can make the code strict.

Laziness is detrimental for deterministic near real-time programming; that is, it makes the correct assessment about duration of a program's execution very difficult, if not impossible.

The real benefit of laziness is in making the right things *efficient enough*. Lazy evaluation allows us to write more simple, elegant code than we could in a strict environment.

Laziness is a necessary consequence of purity. [how?]


## Non-strictness and laziness

Evaluation strategy can be strict and non-strict, and laziness is a way to implement non-strictness. Laziness and non-strictness are slightly different but since they are very closely related, they are often used interchangibly. The concept of thunks helps understanding nonstrictness, and the semantics of nonstrictness explains why the lazy evaluation is used.

* *Nonstrict semantics* refers to the property of Haskell programs that is guaranteed, namely, that nothing will be evaluated until it is needed.

* *Lazy evaluation* is a mode to implement non-strictness using thunks.

* *Thunks* are nullary functions stored on the heap. They are also used in strict languages to introduce laziness via delayed execution. Instead of computing a value and returning it immediately, a function returns a thunk instead that wraps the computation which is executed later, on demand. However, thunk must be managed manually in strict languages, while a lazy language does it automatically (like it does e.g. currying).


## Evaluation forms

* *Nonstrictness* is a property of semantics that guarantees that the evaluation is delayed for as long as possible, and then the code is evaluated as little as possible, only as much as it is absolutely necessary.

* Haskell values are highly layered: they may be boxed, lifted, inside constructors or wrappers, so evaluating a value has different outcomes in regard to its layers. There may be many layers wrapping a value, so each evaluation step peels off a layer.


### Refutable pair pattern

For example, we consider the let-expression:

```hs
-- (1) refutable "pair" pattern
let (x, y) = (length [1..5], reverse "olleh") in ...
```

If x and y are not used in the `in` part of let binding, the `let` wouldn't be evaluated at all; if the RHS of was equal to `undefined`, it wouldn't cause an error, provided the `in` part doesn't use `x` or `y`.

We can assume that `x` is 5 and `y` is "hello", but actually the calculation of `length` and `reverse` is not performed until absolutely necessary. Instead, both x and y are turned into thunks, each holding a "recipe" for how to compute them; x's recipe might look like `x = length [1..5]`.

```hs
-- (2) refutable "plain" pattern
let z = (length [1..5], reverse "olleh") in ...
```

### Irrefutable plain patterns

In fact, the let expression contains pattern matching, `(x, y) = ...`, on the LHS which is a refutable pattern that produces an error if the matched value of RHS cannot be molded as a pair. If we replace it with an irrefutable pattern with a refutable one, the compiler would see that we're not trying to deconstruct the RHS value, so it wouldn't care what it contains. The compiler would make `z` a thunk, that would be further examined only if we reference it later.

We can see the layers of Haskell values when we pattern match on `z`:

```hs
let z = (length [1..5], reverse "olleh")
    (n, s) = z
in ...
```

After the first line, `z` is a thunk and the compiler knows nothing about what sort of value it is because it doesn't need to, at least not yet.

In the following line we pattern match against `z` using a pair pattern, which is refutable, so the compiler must make sure that the value to match is indeed a pair. However, it doesn't concern itself with the components yet (which contain the computations of `length` and `reverse`), so they remain unevaluated.

The var `z` that begun as a completely unevaluated thunk, e.g. `z = <thunk>`, got evaluated a little, its outer layer got peeled off revealing a tuple of two components, e.g. `(<thunk>, <thunk>)`.

Consequently, `n` and `s` are thunks, which will become the components of the `z` bound pair, when they get evaluated.


### Refutable list pattern

Now we add a new line with a slightly more complicated pattern:

```hs
let z     = (length [1..5], reverse "olleh")
   (n, s) = z
   'h':ss = s
in ...
```

The added line contains a pattern that tries to match the second component of `z`, causing further evaluation. The compiler must check whether the pattern `'h':ss` matches it.

The refutable "list" pattern causes the compiler to evaluate the outer layer of `s` to ensure that it is a cons cell, e.g. `s = <thunk> : <thunk>`.

Since, this is a refutable pattern for a cons cell, matching the empty list would cause an error; the empty list can be matched using the refutable pattern `[]` or an irrefutable one.

The compiler then evaluates the first revealed thunk to make sure it complies with the `'h'` literal pattern. The whole pattern, `'h':ss` matches a non-empty list whose head element is an 'h' character; if the match succeeds, the tail gets bound to the variable `ss`, for now another thunk, but later, when evaluated, produces the list (the tail of the origial list, that is).

### Evaluation forms

Almost all of the Haskell values support partial evaluation, that is, a step wise evaluation that can be adjusted to peel off one layer at the time, giving rise to the notion of the minimum amount of evaluation.

* With a thunk that pressumably holds a pair, the minimum amount of evaluation reveals the pair constructor, `(,)`, with two unevaluated components: `(,) <thunk> <thunk>`.

* With a list, the minimum amount of evaluation reveals either a cons cell, `<thunk> : <thunk>`, or the empty list, `[]`. In the case of the empty list, no further evaluation is possible since the value is in the *normal form*.

* At any intermediate step of evaluation, a value is said to be in the **weak head normal form** (WHNF). There is also a *head normal form*, but it's not used in Haskell. Fully evaluating a WHNF reduces a value to the **normal form**.

* If at any point we want to print a value, e.g. print `z`, it would first be necessary to fully evaluate it.

* Performing any degree of evaluation on a value is called **forcing** a value.

* The number of forms a value could be in, depends on its type. Primitive values, like integers, cannot be partially evaluated - they are either in the form of a thunk or in the normal form (i.e. fully evaluated). Primitive types have the notion of liftedness - normally, primitives are lifted, meaning they are boxed and located on the heap, for each type has an extra value called the bottom. However, it is possile to have unlifted (primitive) values, e.g. the "naked" integer, which is of `#` kind.

### Bang patterns

The granularity of evaluation can be influenced with different mechanism. The **bang pattern** is a way to make parts of a data structure strict, so that any amount of evaluation puts them in the normal form.

Parts of data constructors can be made strict by annotating them with a bang:

```hs
data Opt a = No | Yes !a

data List a = Nil | Cons !a !(List a)
```

The components prefixed with the bang sigil become evaluated as soon as we evaluate the layer above. In the example above, there will never be a form like e.g. `Yes <thunk>`; as soon as we get to this level, the strictness annotation on `a` which is inside the ctor `Yes`, forces its evaluation fully.


> The only place where values get evaluated are pattern matching sites and inside some primitive IO functions.


### Lists

Lists are sequences of 2-fields cons cell and they are destructured into head and tail components. The head component is a distinct cons cell with the 'data' field holding a value and a 'link' field that points to the next cons cell.

        :    s              Cons
       / \    p             / \       s
      1   :    i           1  Cons     p
         / \    n             / \       i
        2   :    e           2  Cons     n
           / \                  / \       e
          3  []                3   Nil


This diagram shows how some list operations can be done by considering only a list's spine without touching its values. For example, the `length` function need not examine a list's values to calculate its length, allowing it to be polymorphic and thus to work on any type of list.


## Lazy and strict functions

Functions can be lazy or strict in terms of their arguments, evaluating them to different layers.

For example, `length` needs to evaluate only the cons cells in the argument you give it, not their contents. The `length <thunk>` might evaluate to something like `length (<thunk> : <thunk> : <thunk> : [])`, which in turn evaluates to 3. Others need to evaluate their arguments fully, as in `length . show`. If you had `length $ show <thunk>`, there is no way you can do anything other than evaluate that thunk to normal form.

Given two unary functions, `f` and `g`, we say `f` *is stricter* than `g` if `f x` evaluates `x` to a deeper level than `g x`.

Often, we only care about WHNF, so a function that evaluates its argument to at least WHNF is strict and one that performs no evaluation is lazy.

Polyadic functions can be strict in one parameter but lazy in another. For example, the function: `f x y = length $ show x` demonstrates that `y` needs no evaluation, while `x` needs to be fully evaluated; therefore, `f` is strict in its first parameter but lazy in its second.

Memory problems when a function, like `foldl`, creates a lot of thunks can be curbed by using its strict version, `foldl'` (by convention, the apostroph marks a strict version of a function).

Essentially, `foldr (:) []` and `foldl (flip (:)) []` both evaluate their args to the same level of strictness, but `foldr` can start producing values immediately, whereas `foldl` needs to evaluate cons cells all the way down before it starts producing output.

### Black-box strictness analysis

To find out whether a unary function is strict, and you have no other means available, you can probe it with the `undefnied` value. Forcing the `undefined` value (to any level of evaluation) will error out the program.

```hs
-- all these will ERROR
let (x, y) = undefined in x
length undefined
head undefined
Yes undefined  -- data Opt = No | Yes !a
```

Strict functions end with an error immediately upon encountering `undefined`, while lazy functions do not.

```hs
-- no errors:
let (x, y) = (4, undefined) in x            -- since y is not referenced
length [undefined, undefined, undefined]    -- length doesn't look at the vals
head (4 : undefined)                        -- head needs only the first val
Just undefined                              -- wrapped val is not examined
```

## In the context of nonstrict semantics

If we consider a function like `id`, we might think it is lazy since it doesn't need to look at its arg. Apply the *black-box strictness analysis*, we conclude that `id undefined` is going to error, so `id` is strict after all. Uncertainty comes from Haskell's nonstrict semantics which make this issue somewhat vague; nothing gets evaluated if it doesn't need to be, according to nonstrictness.

The analysys of strictness cannot be done in GHCi since it must put everything we give it into normal form in order to print it. Everything in GHCi gets filtered through `IO`. In a Haskell program, nothing at all is evaluated until the `IO` in the `main` function is performed. So, it makes no sense to say whether something is evaluated or not, unless we know what it is being passed to, one layer/level up.

Instead of wandering if `f x` forces `x`, we should really ask whether `x` gets forced as a consequence of our forcing of `f x`. If we force `id x` to normal form, then `x` will be forced to normal form as well, letting us conclude that `id` is strict. But `id` itself doesn't evaluate its arg, it just hands it to the caller that possibly will.

```hs
-- this pattern match causes evaluation, forcing a value to WHNF

let (x, y) =    undefined in x -- Error, because we force undefined
let (x, y) = id undefined in x -- Error, because we force undefined

-- id doesn't prevent the forcing, so it is strict.
-- contrast this with a lazy function like const:

let (x, y) = undefined in x         -- Error, because we force undefined
let (x, y) = const (3,4) undefined  -- No error for const is lazy

head $ map (2*) [3, undefined]      -- 6
```

The last example shows the laziness of `const` - it doesn't even register its second argumnet because it is not used within its body. It can be any value, even `undefined`; it is only needed as a trigger to `const` to complete and output its result (which is its first argument).

The strictness of a function can be summed up very succinctly with *denotational semantics*: `f` is strict iff `f` evaluates bottom to bottom.

> f ⊥ = ⊥ <=> f is strict

Assuming that, we can say that everything with type `forall a. a`, including `undefined`, `error "error description"`, `throw`, etc, has denotation `⊥`.


## Lazy pattern matching

Normally, when you pattern match on a data ctor, an evaluation step is performed, peeling off one level of the ctor, to check that the value complies.

For example, matching a `Maybe`, in (1), will evaluate the value to reveal the ctor, to see if it is `Just`, `Nothing` or something else entirely; of course only a value wrapped in `Just` will match.

These kind of patterns that may fail are called **refutable** patterns. On the other hand, patterns like variable names, e.g. `x`, or the discard pattern `_`, are **irrefutable** and will match any value.

However, a refutable pattern can be made into an irrefutable, lazy, pattern by prepending it with the tilde sigil, `~`. This just makes the pattern match immediately succeed, but when the pattern is later referenced it will fail if it doesn't match the value.

```hs
-- (1)
f g (Just x) = Just (g x)

-- (2)
let f (Just x) = 1 in f (Just undefined)

-- (3a)
(***) f g (x, y) = (f x, g y)
```

A function like (`***`) in (3a), that lacks the tilde sigil on the third param, would evaluate the third argument to make sure the value matches the pattern, which in this case is a pair ctor, `(,)`. The components of the tuple won't be evaluated, just the 'top level/layer', as in (2).

However, with the tilde sigil prepended on the pair pattern, as in (3b), the pattern becomes irrefutable and the evaluation of the value is delayed until the component parts are referenced. In case the match fails, the RT error is issued.

To illustrate the difference:

```hs
-- (3b)
(***) f g ~(x, y) = (f x, g y)

-- (4a)
Prelude> let f (x,y) = 1
Prelude> f undefined
*** Exception: Prelude.undefined

-- (4b)
Prelude> let f ~(x,y) = 1
Prelude> f undefined
1

-- (5)
Prelude> (const 1 *** const 2) undefined
(1,2)
```

In the example (4a), the value is evaluated because it has to match the tuple pattern. But evaluating `undefined` yields `undefined`, which is an error.

In the example (4b), the matching is skipped, delayed for until it's referenced later; but it turns out the value is never, so it doesn't matter that `undefined` is passed.

In the example (5): if the pattern in (`***`) wasn't irrefutable, this would have failed.

### Use cases for lazy patterns

* Essentially, use lazy patterns when you only have the single ctor for the type, pretty much only with tuples.
* Multiple equations won't work nicely with irrefutable patterns.

To see this, let's examine what would happen if `head` had irrefutable patterns:

```hs
head :: [a] -> a
head ~[]     = undefined
head ~(x:xs) = x
```

With both equations using a lazy pattern, we need not evaluate even the top level of the arg until absolutely necessary, so we don't even know if it's the empty list ctor or a cons ctor. The pattern in the first equation will just match succeed, so we always return `undefined`.

## Benefits of nonstrict semantics

* Separation of concerns without time penalty
* Code modularity, improved code reuse
* Infinite data structures
* "Tying the knot" idiom
* Memoization, Sharing, Dynamic Programming


### Separation of concerns without time penalty

Lazy evaluation encourages programming in a way that feels very natural. For example, to get the first 5 elements of a sorted list (provided it uses the favorable sorting algorithm), we just say `take 5 $ sort xs`. Laziness will make sure the list is sorted just up to its first 5 elements and no further.

Translating this into an imperative language, requires thinking about all the things that will be performed implicitly with the command. In this case, the list will surely be completely sorted, before any other command is performed.

So, lazy strategy merges the functions into a pipeline where each does as little as possible to get the overall result.

In general, expressions that compose pruning with a list generatation, like `prune . generate`, where `generate` produces a list of items and `prune` cuts it down, will always be more efficient in a nonstrict language because, on average, `generate` won't have to run to its end before the result is produced.

In CS, there are many tradeoff that affect all parts of computations, so there are definitely times when a strict lnguage will have advantage. However, while it is possible to force strictness in a lazy language like Haskell, it is significantly more difficult, if not nearly impossible, to force laziness consistenly in a strict language.

When a programmer needs strictness, Haskell offers ad hoc solutions like the bang pattern, but it also has a module-wise solution in form of the Strict Haskell extension.


### Improved code reuse











## Refs

https://en.wikibooks.org/wiki/Haskell/Laziness
https://wiki.haskell.org/Haskell/Lazy_evaluation
https://wiki.haskell.org/Performance
https://en.wikibooks.org/wiki/Haskell/Performance_introduction
https://wiki.haskell.org/Performance/Laziness
https://wiki.haskell.org/Performance/Strictness
https://en.wikibooks.org/wiki/Haskell/Graph_reduction
https://wiki.haskell.org/Performance/Data_types
https://wiki.haskell.org/Thunk
