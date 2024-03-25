# Low-level Haskell: an interactive tour through the STG

`Low-level Haskell: An Interactive Tour Through the STG`   
Talk by David Luposchainsky @ZuriHac 2016    
(5,315 views ∙ 7 Sept 2016 ∙ A Google TechTalk ∙ July 23, 2016)
https://www.youtube.com/watch?v=-MFk7PIKYsg

## Refs

* ZuriHac2016
https://wiki.haskell.org/ZuriHac2016

* STG machine - SPJ
https://www.microsoft.com/en-us/research/wp-content/uploads/1992/04/spineless-tagless-gmachine.pdf


## Talk - Abstract

Executing lazy functional programs is often met with suspicion, as something processors somehow don't do "naturally". In this talk, I will show just how wrong such statements are. We take a tour through a few well-known Haskell programs in their STG representation. STG is a tiny functional language used in GHC's compiler backend. What makes STG particularly interesting is it is feels close to both Haskell and the metal, and allows us to watch how Haskell programs are executed step-by-step.

Why does `foldl (+) 0` overflow, and is it the stack or the heap? How are things pushed onto the stack or allocated on the heap anyway, and what cleans them up again? How does laziness work? Why are unboxed values often faster, how does a list look like in memory, how efficient is the popular Quicksort-inspired example algorithm really?

The goal of the talk is to introduce the audience into the operational semantics of Haskell, by executing a couple of small programs everyone should be familiar with, and discussing what happens during execution.

```hs
-- Surface
main = x + y
x = 1
y = 2

-- 1) STG (very loosely, no class resolution)
main = \ => add x y
x = \ -> Int# 1#
y = \ -> Int# 2#
add = \ x y -> case x of
  Int# x' -> case y of
    Int# y' -> case +# x' y' of
      v -> Int# v

-- Surface
repeat x = let xs = x : xs in xs
-- 2) STG
repeat = \x -> letrec xs = \(x xs) -> Cons x xs in xs
--                          ^^^^^^ captured free vars

-- Surface
map [] = []
map f (x : xs) = f x : map f xs
-- 3) STG
-- STG has no LHS pattern matching (directly on values)
-- scrutinize them via the case exp instead.
map = \f list -> case list of 
  Nil       -> Nil
  Cons x xs ->
    let fx  = \(f x)  => f x
        fxs = \(f xs) => map f xs
    in  Cons fx fxs
```

STG is a minimal FPL, musch alike Haskell but more verbose. All the sugar is gone, like do-exp, also where clauses and pattern matching. Since placing args on the lhs is not allowed, lambda forms are used everywhere, for functions as well as for values - a value has an empty lambda, `x = \ -> Int# 1#`. No args on the lhs means no direct pattern matching, so scrutinize args through `case` instead. `case` evaluates thunks and so it forces exps. In the example (3) `case` will force the list into WHNF. The regular Haskell's `case` is not necessarily this strict - there, if you `case` an exp but don't demand the result, the exp (thunk) won't be forced. In STG, `case` unconditionally forces exps into WHNF. STG supports operator data ctors, and it normalizes function and operators so as to be in the prefix position.

Surprisingly, STG does not support HOFs - no fn as args, and you cannot nest functions. Functions always take atoms as arguments; an atom is either a variable, which is a pointer to the heap, or a primitive value, like an unboxed int. In the surface `map` example, the call `f x` on the rhs of the second equation is not executed; instead, this call creates a new thunk on the heap to repr `f x`. When you demand the value of `f x`, then it is going to force that thunk and update the value in place. So we allocate `fx`, which is the value of `f` applied to `x`, but we don't force it. We just put it down there in the `Cons` cell, and likewise for the recursive call to `map`.

The fat arrow is used to represent the update flag, so you mark a closure that evaluates a value as *updateable* or *non-updateable*.

```hs
  letrec a = \(x xs) => …   -- updateable
         b = \(x xs) -> …   -- non-updateable
  in …
```

**Updateable** flag means that once you have forced this closure to WHNF or NF - e.g. if you got the actual int value, and the closure is sufficiently hard to calculate - it may be desirable to avoid having to recalculate it again, in which case it overwrites the former closure with the result of its evaluation. That is how laziness is implemented in hardware. This may secretly update memory in place - however, this is no issue since the mutation is not observably mutable from the outside, because we only substitute things with their normal form.

Pattern matching is done only with `case`; `let` cannot pattern matching - it introduces new bindings, creates new thunks, and allocates heap memory.

So the STG language is even smaller than the Core language, and up to the update flag, it doesn't do things much differently then Core.

A non-exhaustive list of things that STG lacks:
- **types**
- HOFs, nested functions
- pattern matching except in `case`
- nested patterns
- guards
- where clauses
- if-then-else
- list comprehensions
- do notation
- classes, class instances, deriving clauses (all resolved long before STG)
- (any other sugar)

### Executing STG

The following (and preceding) syntax is mostly a feature of STGi package, not the proper GHC's STG syntax (but kinda sorta similar).

The program state consists of
- code segment: local vars, etc.
- stack: initially empty
- heap: heap is repr as a map from addresses (memory locations) to closures

Using the STGi package, this process may be imagined to look something like:

```hs stg
-- 0) Initial state
-- ----------------
code:  Eval main
       Locals: (empty)

stack: (empty)

heap:  (5 entries)
  0x00 -> Thunk \ => let not = \ not P in or not q
  0x08 -> Fun \ x => case x of …

globals: main -> 0x00
         not  -> 0x08
         or   -> 0x10
         p    -> 0x18
         q    -> 0x20

steps: 0
```

The heap, as a map of pointers to closures, has markers (`Thunk`, `Fun`, and ctor names), which are not evaluated and are only use to provide info to users, (not to GHC; This is a feature of STGi package, not proper GHC's STG syntax).

Markers
- `Thunk`
- `Fun`
- data ctors

These are the markers, and anything that's not a function (`Fun`) or a data ctor is marked with the `Thunk`.

The STG paper (by SPJ from 1992) list about 17 - 19 transition rules that specify what to do in a certain state, performing a single step evaluation.

Starting the execution of the program, the first thing is a function application of `main` to, well, no arguments. It is a rudamentary example of function application. The corresponding rule of function application says that if you want to apply the name `main`, you should first look it up to find out what it means. The lookup process first checks the *locals* list in the `code` segment - however, the *locals* are empty. The *globals* are checked next, and they have the identifier `main` pointing to a closure at the address 0x00. The execution jumps to that address, finishing the first step.

In fact, STGi uses the `code` as a section name - so, `code` contains the current exp, i.e. the expression to evaluate. So, the code for the `main` closure, found at the address 0x00, is now loaded there.

---

(it's all fun and games but this just describes how the author impl STGi library or whatchmacallit, not really detailing the actual proper STG and the execution process of GHC...Hard abort.)
