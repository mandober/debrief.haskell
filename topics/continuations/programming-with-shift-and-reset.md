# Programming with shift and reset

Programming with shift and reset    
paper by Kenichi Asai and Oleg Kiselyov, 2011    
http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-e.pdf

## Abstract

The concept of continuations arises naturally in programming:
- a conditional branch selects a continuation from the two possible futures
- raising an exception discards a part of the continuation
- a tail-call (or a GOTO statement) continues with the continuation

Although continuations are implicitly manipulated in every PL, manipulating them explicitly, as first-class objects, is rarely offered due to the perceived difficulty.

This tutorial aims to give a gentle introduction to continuations and a taste of programming with first class *delimited continuations* using the control operators `shift` and `reset`.

The tutorial explains how to write simple *coroutines* and *nondeterministic searches*. Basic familiarity with FPLs (Haskell, OCaml, Standard ML, Scheme) is assumed, but requires no prior knowledge on continuations.

## Keywords

* Terms
  - overall expression
  - the current redex, the current subexpression
  - hole (subexpression context), `⎡∙⎤`
  - the current continuation (surrounding context wrt a hole)
  - delimited continuation, `⟨∙⟩`, part of the surrounding context wrt a hole

* Keywords
  - CPS
  - first-class continuations
  - continuations
  - delimited continuations
  - `shift`
  - `reset`
  - `callCC`, call-with-the-current-continuation
  - `Cont`, `ContT` in mtl
  - control flow
  - control flow operator, control operators
  - control effects, side effects, effectful computation
  - exceptions
  - coroutines
  - nondeterministic searches

## Refs

* Continuations and Delimited Control
https://okmij.org/ftp/continuations/

* Programming with shift and reset
http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-e.pdf

* Delimited continuations in Haskell
https://okmij.org/ftp/continuations/Haskell-tutorial.pdf

* Module `Control.Monad.Trans.Cont` (based on this paper)
https://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.Cont.html


## Introduction

>Continuations represent the rest of the computation.

Manipulation of continuations is a powerful tool to realize complex control flow of programs without spoiling the overall structure of programs. It is a generalization of exception handling, but is far more expressive.

Traditional way to handle continuations explicitly in a program is to transform a program into continuation-passing style (CPS). However, it requires us to transform whole the program (or at least the part of programs where we want to manipulate continuations) into CPS. If we want to manipulate continuations in a more intuitive direct-style program, we need control operators.

The most famous control operator is `call/cc` found in Scheme and SML. However, continuations captured by call/cc is the whole continuation that includes all the future computation. In practice, most of the continuations that we want to manipulate are only a part of computation. Such continuations are called *delimited or partial continuations*.

There are several *control operators* that capture delimited continuations
- Felleisen's control/prompt
- Danvy and Filinski's shift/reset
- Gunter, Remy, and Riecke's cupto/prompt

Among them, we use `shift`/`reset` in this tutorial, because their foundation is established the most, having sound and complete axiomatization as well as polymorphism. Furthermore, because of its clear connection to CPS, many applications of shift/reset have been proposed. In this tutorial, we introduce programming with shift and reset with various examples.

## The current continuation

- `⎡∙⎤` marks a hole (current redex), replaces [∙]
- `⟨∙⟩` marks a delimited continuation

Continuations represent the rest of the computation. When a complex program is executed, the next redex to be evaluated is selected, evaluated, and the result is used to execute "the rest of the computation". This last "rest of the computation" is the continuation of the redex. Thus, the concept of continuations arises in any PL regardless of whether control operators are supported or not.

Continuations are relative to where the current expression being evaluated is.

- current expression:   `3 +  5 * 2  - 1`  (to evaluate)
- current redex:        `3 + ⎡5 * 2⎤ - 1`  (the hole)
- current continuation: `3 +   ⎡·⎤   - 1`  (the surrounding context)

For example, consider a simple arithmetic expression `3 + 5 * 2 - 1`. If we are about to execute the `5 * 2` redex, then **the current expression** is `5 * 2`, indicated with brackets as `3 + ⎡5 * 2⎤ - 1`. At this moment, **the current continuation** is the surrounding context, `3 + ⎡·⎤ - 1`. In other words, given a value for the hole ⎡·⎤, add 3 to it and subtract 1 from the sum. In this way, a continuation is similar to a function in that it also receives a value (for its hole) and evaluates the rest of the computation using it.

We can also understand *the current continuation* as the discarded computation when the current expression is replaced with an abort directive, like `raise Abort`.

In the example, the current continuation of `3 + ⎡5 * 2⎤ - 1` is `3 + ⎡·⎤ - 1`, since it is the discarded computation when executing `3 + ⎡raise Abort⎤ - 1`.

The current continuation changes as the computation proceeds. After `5 * 2` is evaluated, the expression is `3 + 10 - 1`.

The next redex to evaluate, `3 + 10`, is selected so
- the current expression (hole): `⎡3 + 10⎤ - 1`
- the current continuation:           `⎡·⎤ - 1`

After the evaluation of `3 + 10`, the overall expression is `13 - 1`. Then the only remaining redex is `⎡13 - 1⎤`, so it is itself the hole `⎡·⎤` with no surrounding context. At this point, the current continuation is the empty context, a noop.

```hs
-- the exp:
e = (if 2 == 3   then "delimited" else "partial") ++ " continuations"
-- the current redex:
e = (if ⟨2 == 3⟩ then ...
-- the current cont:
e = (if ⟨·⟩      then ...

-- the exp:
fst (let x = 1 + 2 in (x, x))
-- the current redex:
fst (let x = ⟨1 + 2⟩ in (x, x))
-- the current cont:
fst (let x = ⟨·⟩ in (x, x))
```

## Delimited continuations

> **Delimited continuation** is a continuation whose extent is delimited.

Rather than taking the entire continuation sometimes we want to capture only a part of it. The extent to which the current delimited continuation spans is explicitly designated by `⟨…⟩`.

For example, in the expression `⟨3 + ⎡5 * 2⎤⟩ − 1`, the current delimited continuation is only `3 + ⎡·⎤` - it doesn't include the final subexp, `− 1`.

## Delimiting continuations: reset

In OchaCaml, continuations are delimited by the `reset` construct:

>reset (fun () -> M)

It receives a thunk `(fun () -> M)` and executes its body `M` in a delimited context. If a continuation is captured during the execution of `M`, it is delimited up to this `reset`. When `M` evaluates to a value, it becomes the value of this whole `reset` expression.

For example, in the following expression:

reset (fun () -> 3 + ⎡5 * 2⎤) - 1

the current delimited continuation at `⎡5 * 2⎤` becomes "add three" and does not contain "subtract one". In this example, however, because continuations are not captured, the execution goes without any surprise: `5 * 2` is reduced to 10, 3 is added, 13 becomes the result of `reset`, and the final result is 12.

## Capturing continuations: shift

To capture the current delimited continuation in OchaCaml, we use the `shift` construct:

>shift (fun k -> M)

The execution of this expression proceeds in three steps:
1. The current delimited continuation is cleared
2. The cleared continuation is bound to `k`
3. The body `M` is executed

## Discarding delimited continuations

The first use of `shift` is to discard delimited continuations by:

>shift (fun _ -> M)

The execution of `shift (fun _ -> M)` proceeds as follows:
1. The current delimited continuation is cleared
2. The cleared continuation is passed to `fun _ -> M` as an (ignored) arg
3. The body `M` is executed

Since the body of `shift` doesn't mention the captured continuation, it is discarded and the current context (up to the enclosing `reset`) is replaced with `M`. In other words, we can discard/abort the computation up to the enclosing `reset`.
