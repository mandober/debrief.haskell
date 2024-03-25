# Continuations

- title: Continuations
- draft: https://dev.to/mandober/continuations-4fi4-temp-slug-8600421/edit
- tags: [haskell], [continuations], [cps], [plt]

Continuations are a programming construct that allows computations to be suspending and resumed later.

There are special forms (lisp terminology) for creating snapshots of the call stack which act as checkpoints that can be saved and returned to later.



## TLDR
- `⎡∙⎤` marks a hole (current redex), replaces [∙]
- `⟨∙⟩` marks a delimited continuation


Continuations are implicit in all PLs, but only languages with first-class functions can manipulate continuations explicitly. Continuations also allow us to define some control flow operators, such as exceptions, multithreading, logic programming, nondeterminism.

Flow control
- jumps: GOTO, functions, while-loops (continue/skip; break to a label)
- conditional statements: if-then-else
- early exit: return (functions), continue, break (loops)
- exceptions (try-catch, -finally, throw, re-throw)
- lazy evaluation: defer, delay, force
- threading: create/pause/stop a thread, yield control
  - coroutines
  - generators: yield
- nondeterminism
- logic programming
- etc.





## Definition of a continuation

A simple program would be the one consisting of one expression that is itself simple, in that it has no subexpressions. We could call a program complex if contains expressions that are in turn composed of multiple subexpressions. When such a program is undergoing execution, a complex expression is evaluated by first evaluating all the subexpression with it.

Considering an expression, which we'll call the *overall expression*, the evaluation proceeds by selecting the next redex to be evaluated. The selection is driven by the grammar and syntax rules of the language, for example, the precedence rules, or even explicitly by using parenthesis to group the subexpressions. So, the next subexpression to be evaluated is picked, and it becomes the current subexpression or **the current redex**.

To reitarate, we can even execute a functional program manually (mentally) with a relative ease thanks to *equational reasoning*. 

Equational reasoning relies on the fact that we can always replace equals for equals. 


allows us to 

In other words, we will be proving that one expression is equal to another.


evaluation precedes the execution of a functional program, and the former is done 

we have an overall expression undergoing evaluation, which is done by first evaluating the subexpresions it consists of.


the subexpression currently selected to be evaluated next is called the current redex.


it is then evaluated, and the result is used to execute the rest of the computation. It can also be said that the rest of the computation depends on the 

This last "rest of the computation" is the continuation (of the redex). That's why the concept of continuations arises in any PL.


## Running example

Considering this in terms of the following expression

```hs
e = 3 + 5 * 2 - 1
```

The *overall expression* is `3 + 5 * 2 - 1`. To evaluate it, the next redex to be evaluated is selected - here, that's the `5 * 2` subexpression and we'll refer to it as **the current redex** (or the current expression). The current redex is emphasized by enclosing it in the brackets, `3 + ⟬5 * 2⟭ - 1`. The bracketted subxpression represent the redex selected for evaluation, which we also referred to as a **hole**, `3 + ⟬∙⟭ - 1`, denoting it by `⟬∙⟭` to better emphasize the surrounding context. **The current continuation** is exactly the expression surrounding the hole.


We call it a hole and denote it by `⟬∙⟭` to better show what the continuation is: 

We can imagine it as some expression whose value will eventually be computed, but for now it is unknown, hence it represents a hole in the computation, denoted by `⟬∙⟭`. 


>The surrounding expression is the *continuation of the current redex*.

```hs
-- the overall expression
exp = 3 + 5 * 2 - 1
-- the current redex
exp = 3 + ⟬5 * 2⟭ - 1
-- the current redex is a hole
exp = 3 + ⟬∙⟭ - 1
-- the surrounding expression is the continuation of the current redex
exp = 3 + ⟬∙⟭ - 1
-- we can think of this as a function
exp = \x -> 3 + x - 1
```

>In summary, there is the overall expression, and one of its subexpressions is selected as the next redex - it is then referred to as the current redex. The surrounding expression (context) is the continuation of the current redex.

We can also think of this as being similar to a function, especially with the current redex marked as a hole.


exp = 3 + ⟬∙⟭ - 1
exp = \x -> 3 + x - 1

the hole as the place where a param occurs in a fn





- current expression:   `3 +  5 * 2  - 1`  (to evaluate)
- current redex:        `3 + [5 * 2] - 1`  (the hole)
- current continuation: `3 +   [·]   - 1`  (the surrounding context)

⟨ a ⟩        ⟪ b ⟫     ⧼ c ⧽     ⦑ d ⦒
⦓ e ⦔     ⦕ f ⦖    ⦉ g ⦊     ⦇ h ⦈
⦅ i ⦆        ⦃ j ⦄      ⟮ k ⟯     ⦗ l ⦘
⟬5*2⟭      ⟦ n ⟧     ⦋ o ⦌     ⦍ p ⦎
⦏ q ⦐        ‹d›       «h»
