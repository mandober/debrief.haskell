# Monads for functional programming
(Philip Wadler, University of Glasgow)

Using monads to structure FP programs and model effects found in impure langs.

3 analysed examples: Implementing...
1. Exceptions
2. State (global state)
3. Tracing (generating output)

3 case studies: How monads...
1. ease the modification of a simple evaluator
2. can be used to build parsers
3. act as basis of a datatype of arrays, subject to the in-place updates


```hs
-- Exceptions
type Exception = String
data M a = Raise Exception | Return a
eval :: Term -> M Int

-- State
type State = Int
type M a = State -> (a, State)
eval :: Term -> M Int

-- Tracing
type Trace = String
type M a = (Trace, a)
eval :: Term -> M Int
```


## TOC

<!-- TOC -->

- [TOC](#toc)
- [1. Pure and Impure](#1-pure-and-impure)
- [2. Monadic Framework](#2-monadic-framework)
- [2.1 The Basic Evaluator](#21-the-basic-evaluator)
- [2.2 Variation 1: Exceptions](#22-variation-1-exceptions)
- [2.3 Variation 2: State](#23-variation-2-state)
- [2.4 Variation 3: Output](#24-variation-3-output)
- [2.5 A monadic evaluator](#25-a-monadic-evaluator)
  - [Evaluator in terms of these abstractions](#evaluator-in-terms-of-these-abstractions)
- [2.6 Variation zero, revisited: The basic evaluator, v.0.1.1](#26-variation-zero-revisited-the-basic-evaluator-v011)
- [2.7 Variation one, revisited: Exceptions, v.1.1](#27-variation-one-revisited-exceptions-v11)

<!-- /TOC -->


<!-- #region -->

## 1. Pure and Impure

Functional Programming Languages (FP) are split into two schools:

*Pure FP (pFP)*
- pure and simple Lambda Calculi (Miranda and Haskell)
- the most expressive languages
- benefit from lazy evaluation (infinite types, computation savings, etc.)
- easier to reason about

*Impure FP (iFP)*
- impure languages with effects (Scheme and SML)
- augment Lambda Calculus with effects (assignment, exceptions, continuations)
- offer better performance

Advances in theoretical CS, in *type theory* and *category theory*, have discovered a way to integrate the benefits of both schools by *using monads to model side-effects* in pure FPL. The concept of monads, originating in the category theory, was applied by *Moggi* to structure *denotational semantics* of a FPL, but the same technique could be applied to structure functional programs.


## 2. Monadic Framework

In a pFP all flow of data is made explicit, sometimes painfully so. A program is written as a set of equations and *explicit data flow* (#getbandname) ensures that the value of an expression depends only on its free variables. Hence, substitution of equals for equals is always valid, making such programs especially easy to reason about. EDF also ensures that the order of computation is irrelevant, making such programs susceptible to *lazy evaluation*.

On the up side, *explicit data flow* is great for code *modularity*; all incoming and outgoing data is *rendered manifest and accessible*, providing a maximum of flexibility.

On the down side, the essence of an algorithm can become buried under the plumbing required to carry data from its point of creation to its point of use.

Considering a program in pFP, any statefull change would require modifying each recursive call. For example, to impl error handling we can't rely on exceptions like in iFP, but we have to check each recursive call and handle errors appropriately. The same goes for other requirements like tracing the execution (which can be solved with a global var in an iFP) or having some output (in an iFP, a function can have output as a side effect).

However, we can use monads to help us handle the exceptions, tracing, output. The use of monads to structure an evaluator allows us to impl changes, such as these 3 mentioned, easily. In each case, we'd just redefine the monad and make a few local changes. The monadic approach makes up for some of flexibility, but it doesn't eliminate all the tension between the flexibility afforded by explicit data and the brevity allowed by implicit plumbing.

We begin with the basic evaluator for simple terms, then consider variations that mimic exceptions, state and output. We analyse these for commonalities, then abstract the concept of a monad. We show how each variations fits into the *monadic framework*.

<!-- #endregion -->


<!-- #region -->

## 2.1 The Basic Evaluator

A term is a constant, `Literal a`, or `Div t u`, where `t` and `u` are terms: 
data Term = Literal a | Div t u

```hs
data Term = Literal Int | Div Term Term

-- basic evaluator, eval0
eval :: Term -> Int
eval (Literal a) = a
eval (Div t u) = eval t `div` eval u

-- example terms
ans, err :: Term
ans = (Div (Div (Literal 1972) (Literal 2)) (Literal 23))
err = (Div (Literal 1) (Literal 0))

-- eval
r0a = eval ans -- 42
r0e = eval err -- Exception: divide by zero
```


This evaluator does not incorporate error handling, so the result of `eval err` is `Exception: divide by zero`.


<!-- #endregion -->


## 2.2 Variation 1: Exceptions
<!-- #region eval1 -->

In a pFP we can mimic exceptions by introducing a type to represent computations that may raise an exception.

A value of type `M1 a` either has the form `Raise e`, where `e` is an exception, or `Return a`, where `a` is a value of type `a`.

```hs
type Exception = String
data M1 a = Raise Exception | Return a

eval1 :: Term -> M1 Int
eval1 (Con a) = Return a
eval1 (Div t u) = case eval1 t of
    Raise e -> Raise e
    Return a ->
        case eval1 u of
            Raise e -> Raise e
            Return b ->
                if b == 0
                then Raise "divide by zero"
                else Return (a `div` b)

r1a = eval1 ans -- Return 42
r1e = eval1 err -- Raise "divide by zero"
```

At each call of the evaluator, the form of the result must be checked:
- if an exception was raised it is re-raised
- if a value was returned it is processed

<!-- #endregion -->


## 2.3 Variation 2: State
<!-- #region eval2 -->

Now we want to count the number of divisions performed during evaluation.

In an iFP this is achieved by using a global var and incrementing it after each division. In a pFP, we can *mimic the state* by introducing a type to represent computations that act on a state.

A value of type `M2 a` is a function that accepts the *initial state* and returns the computed value paired with the *final state*.

At each call of the evaluator, the old state must be passed in, and the new state extracted from the result and passed on appropriately.

```hs
type State = Int
type M2 a = State -> (a, State)

eval2 :: Term -> M2 Int
eval2 (Con a) x = (a, x)
eval2 (Div t u) x = let (a, y) = eval2 t x in
                        let (b, z) = eval2 u y in
                            (a `div` b, z + 1)
-- eval
r2a = eval2 ans 0 -- (42, 2)
r2e = eval2 err 0 -- Exception: divide by zero
```

Computing `eval ans 0` yields `(42, 2)`, so with the initial state `0` the `ans` is `42` and the final state is `2`, indicating that 2 divisions were performed.

<!-- #endregion -->


## 2.4 Variation 3: Output
<!-- #region eval3 -->

Finally, to impl tracing of execution in an iFP, we can just insert output commands at appropriate points. In a pFP, we can mimic output by introducing a *type to represent computations that generate output*.

A value of type `M3 a` is a pair of generated output (string) and the computed value.

At each call of the evaluator, the output string must be collected and concatinated to form the output of the enclosing call. The function line generates one line of the output.

```hs
type Output = String
type M3 a = (Output, a)

eval3 :: Term -> M3 Int
eval3 (Con a) = (line (Con a) a, a)
eval3 (Div t u) = let (x, a) = eval3 t in
                  let (y, b) = eval3 u in
                  (x ++ y ++ line (Div t u) (a `div` b), a `div` b)

line :: Term -> Int -> Output
line t a = "eval3 (" ++ show t ++ ") ( " ++ show a ++ "\n"

r3a1 = eval3 (Con 1972)                                -- 1972
r3a2 = eval3 (Con 2)                                   -- 2
r3a3 = eval3 (Div (Con 1972) (Con 2))                  -- 986
r3a4 = eval3 (Con 23)                                  -- 23
r3a5 = eval3 (Div (Div (Con 1972) (Con 2)) (Con 23))   -- 42
```


Computing `eval ans` returns the pair `(x, 42)`, where `x` is the string which represents a trace of the computation.
<!-- #endregion -->


## 2.5 A monadic evaluator

<!-- #region monadic evaluator -->

Each of the variations on the interpreter has a similar structure, which may be abstracted to yield the notion of a **monad**.

In each variation, we introduced a type of computations. 
Respectively, `M` represented computations that could:
- raise exceptions
- act on state
- generate output

Evaluators
- the basic (original) ev: `eval0:: Term -> Int`
- in each version it was : `eval :: Term -> M Int`

In general, a function of type 
  `f :: a -> b` 
is replaced by 
  `g :: a -> M b`


This can be read as a function that accepts an argument of type `a` and returns a result of type `b`, with a possible additional effect captured by `M`. This effect may be to act on state, generate output, raise an exception, etc.


Examination of the examples reveals two sorts of operations that are required on the `M` type: *identity* (unit) and *composition* (chaining):
* we need a way to lift a value into a computation: `unit :: a -> M a`
* we need a way to apply `(a -> M b)` fn to a computation of type `M a`. 
  It is convenient to write these in an order where the argument comes before the function:
  `(⋆) :: M a -> (a -> M b) -> M b`

A **monad is a triple**, `(M, unit, ⋆)`, consisting of a type constructor `M` and 2 operations of the given polymorphic types. These operations must satisfy *3 monadic laws*.

Monadic expressions will often have the form: `m ⋆ λa.n`   
where `m` and `n` are expressions, and `a` is a variable. The form `λa.n` is a lambda expression, with the scope of the bound variable `a` being the expression `n`.

The above can be read as follows: 
perform computation `m`, 
bind `a` to the resulting value, 
and then perform computation `n`. 

Types provide a useful guide. From the type of (⋆), we can see that expression has type `M a`, variable `a` has type `a`, expression `n` has type `M b`, lambda expression `λa.n` has type `a -> M b`, and the whole expression has type `M b`.

The above is analogous to the expression: `let a = m in n`    
The analogy with `let` explains the choice of the order of arguments to ⋆. It is convenient for argument `m` to appear before the function `λa.n`, since computation `m` is performed before computation `n`.

In an iFP, this has the same reading: perform computation `m`, bind `a` to the resulting value, then perform computation `n` and return its value. Here the types say nothing to distinguish values from computations: expression `m` has type `a`, variable `a` has type `a`, expression `n` has type `b`, and the whole expression has type `b`.

### Evaluator in terms of these abstractions

The evaluator is easily rewritten in terms of these abstractions.

```hs
eval :: Term -> M Int
eval (Con a) = unit a
eval (Div t u) = eval t ⋆ \a -> (eval u ⋆ (\b -> unit (a `div` b)))
```

A word on precedence: lambda abstraction binds least tightly and fn application binds most tightly, so the last equation is equivalent to the following:

```hs
eval (Div t u) = ((eval t) ⋆ (\a -> ((eval u) ⋆ (\b -> (unit (a `div` b))))))
```

The type `Term -> M` Int indicates that the evaluator takes a term and performs a computation yielding an integer. To compute `(Con a)`, just return `a`. To compute `(Div t u)`, first compute `t`, bind `a` to the result, then compute `u`, bind `b` to the result, and then return `a ÷ b`.

The new evaluator is a little more complex than the original basic evaluator, but it is much more flexible. Each of the variations given above may be achieved by simply changing the definitions of `M` , `unit`, and `⋆`, and by making one or two local modifications. It is no longer necessary to re-write the entire evaluator to achieve these simple changes.

```
v.0.1.0 basic evaluator          : eval0 :: Term -> Int
v.0.1   evaluator with exceptions: eval1 :: Term -> M1 Int
v.0.2   evaluator with state     : eval2 :: Term -> M2 Int
v.0.3   evaluator with output    : eval3 :: Term -> M3 Int

v.0.1.1 basic evaluator          : eval0 :: Term -> Int
v.1.1   evaluator with exceptions: eval1 :: Term -> M1 Int
v.1.2   evaluator with state     : eval2 :: Term -> M2 Int
v.1.3   evaluator with output    : eval3 :: Term -> M3 Int

```

<!-- #endregion -->


## 2.6 Variation zero, revisited: The basic evaluator, v.0.1.1
<!-- #region zero revisited -->

In the simplest monad, a computation is no different from a value.

```hs
type M a = a

unit :: a -> I a
unit a = a

(⋆) :: M a -> (a -> M b) -> M b
a ⋆ k = k a
```

This is called the **identity monad**: `M` is the identity function on types, `unit` is the identity function, and (⋆) is just application.

Taking `M`, `unit`, and (⋆) as above in the monadic evaluator and simplifying, yields the basic evaluator from the beginning (zero evaluator).


<!-- #endregion -->

## 2.7 Variation one, revisited: Exceptions, v.1.1


```hs
type M a = a

unit :: a -> I a
unit a = a

(⋆) :: M a -> (a -> M b) -> M b
a ⋆ k = k a
```


v.1.2


```hs
type M a = a

unit :: a -> I a
unit a = a

(⋆) :: M a -> (a -> M b) -> M b
a ⋆ k = k a
```

v.1.3



```hs
type M a = a

unit :: a -> I a
unit a = a

(⋆) :: M a -> (a -> M b) -> M b
a ⋆ k = k a
```
