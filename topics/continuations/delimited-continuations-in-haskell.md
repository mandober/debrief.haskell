# Delimited continuations in Haskell

https://okmij.org/ftp/continuations/Haskell-tutorial.pdf

- `⎡∙⎤` marks a hole (current redex), replaces [∙]
- `⟨∙⟩` marks a delimited continuation


This is about programming with delimited control in Haskell. *Delimited control*, like its instance, *exceptions*, is an *effect*. Therefore, we have to use monads. We'll use the `Cont` monad from mtl, the monad intended to manage *delimited control*.

## Refs

* Continuations and Delimited Control
https://okmij.org/ftp/continuations/

* Programming with shift and reset
http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-e.pdf

* Delimited continuations in Haskell
https://okmij.org/ftp/continuations/Haskell-tutorial.pdf

* Module `Control.Monad.Trans.Cont` (based on this paper)
https://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.Cont.html

## Introduction to the Cont monad programming

As the running example, we'll use the expression

```hs
reset (3 + shift (\ k -> 5 * 2)) - 1
```

This example is OchaCaml code, not a valid Haskell code. We first need to convert it into a Haskell expression and that in the monadic style. This can be done almost automatically:
- move the infix operators into prefix positions
- `return` all literal numbers
- lift all binary operators, (+), (*) and (-)

```hs
t1 = reset (3 + shift (\ k -> 5 * 2)) - 1

-- 1) infix into prefix operators:
t1 = (-)
      (reset
        ((+) 3 (shift (\ k -> (*) 5 2))))
      1

-- 2) return all literal numbers
t1 = (-)
      (reset
        ((+) (return 3) (shift (\ k -> (*) (return 5) (return 2)))))
      (return 1)

-- 3) lift binary operators
t1 = liftM2 (-)
      (reset
        (liftM2 (+) (return 3)
          (shift (\ k -> liftM2 (*) (return 5) (return 2)))))
      (return 1)
```

Dude, Haskell looks like a Scheme-y, ah-ah ah-ah.

>>> type 1
1 :: Num a => a

>>> type (return 1)
(return 1) :: (Num a, Monad m) => m a

Whereas the literal `1` is a number, `return 1` is a computation (in some monad `m`) that produces the number but may also do something else, like printing, or, in our case, throwing exceptions and performing other *control effects*.

>Types can tell us a lot about expressions. After all, a type is an approximation of expression's behavior, outlining the behavior of an expression without running it.

GHC infers the type of the expression `t1` (from above) as

>>>:t t1
t1 :: Cont w Integer

`Cont w Integer` is an effectful expression, within a particular monad `Cont w` (hmm, it's either none or the `Identity` monad; but `ContT`, now, there's a monadic type), which is parametrized by the so-called *answer-type*.

To get the result, we have to run the expression, executing all its effects and obtaining its eventual result:

>>> runCont t1
9


The expression `t1` looks ugly, even to a Schemer. We can make it prettier using monadic laws:

```hs
-- ugly
t1 = liftM2 (-)
      (reset
        (liftM2 (+) (return 3)
          (shift (\ k -> liftM2 (*) (return 5) (return 2)))))
      (return 1)

-- prettier (?!)
t1 = liftM2 (-)
      (reset
        (liftM2 (+) (return 3)
          (shift (\ k -> return (5 * 2)))))
      (return 1)

-- that is
liftM2 (*) (return 5) (return 2) === return (5 * 2)
```
