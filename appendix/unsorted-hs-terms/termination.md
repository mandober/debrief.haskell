# Termination of Haskell Functions

`AProVE` Web Interface
http://aprove.informatik.rwth-aachen.de/interface/v-Termcomp2020/haskell

Termination of Haskell Functions
http://termination-portal.org/wiki/Functional_Programming



## Termination of Haskell Functions

Within the Termination Competition, there is a category on FP where the object is to prove termination of Haskell functions automatically. Currently, the Termination Problems Data Base contains 1676 functions from the Haskell Prelude, and `AProVE` shows termination of 1294 of them.

This category compares tools that automatically analyze termination of Haskell programs.

If one considers a whole Haskell program, *termination* corresponds to finishing the computation in finite time on every possible input.

However, since Haskell uses lazy/on-demand evaluation, this notion is not directly useful for analysis of single functions. Consider for example a program containing:

```hs
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

bot :: a -> b
bot = bot

zeros :: [Integer]
zeros = 0 : zeros

strange :: (Integer -> Integer) -> Integer
strange f
    | f 23 == 42 = strange f
    | otherwise  = 5
```

> Intuitively, evaluation of a Haskell function (with every arg supplied) is terminating if the evaluation strategy needs finitely many steps to compute the result completely.

This means, that evaluation doesn't halt when a term headed by a constructor is reached (WHNF), but it keeps on going, evaluating all the args of the constructor. Hence, the function `zeros` above is considered non-terminating.

When analyzing the termination behavior of a function alone (without specific arguments), the picture gets slightly more complicated. If one allows arbitrary arguments, then nearly every Haskell function will not finish in finitely many steps. Even a function like `head` would be considered non-terminating if one allows non-terminating arguments: `head bot` will obviously not terminate. Hence, this idea is not useful. 

> Instead, one regards a function as terminating, if it finishes in a finite number of steps whenever it is called with terminating arguments.

This can be written formally as follows:

(DEFINITION) 
**Termination of Haskell Expressions** 
The set of terminating Haskell expressions is defined as the smallest set, such that `t` is in this set iff all of these conditions hold:

- `t` doesn't start an infinite evaluation sequence

- if `t` evaluates to an expression `C t₁ … tₙ`, 
  where `C` is a constructor, 
  then `t₁ … tₙ` must also be terminating expressions

- if `t` evaluates to an expression `f t₁ … tₙ`, 
  where `f` is a function symbol 
  and the type of `t` is `α -> β`, 
  then `f t₁ … tₙ t'` must be terminating 
  for every terminating expression `t'`


This means that in the examples above, the expression `map` would be considered terminating, while the expression `\f -> map f zeros` would be non-terminating. On the other hand, `head zeros` is obviously nonterminating.

The expression `\x -> if x == 23 then 42 else 5` is terminating, while the function `strange` is not. This is because of the 3rd point in the definition one needs to analyze `strange` called with any terminating argument (like our lambda expression) which would result in a non-terminating behavior.

The semantics of "termination" (in the context of the competition) is here
http://lists.lri.fr/pipermail/termtools/2006-March/000179.html


## The Problem Format

Every input for the FP category is a file containing a Haskell module with exactly the compiler pragma `htermination`, that specifies the start term. The start term is any Haskell expression containing no free variables. Function names, constructors etc. bound in the module itself can be used.

```hs
{-# htermination (foldr1 :: (a -> a -> a) -> (List a) -> a) #-}

import qualified Prelude

data MyBool = MyTrue | MyFalse
data List a = Cons a (List a) | Nil

foldr1 :: (a -> a -> a) -> (List a) -> a
foldr1 f (Cons x Nil) = x
foldr1 f (Cons x xs) = f x (foldr1 f xs)
```


## Implementations

The termination analyzer `AProVE` has a web front end at:
http://aprove.informatik.rwth-aachen.de/interface/submission
http://aprove.informatik.rwth-aachen.de/eval/Haskell/

tools for termination analysis
http://termination-portal.org/wiki/Category:Tools

Termination Competition
http://termination-portal.org/wiki/Termination_Competition

Termination Competition 2021 will be affiliated with CADE 2021
http://termination-portal.org/wiki/Termination_Competition_2021

CADE 2021 - The 28th International Conference on Automated Deduction
https://www.cs.cmu.edu/~mheule/CADE28/

The International Workshop on Termination (WST)
http://termination-portal.org/wiki/WST

The Termination Problems Data Base collects all the problems from competitions
http://termination-portal.org/wiki/TPDB

Automated Termination Proofs for Haskell by Term Rewriting
