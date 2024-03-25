# Termination in Haskell

https://termination-portal.org/wiki/
http://lists.lri.fr/pipermail/termtools/2006-March/000179.html

## What is termination?

Obviously, "termination of all functions for all possible arguments" is not a good definition. Consider the following simple Haskell program:

```hs
head (x:_) = x
ones = 1 : ones
main = head ones
```

The program terminates, but the function `ones` does not. Thus, I would propose to use abstract start terms, i.e. Haskell terms possibly containing abstract variables. This roughly corresponds to the idea of abstract queries in Prolog:

A term `t` containing no abstract variables is "terminating" iff
1. its evaluation (according to Haskell strategy) is finite
2. all (type-correct) applications of `t` to a terminating term are terminating

>An abstract term `t` is terminating iff all (type-correct) instantiations of `t` which instantiate all abstract variables with terminating terms are terminating.

For the above example program the following abstract start terms (among others) are terminating:

```hs
head
head x
main
head ones
1 : [head ones]
```

Here `x` is an abstract variable.

Examples for abstract start terms that are not terminating:

```hs
ones
1 : ones
```

Thus, like for Prolog, there could be multiple abstract start terms (terminating and non-terminating) for one Haskell program.
