# Haskell functions

## Defining a function

Haskell 2010 offers two syntactic forms to define a function:
- function definition syntax
- lambda expression

The differences between these two forms
- function definition syntax
  - binds function to an identifier
  - can have multiple clauses (arbitrary number of equations, clause >= 1)
  - may be parameter-less (becoming more of a value then fn binder)
  - admits extended pattern matching, with guards
- lambda expression
  - creates an anonymous function expression
  - only has a single clause (clause = 1)
  - must have at least one parameter
  - admits restricted pattern matching, no guards

```hs
curry :: ((a, b) -> c) -> (a -> b -> c)
curry f a b = f (a, b)            -- function definition
curry f = \ a b -> f (a, b)       -- mixed
curry = \ f a b -> f (a, b)       -- lambda expression

map (Cons f) = let (c,d) = \ (a,b) -> f a b in (c, f d)
```

The let expressions don't express functions but function application:

```hs
let x = a in b
-- is like
(\x -> b) a
```

There have been multiple attempts in the past to bring the capabilities of lambda expressions closer to those of function declarations:

* The extension `LambdaCase` introduces a `\case` construct which allows lambda expression to have multiple clauses, however, only one pattern can be matched on. Like a regular case-expression, this can also have guards. During its implementation as well as after it, there were attempts to make it possible to match on multiple patterns. No solution was found, in part because this would make it different from regular case-expressions.

If there were an expression that had pattern matching syntax more similar to lambda expressions but which could also have guards and multiple clauses, it could be used instead of -XLambdaCase and would be able to match on multiple patterns.

* The extension `MultiWayIf` essentially introduces standalone guards, simplifying the use of guards that aren't at the outermost level of a function declaration or case-expression. Among other things, this made it easier to use guards inside of lambda expressions.

If there were an expression similar to lambda expressions that could have guards and wasn't required to have at least one parameter, it could be used instead of -XMultiWayIf. This includes all uses of -XMultiWayIf, not just those inside of lambdas (see Example section).

* During the implementation of -XLambdaCase, some suggested allowing lambda expressions to have multiple clauses. This was not implemented: The most obvious approach of turning the *lambda herald* (`\`) into a *layout herald* had the disadvantage of making some common idioms invalid.

This could be circumvented by introducing a new expression that isn't required to be backwards compatible with existing idioms.




## Refs

- Multi-way lambda expressions
https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0302-cases.rst
