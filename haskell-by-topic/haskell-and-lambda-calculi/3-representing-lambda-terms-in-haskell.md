# Representing lambda terms in Haskell

## Straightforward representation

The extremely simple grammar of untyped lambda calculus may be translated to Haskell almost literally:

```hs
-- grammar
Exp := x
     | λ x . Exp
     | Exp Exp

-- Haskell representation
data Exp
  = Var String
  | Abs String Exp
  | App Exp Exp
```

This, straightforward, representation is called **first-order abstract syntax** (FOAS), although that term is more justified when compared to other manners of representation, such as the *higher-order abstract syntax* (HOAS).

In this representation, which we'll refer to as straightforward or naive, the 3 lambda terms as encoded directly, each by its own data ctor, and collected under the umbrella type `Exp`.


**Variables** are represented by the data ctor `Var` that takes a string as the *name of variable*, e.g. `Var "x"`, `Var "y"`, etc.

**Abstractions** are represented by the data ctor `Abs` that takes a string, as the *name of a formal parameter*, along with a lambda expressions that represents the abstractions's body.

Abstractions are lambda functions, so they are often called functions or just lambdas. An abstraction defines an *anonymous unary function* by declaring exactly one *formal parameter* (its input), along with a lambda expression that represents its *body* (its output). Functions with more than one parameter are encoded in lambda calculus using *currying*.

Like any function, a lambda does nothing by itself - the action happens only when it gets applied to an argument. In ULC, a lambda can be applied to any argument whatsoever - the calculus is completely unrestricted; after all, it's all just functions being applied to functions. Even self-application is possible in ULC as embodied by the ω combinator, `ω := λf.ff`. The problem with this arises later, in typed lambda calculi, with the introduction of types.

Aside: it is impossible to type self-application (`λf.ff`) because the same variable `f` would have to represent both the function and its own argument. If the function is typed, very generally, as `a -> b`, then the type of the arg has to be `a`, but it is impossible to unify the types `a -> b` and `a`.



which is not ideal because 


Lambda abstractions will also use strings for their formal parameters:

    Abs "x" (Var "x")



Even though the grammar of untyped lambda calculus is very simple and may be translated to Haskell almost literally, such straightforward representation is not always optimal, particularly in face of problems that appear later, with the biggest one being the problem of capture-free substitution.

A common solution that ensures that the substitution does not inadvertantly capture variable names is using *deBruijn indices* instead of named variables. 


## Straightforward representation

Lambda terms are represented by the data type `Exp`:

```hs
-- First-order Abstract Syntax
data Exp
  = Var String
  | Abs String Exp
  | App Exp Exp

-- Higher-order Abstract Syntax (using host lang's fns for Abs)
data Lam
  = FreeVar String
  | Abs (Lam -> Lam)
  | App Lam Lam
```
