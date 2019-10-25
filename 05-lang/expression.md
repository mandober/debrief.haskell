# Expressions

<!-- TOC -->

- [Evaluation](#evaluation)
- [Let and where](#let-and-where)
- [Desugaring let into lambda expressions](#desugaring-let-into-lambda-expressions)
- [Translating let and where](#translating-let-and-where)

<!-- /TOC -->

## Evaluation

Reducing an expression (redex) means evaluating the terms until the expr reaches its simplest form. Once it does, we say that it's irreducible or that it has finished evaluating. Usually, we call this a value.

Haskell uses a *non-strict evaluation*, also called *lazy evaluation* strategy which defers evaluation of terms until forced by other terms.

Values are irreducible, but applications of functions to arguments are reducible. As in the lambda calculus, application is evaluation.

Values are expressions that cannot be further reduced. 
Values are a terminal point of reduction. 


## Let and where

https://wiki.haskell.org/Let_vs._Where


- `let-in` is an expr, it can be used wherever expressions are allowed.
- In contrast, `where` **is bound to a surrounding syntactic construct**, like the pattern matching line of a function definition.


**Scope** is where a variable referred to by name is valid. Another word used with the same meaning is *visibility*, because if a variable isn't visible it's not in scope.

**Local bindings** are bindings local to particular expressions. The primary delineation from global bindings is that local bindings cannot be imported by other programs or modules.

**Global bindings** or **top level bindings** mean bindings visible to all code within a module; if exported, they are importable into other modules. in Haskell, an unconditionally visible variable throughout an entire program doesn't exist as a concept.



Use `let` and `where` to introduce names for expressions

```hs
module FunctionWithWhere where
printInc n = print plusTwo
  where plusTwo = n + 2

module FunctionWithLet where
printInc' n = 
  let plusTwo = n + 2
  in print plusTwo
```

When `let` is followed by `in` it is a **let expression**


Expressions:

- let expression:
  let <param> = <arg> in <body>
  let x = 5 in x + 3

  let <param1 = arg1>; <param2 = arg2> in <body>
  let x = 3; y = 1000 in x + y

- where expression:
  [identifier] = <body> where
       <param> = <arg>
  add x + 3 where x = 5

  [identifier] = <body> where
       <param1 = arg1>
       <param2 = arg2>
  mul = x + y where
    x = 2
    y = 5

- lambda expression:
  (\<param> -> <body>) <arg>
    (\x -> x + 3) 5

  (\<param1> -> \<param2> -> <body>) <arg1> <arg2>
    (\x -> \y -> x + y) 5 6




## Desugaring let into lambda expressions

```hs
printInc' n =
  (\plusTwo -> print plusTwo) (n + 2)
```

This doesn't work for every possible let expression as we don't have a good way to translate let expressions that use free variables recursively into the lambda calculus (technically, let and lambda are different primitives in GHC Haskell's underlying Core language).


## Translating let and where

>let x = 3; y = 1000 in x + 3
6

>let x = 5; y = 6 in x * y
30

The last one could be rewritten as:

```hs
mult1     = x * y
  where x = 5
        y = 6
```

Making the equals signs line up is a stylistic choice. As long as the
expressions are nested in that way, the equals signs do not have to line
up.
