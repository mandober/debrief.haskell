# Hasochism - The Pleasure and Pain of Dependently Typed Haskell Programming
Sam Lindley, Conor McBride, 2013

## Abstract

Haskell's type system has outgrown its Hindley-Milner roots to the extent that it now stretches to the basics of dependently typed programming. In this paper, we collate and classify techniques for programming with dependent types in  Haskell, and contribute some new ones. In particular, through extended examples-merge-sort and rectangular tilings-we show how to exploit Haskell's constraint solver as a theorem prover, delivering code which, as Agda programmers, we envy. We explore the compromises involved in simulating variations on the theme of the dependent function space in an attempt to help programmers put dependent types to work, and to inform the evolving language design both of Haskell and of dependently typed languages more broadly.

## 1. Introduction

In the design of Standard ML, Milner and his colleagues achieved a remarkable alignment of distinctions:

```
syntactic category | terms    | types
phase distinction  | dynamic  | static
inference          | explicit | implicit
abstraction        | simple   | dependent
```

The things you write are the things you run, namely *terms* (values), for which abstraction (with an explicit `λ`) is *simply typed*, i.e. *the bound variables do not occur in the return type of the function*.

The things which you leave to be inferred, namely *polymorphic type schemes*, exist only at compile time and allow *(outermost) dependent abstraction over types*, with *implicit application at call sites* that instantiate the bound variables.
