# Introduction to recursion schemes
by Patrick Thomson, 2014
https://blog.sumtypeofway.com/posts/introduction-to-recursion-schemes.html

## Intro

In 1991, Erik Meijer, Maarten Fokkinga, and Ross Paterson published their now-classic paper "Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire". This is a seminal paper in which the authors use category theory to express a set of simple, composable combinators, called recursion schemes.

*Recursion schemes* abstract and generalize recursion and traversal (of data structures) from the data structures themselves.

Generalized traversal replaces a slew of type-specific traversal functions with a generic one. Instead of coding one function that traverses through each distinct shape of a tree data structure (and there are many variants of trees), a single traversal scheme is used as a generic abstraction over the specific cases. Moreover, most data structures are inductively defined, so functions that traverse them are necessarily recursive.

By decoupling recursion and traversal from data structures, recursion schemes let us focus on the bussiness logic, freeing us from having to worry about the mechanics of recursion and traversal. Not only for a single, specific data structure, no - but rather for almost all sensible data structures.

No matter the structure in question - lists, directory hierarchies, control flow graphs, database records - recursion schemes provide many ways to process them efficiently.

As the running example we consider a DSL of simple expressions, the different language evaluators, the AST they produce, and the difficulties encountered when trying to write functions that manipulate the AST.

*The problem of expressibility*, as formulated by Philip Wadler, deals with the support a programming language offers for extending both the data types (i.e. the types of expressions) and operations on them (i.e. behaviors), *such that the existing code is not changed*. It is known that OO languages support extending the types without touching the existing code, but have problems extending the behaviors. And the situation with FP languages is precisly reversed. This is true for a naive approach - certainly both paradigms have convoluted ways to achieve this, in which case the question of accumulated overhead comes to mind. (In any case, OOPLs suck shit, while by-math-informed and by daemons designed n' driven FPLs win, ha-ha).

>This just in: on our way toward recursion schemes, we parameterize a data type like `List a`, turning it into a functor `ListF a r`, replacing the recursion sites with the type parameter `r`. Is the realization that this breaks the holy motto - *make invalid states unrepresentable* - justified?
