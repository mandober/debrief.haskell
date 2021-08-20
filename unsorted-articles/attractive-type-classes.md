# Attractive Type Classes

> Explanations and arguments about type classes and families; descriptions of their unusual applications

Given multi-parameter type-class declarations with functional dependencies and a set of their instances, we explain how to check if the instances conform to a functional dependency. If the check fails we give a counter-example, which is more helpful than compiler error messages. Our checker, which is a simple Prolog code, fills the real need nowadays: regrettably, GHC no longer does the functional dependency conformance check when the `UndecidableInstances` extension is on. The unconformant instances are admitted and cause problems, but at a later time and place and accompanied with even harder to understand error messages.

Functional dependencies are quite well understood, see the works by M. P. Jones and M. Sulzmann. According to Mark P. Jones, they are inspired and quite akin to functional dependencies in relational databases. In a class declaration

     class C a b | a -> b where cm :: a -> b

the functional dependency `a->b` asserts the following implication

     	(\*)  C a1 b1, C a2 b2, a1 ~ a1 ===> b1 ~ b2

where `~` denotes type equality. We view the type-class as defining a predicate on types: `C t1 t2` holds for types `t1` and `t2` if there is an instance of `C` that matches `C t1 t2`, that is, will be selected for `t1` and `t2`. On one hand, the implication (\*) lets us derive the proof of type equality, to be used for improving other types and resolving further constraints. For example, if we enter the following instance and a definition

     instance C Bool Int where ...
     
     f x = cm (not x)

GHC infers for `f` the type `Bool -> Int`. Without the functional dependency, the inferred type is polymorphic: `C Bool b => Bool -> b`. If the context in which `f` is used does not constrain `f`'s result type, the user will have to write a type annotation to help the compiler select the instance for `C`. Since our instance tells the compiler that `C Bool Int` holds, the implication (\*) says `C Bool b ===> b ~ Int`. The constraint `C Bool b` can hence be discharged from the type of `f` and `b` improved to `Int`.

A functional dependency is also a restriction on the set of instances. If we enter instances of `C` such that the implication (\*) is violated, the compiler should reject the program. Sometimes, the error message makes it hard to understand what exactly is wrong with the instances. The simple Prolog code below is intended to help, by printing a counter-example of the violation of the functional dependency. The code works with and without overlapping instances. The program is a simple model checker for implications like (\*).

Our first example has one type class and two instances

     class C a b | a -> b
     instance C Bool Int
     instance C \[a\] \[b\]

Each instance is encoded as a Prolog clause:

     c(i1,bool,int).
     c(i2,\[\_A\],\[\_B\]).

In Haskell, lower-case type identifiers are variables and upper-case ones are type constants. In Prolog, it's the other way around. The first argument of `c` identifies the instance. Underscored variables are singleton variables (if we omit the underscores, we get a warning from the interpreter).

Here is the model-checking code itself

     ?- G = (c(\_,X,Y), c(\_,X,Y1)), 
        G, Y \\== Y1,
        print('counterexample: '), instantiate(G), print(G), nl.

we search for types `X`, `Y` and `Y1` such that `c(_,X,Y)` and `c(_,X,Y1)` both hold and `Y` is different from `Y1`. If the search succeeds, we print out the found types. (Actually, we print out a grounding of the found types, which makes the print-out nicer.) The above program does find a counterexample: `c(i2, [t2], [t3]), c(i2, [t2], [t4])`. That is, one can select an instance for `C [t2] [t3]` and for `C [t2] [t4]`, where `t2`, `t3`, `t4` are some distinct types; in particular `t3` is different from `t4`. Since the implication (\*) is violated, the program must be rejected.

The Prolog code used syntactic disequality `\==`, which is defined as the negation of `==`. The ordinary Prolog equality `Term1 = Term2` holds if there exists a substitution for free variables in `Term1` and `Term2` that makes the terms identical. This equality is decided by unification. The syntactic equality `Term1 == Term2` holds if the terms are identical for _any_ substitution. If `X` and `Y` are two free variables, then `X = Y` holds (making these variables shared) but `X == Y` fails. Correspondingly, `X \== Y` succeeds (and `X \== X` fails).

Our second example has overlapping instances:

     class C a b | a -> b
     instance C Bool Int
     instance C a b

encoded as

     c(i1,bool,int).
     c(i2,\_A,\_B).

The same Prolog query produces the counter-example: `c(i1, bool, int), c(i2, bool, t1)`. That is, `C Bool Int` can be resolved (to the first instance) and `C Bool t1` with `t1` different from `Int` can also be resolved, to the second instance. The program should be rejected.

A violation of a functional dependency is usually harder to see. Here is a real example, posted on the Haskell mailing list by Wolfgang Jeltsch in Aug 2003.

     class C a b c | a b -> c where ...
     instance C a b c => C a (x,y,b) c where ...
     instance C a (a,c,b) c where ...

Its encoding in Prolog, the model checking query and the found counter-example are as follows

     c(c2,   A,tup(A,C,\_B), C).
     c(c1(I),A,tup(\_X,\_Y,B),C) :- c(I,A,B,C).
     
     ?- G = (c(\_,X,Y,Z), c(\_,X,Y,Z1)), 
        G, Z \\== Z1,
        print('counterexample: '), instantiate(G), print(G), nl.
     
     %% counterexample: c(c2,     t11, tup(t11, t12, tup(t11, t13, t14)), t12), 
     %%                 c(c1(c2), t11, tup(t11, t12, tup(t11, t13, t14)), t13)

We have described how to check if the instances of a type class with a functional dependency conform to the dependency. The method is model checking of the implication represented by the functional dependency. If the check fails, it produces a counter-example of a concrete set of instances that violate the dependency.


[Source](http://okmij.org/ftp/Haskell/TypeClass.html#peano-arithm)