# Haskell single homomorphic restriction leads to type ambiguity: Ambiguous type variable ‘’ arising from a use of ‘’ prevents the constraint ‘’ f

> Haskell single homomorphic restriction leads to type ambiguity: Ambiguous type variable ‘’ arising from a use of ‘’ prevents the constraint ‘’ f, Programmer Sought, the best programmer technical posts sharing site.

> Provide an example of type ambiguity caused by single homomorphism restriction.

Problem Description
-------------------

In the following code, the type of the function cannot be automatically deduced when the single homomorphism limit is turned on:

    my_liftA2 :: Applicative f => (c -> a -> b) -> f c -> f a -> f b
    my_liftA2 f a b = f <$> a <*> b
    
    -- my_ap :: Applicative f => f (a -> b) -> f a -> f b
    my_ap = my_liftA2 id
    

The error is as follows:

    Prelude> :l main
    [1 of 1] Compiling Main             ( main.hs, interpreted )
    
    main.hs:5:9: error:
        • Ambiguous type variable ‘f0’ arising from a use of ‘my_liftA2’
          prevents the constraint ‘(Applicative f0)’ from being solved.
          Relevant bindings include
            my_ap :: f0 (a -> b) -> f0 a -> f0 b (bound at main.hs:5:1)
          Probable fix: use a type annotation to specify what ‘f0’ should be.
          These potential instances exist:
            instance Applicative (Either e) -- Defined in ‘Data.Either’
            instance Applicative IO -- Defined in ‘GHC.Base’
            instance Applicative Maybe -- Defined in ‘GHC.Base’
            ...plus three others
            (use -fprint-potential-instances to see them all)
        • In the expression: my_liftA2 id
          In an equation for ‘my_ap’: my_ap = my_liftA2 id
    Failed, modules loaded: none.
    

Solution
--------

Solution 1: Explicitly specify`my_ap`Type signature, avoid type inference, only type check

Solution 2: Turn off single homomorphism restriction:`{-# LANGUAGE NoMonomorphismRestriction #-}`or`Prelude> :set -XNoMonomorphismRestriction`or`ghc ... -XNoMonomorphismRestriction`

Cause of error
--------------

What is a single homomorphic restriction? The rules for unimorphism are as follows:

> The usual Hindley-Milner restriction on polymorphism is that only type variables that do not occur free in the environment may be generalized. In addition, **the constrained type variables of a restricted declaration group may not be generalized in the generalization step for that group.** (Recall that a type variable is constrained if it must belong to some type class; see Section 4.5.2 .)
> 
> Any monomorphic type variables that remain when type inference for an entire module is complete, are considered ambiguous, and are resolved to particular types using the defaulting rules (Section 4.3.4 ).

The advantage of this is to avoid redundant calculations in some places, but the disadvantage is simple`default rule`The resolve at some point sacrifices the fine nature of the original type that can be type inference.

Then let us consider the original question:

> A pattern binding is a declaration of the form: `<pattern> = expr`

and so`my_ap`It is a pattern binding.

> a given declaration group is _unrestricted_ if and only if:
> 
> 1.  every variable in the group is bound by a function binding (e.g. `f x = x`) or a simple pattern binding (e.g. `plus = (+)`; Section 4.4.3.2 ), and
> 2.  an explicit type signature is given for every variable in the group that is bound by simple pattern binding. (e.g. `plus :: Num a => a -> a -> a; plus = (+)`).

and so`my_ap`It is a restricted declaration group, subject to single homomorphism restriction.

The reason for the original problem is the second thing done by the single homomorphic restriction:

> Any monomorphic type variables that remain when type inference for an entire module is complete, are considered ambiguous, and are resolved to particular types using the defaulting rules (Section 4.3.4 ).

That is, because`my_ap`Derived type`my_ap :: f0 (a -> b) -> f0 a -> f0 b`Include type variables`f0`, So the system can’t tolerate this happening and will follow`default rule`Trying to give`f0`Give a real type, but this thing could not be completed, so an error was reported.

So what is`default rule`? In fact, try one by one in a list. But for Applicative type classes, we do not allow this list, so the system fails to do type inference, it will report an error, and we recommend that we clearly`my_ap`Type, specify`f0`Which Applicative instance is it, so you don’t need to`my_ap`Do type inference to avoid this error. And we choose`my_ap`Add the type signature so`my_ap`As a pattern binding, it will not be affected by a single homomorphism restriction.

Finally, if the above operation is entered interactively on ghci, it will not appear`Ambiguous type variable`This error. This is how StackOverflow explains this:

> This is due to how ghci handles (and must handle) the interactive definitions. Basically every statement entered in ghci must be completely type checked before the following is considered; in other words it’s as if every statement was in a separate module.

This article is just a rough summary of the answers on StackOverflow based on this question. If you want to learn more, you still have to look at the original question on SO.[what-is-the-monomorphism-restriction](https://stackoverflow.com/questions/32496864/what-is-the-monomorphism-restriction)


[Source](https://www.programmersought.com/article/51846997377/)