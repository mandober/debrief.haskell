# Defunctionalizing dependent type families in Haskell

> A blog about functional programming

Posted on January 16, 2021

Extensions and import used in this Literate Haskell post.

    {-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, RankNTypes,
                 GADTs, TypeOperators, UndecidableInstances #-}
    import Data.Kind (Type)
    import Data.Proxy

Type families in Haskell offer a flavor of dependent types: a function `g` or a type family `G` may have a result whose type `F x` depends on the argument `x`:

    type family F (x :: Type) :: Type
    
    g :: forall x. Proxy x -> F x  -- Proxy to avoid ambiguity
    g = undefined  -- dummy
    
    type family G (x :: Type) :: F x

But it is not quite clear how well features of other “truly” dependently typed languages translate to Haskell. The challenge we’ll face in this post is to do type-level pattern-matching on GADTs indexed by type families.

Problem
-------

Sorry if that was a bit of a mouthful. Let me illustrate the problem with a minimal non-working example. You run right into this issue when you try to [defunctionalize](https://blog.poisson.chat/posts/2018-08-06-one-type-family.html) a dependent function, such as `G`, which is useful to reimplement “at the type level” libraries that use type families, such as _recursion-schemes_.

First encode `G` as an expression, a symbol `SG`, denoting a value of type `F x`:

    type Exp a = a -> Type
    data SG (x :: Type) :: Exp (F x)

Declare an evaluation function, mapping expressions to values:

    type family Eval (e :: Exp a) :: a

Define that function on `SG`:

    type instance Eval (SG x) = G x

And GHC complains with the following error message (on GHC 8.10.2):

    error:
        • Illegal type synonym family application ‘F x’ in instance:
            Eval @(F x) (SG x)
        • In the type instance declaration for ‘Eval’

The function `Eval :: forall a. Exp a -> a` has two arguments, the type `a`, which is implicit, and the expression `e` of type `Exp a`. In the clause for `Eval (SG x)`, that type argument `a` must be `F x`. Problem: it contains a type family `F`. To put it simply, the arguments in each `type instance` must be “patterns”, made of constructors and variables only, and `F x` is not a pattern.

As a minor remark, it is necessary for the constructor `SG` to involve a type family in its result. We would not run into this problem with simpler GADTs where result types contain only constructors.

    -- Example of a "simpler" GADT
    data MiniExp a where
      Or :: Bool -> Bool -> MiniExp Bool
      Add :: Int -> Int -> MiniExp Int

How it’s solved elsewhere
-------------------------

It’s a problem specific to this usage of type families. For comparison, a similar value-level encoding does compile, where `eval` is a function on a GADT:

    data Exp1 (a :: Type) where
      SG1 :: forall x. Proxy x -> Exp1 (F x)
      -- Proxy is necessary to avoid ambiguity.
    
    eval :: Exp1 a -> a
    eval (SG1 x) = g x

You can also try to promote that example as a type family, only to run into the same error as earlier. The only difference is that `SG1` is a constructor of an actual GADT, whereas `SG` is a type contructor, using [`Type` as a pseudo-GADT](https://blog.poisson.chat/posts/2018-07-09-type-gadt.html).

    type family Eval1 (e :: Exp1 a) :: a
    type instance Eval1 (SG1 (_ :: Proxy x)) = G x

    error:
        • Illegal type synonym family application ‘F x’ in instance:
            Eval1 @(F x) ('SG1 @x _1)
        • In the type instance declaration for ‘Eval1’

Type families in Haskell may have implicit parameters, but they behave like regular parameters. To evaluate an applied type family, we look for a clause with matching patterns; the “matching” is done left-to-right, and it’s not possible to match against an arbitrary function application `F x`. In contrast, in functions, type parameters are implicit, and also _irrelevant_. To evaluate an applied function, we jump straight to look at its non-type arguments, so it’s fine if some clauses instantiate type arguments with type families.

In Agda, an actual dependently-typed language, [_dot patterns_](https://agda.readthedocs.io/en/latest/language/function-definitions.html#dot-patterns) generalize that idea: they indicate parameters (not only type parameters) whose values are determined by pattern-matching on later parameters.

#### GADTs = ADTs + type equality

A different way to understand this is that the constructors of GADTs hold _type equalities_ that constrain preceding type arguments. For example, the `SG1` constructor above really has the following type:

    SG1 :: forall x y. (F x ~ y) => Proxy x -> Exp1 y

where the result type is the GADT `Eval1` applied to a type variable, and the equality `F x ~ y` turns into a field of the constructor containing that equality proof.

So those are other systems where our example does work, and type families are just weird for historical reasons. We can hope that Dependent Haskell will make them less weird.

Solution
--------

In today’s Almost-Dependent Haskell, the above desugaring of GADTs suggests a workaround: type equality allows us to comply with the restriction that the left-hand side of a type family must consist of patterns.

Although there are no constraints in the promoted world to translate `(~)`, type equality can be encoded as a type:

    data a :=: b where
      Refl :: a :=: a

A type equality `e :: a :=: b` gives us a _coercion_, a function `Rewrite e :: a -> b`. There is one case: if `e` is the constructor `Refl :: a :=: a`, then the coercion is the identity function:

    type family Rewrite (e :: a :=: b) (x :: a) :: b
    type instance Rewrite Refl x = x

Now we can define the defunctionalization symbol for `G`, using an equality to hide the actual result type behind a variable `y`:

    data SG2_ (x :: Type) (e :: F x :=: y) :: Exp y
    -- SG2_ :: forall y. forall x -> F x :=: y -> Exp y

We export a wrapper supplying the `Refl` proof, to expose the same type as the original `SG` above:

    type SG2 x = SG2_ x Refl
    -- SG2 :: forall x -> Exp (F x)

We can now define `Eval` on `SG2_` (and thus `SG2`) similarly to the function `eval` on `SG1`, with the main difference being that the coercion is applied explicitly:

    type instance Eval (SG2_ x e) = Rewrite e (G x)

To summarize, type families have limitations which get in the way of pattern-matching on GADTs, and we can overcome them by making type equalities explicit.

Acknowledgements
----------------

Thanks to [Denis Stoyanov](https://twitter.com/xgrommx) for discussing this issue with me.


[Source](https://blog.poisson.chat/posts/2021-01-16-dependent-fcfs.html)