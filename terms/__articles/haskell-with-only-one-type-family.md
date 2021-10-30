# Haskell with only one type family

> A blog about functional programming

Posted on August 6, 2018

In this post, we will implement open type families with a single actual type family.[1](#fn1) Surprisingly, this endeavor leads to increased expressivity: type families become first-class.

Check out my very creatively named package: [_first-class-families_](https://hackage.haskell.org/package/first-class-families/).

The result is a form of defunctionalization (I call “eval-style”), to be compared later with a more common one (“apply-style”). I will not assume knowledge of it in this post, but you can read more about it over there: [Defunctionalization for the win](https://typesandkinds.wordpress.com/2013/04/01/defunctionalization-for-the-win/).

Extensions and imports for this Literate Haskell file

    {-# LANGUAGE
        DataKinds,
        PolyKinds,
        TypeFamilies,
        TypeInType,
        TypeOperators,
        UndecidableInstances #-}
    
    module OneTypeFamily where
    
    import Data.Kind (Type)
    import GHC.TypeNats (Nat, type (+))
    import GHC.TypeLits (TypeError, ErrorMessage(..))

Summary
-------

Type families are defined by pattern-matching. We are going to replace all type families with a single one that matches on an encoding of a type family applied to its arguments.

For example, the following family, a type-level `fst`:

    type family   Fst' (xy :: (a, b)) :: a
    type instance Fst' '(x, y) = x

will be replaced with this `data` type `Fst` where the type constructor represents the `Fst'` type family, together with a `type instance` clause for a single general type family, called `Eval`:

    --   fst ::     (a, b)  ->     a
    data Fst (xy :: (a, b)) :: Exp a
    type instance Eval (Fst '(x, y)) = x

The `Eval` type family
----------------------

As its name indicates, the type family `Eval` _evaluates_ applied type families. More generally, we will see that `Eval` can also work with complex _expressions_, hence the name of the kind `Exp`.

    type family Eval (e :: Exp a) :: a

The kind `Exp a` of an expression is indexed by the kind `a` of the result of its evaluation. The kind of expressions `Exp` is actually defined as:

Instead of declaring a type family, we introduce a new expression constructor by declaring a `data` type, such as `Fst` above. The inhabitants of `Fst` don’t matter (in practice we leave those types empty). We only use `data` as a way to introduce new symbols in the type-level language that we can pattern-match on; we sometimes call them “defunctionalized symbols”: symbols are not actually functions but they stand for them. Essentially, we use `Exp` as an “[extensible GADT](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/2018-07-09-type-gadt.html)”, whose constructors are type constructors.

Here are two more examples of expressions encoding some common functions:

    --   snd :: (a, b) ->     b
    data Snd :: (a, b) -> Exp b
    type instance Eval (Snd '(x, y)) = y
    
    --   fromMaybe :: a -> Maybe a ->     a
    data FromMaybe :: a -> Maybe a -> Exp a
    type instance Eval (FromMaybe x0 'Nothing ) = x0
    type instance Eval (FromMaybe x0 ('Just x)) = x

### Exercise

Translate this type family using `Eval`:

    type family   Length' (xs :: [a]) :: Nat
    type instance Length' '[] = 0
    type instance Length' (x ': xs) = 1 + Length' xs

Expected result in `ghci`:

    λ> :kind! Eval (Length '[1,2,3])
    (...)
    3

That is all there is to it, to have type families with only one type family.

From now on, only type family signatures (actually, the `data` type) will be given, leaving the `Eval` instances as exercises for the reader.

First-class type families
-------------------------

Encoding type families with type constructors allows them to be passed around without applying them, which is not possible in their original form. Hence we will call this new thing “first-class type families”, as opposed to “regular type families”. And indeed, we can define higher-order type families, such as `Map`, where the first parameter is a unary type family `a -> Exp b`:

    data Map :: (a -> Exp b) -> [a] -> Exp [b]

Expected result in `ghci`:

    λ> :kind! Eval (Map Snd '[ '(1, 2), '(3, 4) ])
    (...)
    '[2, 4]

Of course that is meant to correspond to `map`:

    map :: (a -> b) -> [a] -> [b]

But those `Exp` constructors are in quite familiar places. Doesn’t it look like…

    traverse :: Applicative m => (a -> m b) -> [a] -> m [b]

Did you notice `Exp` is a monad?

    data (>>=) :: Exp a -> (a -> Exp b) -> Exp b
    data Pure :: a -> Exp a

Composition of type families is Kleisli composition:

    data (>=>) :: (a -> Exp b) -> (b -> Exp c) -> a -> Exp c

The monad laws should hold when we observe the result with `Eval`:

    Eval (m >>= Pure)      = Eval m
    Eval (Pure x >>= k)    = Eval (k x)
    Eval ((m >>= h) >>= k) = Eval (m >>= (h >=> k))

We can play with a few more `Functor`/`Applicative`/`Monad` combinators.

    data (<$>) :: (a -> b) -> Exp a -> Exp b
    data (<*>) :: Exp (a -> b) -> Exp a -> Exp b
    data Join  :: Exp (Exp a) -> Exp a

### Lazy type-level functional programming

Thus, first-class type families bring a certain amount of functional programming to the type level. Moreover, with the control we have over `Eval`\-uation of `Exp`\-ressions, we can emulate some _lazy_ functional programming patterns.

This is a big deal, because type families are strict[2](#fn2). For example, consider this `If` type family:[3](#fn3)

    type family   If (b :: Bool) (x :: k) (y :: k) :: k
    type instance If 'True  x _ = x
    type instance If 'False _ y = y

What happens if we compile the following snippet?

    type X =
      If 'True
        ()
        (TypeError ('Text "This shouldn't happen"))

The condition is `True`, so it seems the result should be the first branch `()`, but the error in the second branch is still evaluated:

    error:
        • This shouldn't happen
        • In the type synonym declaration for ‘X’
        |
    218 | type X =
        | ^^^^^^^^...

The old solution with regular type families is to specialize `If` manually, but this is certainly cumbersome to do for every conditional:

    type family   IfThis (b :: Bool) :: Type
    type instance IfThis 'True  = ()
    type instance IfThis 'False = TypeError ('Text "This shouldn't happen")
    
    type Y = IfThis 'True

Using first-class type families, we don’t need to change `If` at all.[4](#fn4) Instead, we first make an `Exp` to delay the evaluation of a `TypeError`.

    data TypeError' :: ErrorMessage -> Exp a

Now, the branches of `If` are expressions, and we will unpack only the one we need with `Eval`.

    type Z = Eval
      (If 'True
        (Pure ())
        (TypeError' ('Text "This shouldn't happen")))

Compilation succeeds, evaluating `Z` to `()` (if you have `Pure` defined).

At this point, you may know enough to really use first-class type families.

Apply and Eval
--------------

As mentioned at the beginning, this is a kind of _defunctionalization_, closely related to another variant that you may have come across elsewhere, such as in [Defunctionalization for the win](https://typesandkinds.wordpress.com/2013/04/01/defunctionalization-for-the-win/) or in [Higher-kinded data](https://reasonablypolymorphic.com/blog/higher-kinded-data/). Here is a brief explanation.

The original motivation was to have first-class functions, so `Exp` is replaced with this kind `(~>)` of “defunctionalized function symbols”:

    type a ~> b = a -> b -> Type

(Actually, looking at _singletons_, `(~>)` is defined a bit differently for historical reasons, I believe, but that is a minor detail.)

Now the one type family is descriptively called `Apply`:

    type family Apply (f :: a ~> b) (x :: a) :: b
    
    -- Example
    data Fst_ :: (a, b) ~> a
    type instance Apply Fst_ '(x, _) = x

`Eval` separates “application” (via type constructor application) and “evaluation”, whereas they happen simultaneously in `Apply`. In that sense, `Eval` seems like a more elementary presentation of defunctionalization. Nonetheless, the two styles are equivalent in expressivity.

### Equivalence between Apply and Eval

We can translate one style of defunctionalization into the other and vice versa.

First, we can define `Eval_` in terms of `Apply`, representing `Exp a` expressions with apply-style constant functions `() ~> a`.

    type Exp_ a = () ~> a
    
    type Eval_ (e :: Exp_ a) = Apply e '()

And second, we can define `Apply_` using `Eval`, representing `a ~> b` with Kleisli arrows `a -> Exp b`.

    type Apply_ (f :: a -> Exp b) (x :: a) = Eval (f x)

Note that we literally have `(a ~> b) = (a -> Exp b)` here, so we can directly reuse the same defunctionalized symbols in this second translation.

Conclusion
----------

Converting regular type families to first-class type families is so straightforward, I’m surprised to not have seen any discussion about this “eval-style” defunctionalization before.

Feel free to share any idea or issue you encounter with [_first-class-families_](https://hackage.haskell.org/package/first-class-families/)!

* * *

1.  An idea reminiscent of [Haskell with only one typeclass](http://okmij.org/ftp/Haskell/TypeClass.html#Haskell1).[↩︎](#fnref1)
    
2.  The truth is more complicated (and I don’t know the details well), but that will be a good enough approximation for the purposes of this post,[↩︎](#fnref2)
    
3.  I also talk about it in [another post](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/2018-06-06-hlists-dependent-haskell.html) on type-level programming.[↩︎](#fnref3)
    
4.  I find this `If` quite nice. I’m not sure making it a first-class type family is worth the extra steps. “One type family” is only a nice title.[↩︎](#fnref4)


[Source](https://blog.poisson.chat/posts/2018-08-06-one-type-family.html)