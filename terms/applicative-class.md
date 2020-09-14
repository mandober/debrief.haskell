# Applicative class

Functors are a useful concept for values that can be mapped over. Taking this concept further we introduce *applicatives* i.e. *applicative functors* which allow us to view values of certain types as values with contexts and use normal functions on those values while preserving the meaning of those contexts.

Applicative is a Functor with application, providing operations to:
- to embed pure expressions with `pure`
- to sequence computations and combine their results with `<*>` and `liftA2`


## Applicative class definition

```hs
class (Functor f) => Applicative f where
    -- Lift a value into Applicative context
    pure :: a -> f a

    -- Sequential application
    (<*>) :: f (a -> b) -> f a -> f b
    (<*>) = liftA2 id

    -- Lift a binary function to actions.
    liftA2 :: (a -> b -> c) -> f a -> f b -> f c
    liftA2 f x = (<*>) (fmap f x)

    -- Sequence actions, discarding the value of the first argument:
    (*>) :: f a -> f b -> f b
    a1 *> a2 = (id <$ a1) <*> a2

    -- Sequence actions, discarding the value of the second argument:
    (<*) :: f a -> f b -> f a
    (<*) = liftA2 const
```

`pure`   
The applicative's `pure` is equal to the monadic method `return` - *it takes a value and puts it in a minimal default context that still holds that value*.

`<*>`    
sometimes called "apply", is for sequential application, for sequencing actions. A few functors support an implementation of (<*>) that is more efficient than the default one: `(<*>) = liftA2 id`

`liftA2`    
liftA2 lifts a binary function to actions. Some functors support an impl of this function that is more efficient than its default definition. Particularly, if fmap is an expensive operation, it is likely better to use liftA2 than to fmap and then use <*>.

`*>`    
is also for sequencing actions, but this one *discards the first argument*.

`<*`    
is also for sequencing actions, but this one *discards the second argument*.


## Applicative laws

A minimal complete definition must include implementations of:
- `pure`
- and either `<*>` or `liftA2`

If it defines both, they must behave the same as their default definitions:
- `(<*>) ≡ liftA2 id`
- `liftA2 f x y ≡ f <$> x <*> y`

Further, any definition must satisfy the following axioms:
- IDENTITY:                    pure id <*> v ≡ v
- COMPOSITION:    pure (.) <*> u <*> v <*> w ≡ u <*> (v <*> w)
- HOMOMORPHISM:            pure f <*> pure x ≡ pure (f x)
- INTERCHANGE:                  u <*> pure y ≡ pure ($ y) <*> u

The other methods have the following default definitions, which may be overridden with equivalent specialized implementations:
- `u *> v = (id <$ u) <*> v`
- `u <* v = liftA2 const u v`

As a consequence of these laws, the Functor instance for `f` will satisfy:
- `fmap f x = pure f <*> x`

It may be useful to note that supposing:
- `forall x y. p (q x y) = f x . g y`
it follows from the above that:
- `liftA2 p (liftA2 q u v) = liftA2 f u . liftA2 g v`

If `f` is also a Monad, it should satisfy:
- `pure = return`
- `(<*>) = ap`
- `(*>) = (>>)`
which implies that `pure` and `<*>` satisfy the *applicative functor laws*.
