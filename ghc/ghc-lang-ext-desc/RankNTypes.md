# RankNTypes

https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html

- Polymorphism and the higher-rank polymorphism using GHC's `RankNTypes`
- inference becomes undecidable at rank > 2
- intuition behind HRT is they are functions which take callbacks
- rank of a function is how often control gets handed off
- rank-2 function will call a polymorphic function for you
- rank-3 function will run a callback which itself runs a callback

## Rank-2

Because callbacks are used to transfer control from a callee back to the caller context, there's a sort of a seesaw going on. For example, consider this rank-2
function `foo` below. As the caller of `foo`, we are responsible for determining the instantiation of `r`. However, the implementation of `foo` gets to choose what type `a` is - the callback you call `foo` with must work for any choice of `a`.

```hs
foo :: forall r. (forall a. a -> r) -> r

applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5
```

The implementation of `applyToFive` is what calls `f`. Because `f` is rank-1 here, `applyToFive` can instantiate it at `Int`.

By pushing up the rank of `applyToFive`, we can delay who gets to decide the type `a`. We move it from being the caller's choice to being the callee's choice.

Even higher ranks work like this: the caller of the function and the implementation toggle between who is responsible to instantiate a polymorphic type.

> A function gets +1 rank every time a forall exists on LHS of an arrow.

Because the `forall` quantifier binds more loosely than the (`->`), the type of `id` seems to be `forall a. a -> a`, but it is in fact, `forall a. (a -> a)` with the explicit parens, when it becomes easier to see that the arrow is captured by the forall.

Compare this to a rank-n type with all of its implicit parentheses inserted:

```hs
forall r. ((forall a. (a -> r)) -> r)
```

Here we can see that, indeed, the `forall a.` is to the left of a function arrow, the outermost one.

> The rank of a function is the number of arrows its deepest forall is to the left of.











## Polymorphism

**Monomorphic** types are *concrete*, fully saturated, *instantiated*, final, fully resolved, fully applied types.

```hs
-- fully defined value (a function) of a fully defined type
intId :: Int -> Int
intId x = x
-- fully defined value (a function) of a fully defined type
doubleId :: Double -> Double
doubleId x = x

-- parametrically polymorphic
id :: a -> a
id x = x
-- actually
id :: forall a. a -> a
id x = x
```

The definition of functions stays the same. This kind of polymorphism is called **parametric polymorphism** (*generics*, in other languages). Haskell only allows this if there is indeed a single definition - in other words, you cannot choose the definition of a value based on its type (for now, without `GADTs`).

It also adds safety through a property called **parametricity**. If we pretend that there are no infinite loops or exceptions (reasoning wo bottoms), then the function is actually fully determined by its type; if we see the type `a -> a`, we know that the corresponding value must be the identity function.

Parametricity decreases the semantic scope (of a function's implementation, i.e. the number of things the function could possibly do) by increasing the number of types a type parameter can be instantiated with. That is, *the bigger the number of applicable types, the less the behaviour they have in common*.

```hs
-- one type -> many behaviours
f :: Int -> Int

-- more types -> less behaviours
g :: Num a => a -> a

-- all types -> one behaviour
h :: a -> a
```

A function `Int -> Int` might do just about anything concerning integers (double it, triple it, multiply it with a constant, return the number of its digits, etc.); it may perform any conceivable (arithmetic) operation, maybe not even involving the supplied integer. There's no way to reason about it, to know what it does. However, by turning a monomorphic type into a type parameter and, possibly, fine-tuning it with class constraints, the number of applicable types is increased. As the number of concrete types that could be instantiated goes up, the behaviour they all share goes down. So, a function of type `a -> a`, where `a` can be any type at all, doesn't have a whole lotta (implementation) options. Come to think about it, it cannot do anything else but return the value it received. That value is polymorphic and it's not to be touched, it cannot be touched.

```hs
-- one type -> all behaviours
m :: (a ~ Int) => a -> a

-- few types -> most behaviours
g :: Integral a => a -> a

-- some types -> some behaviours
m :: Num a => a -> a

-- more types -> less behaviours
t :: Show a => a -> a

-- all types -> one behaviour
z :: a -> a
```

## Rank-1 polymorphism

The `id` is the identity function; but, in fact, it is a whole family of functions. We should really say that `id` is an identity function for any type `a`. For every type `a`, there is an identity function `id`, of type `a -> a`. This is how the type-checker sees it anyway, and by turning on the `RankNTypes` extension, we can be explicit about it:

```hs
{-# LANGUAGE RankNTypes #-}

id :: forall a. a -> a
id x = x
```

So, `id` is a family of infinitely many functions. We can say that it's an abstract function (as opposed to a concrete one), because its type abstracts over the type variable `a`. The common proper mathematical wording is that the type is *universally quantified* (or often just quantified) over `a`.

When we apply the identity function to a value of a concrete type, then the type variable `a` is **instantiated** to that concrete type:

```hs
-- the identity function(s)
id :: forall a. a -> a
id x = x

-- applying the function to a value instantiates the
-- type paramater with a concrete, inferred, type
id '&'

-- applying the function to a value but instantiating
-- the type paramater to a concrete, but explicitly provided, type
id (3 :: Int)

-- instantiating only the type paramater (use :type)
-- that is, specializing the polymorphic function
id @Int
-- id @Int :: Int -> Int
```

Another way to look at this is in terms of **promise and demand**. You could say that the type signature of the `id` function promises that the definition works for all types `a`. When you actually apply the identity function you demand a certain type.

## Rank-2 and higher polymorphism

The `RankNTypes` extension allows us to be more explicit about the forall part. This alone is just a syntactic change and adds no new expressivity. However, we can use this new syntax within a type alias:

```hs
{-# LANGUAGE RankNTypes #-}

type IdFunc = forall a. a -> a
```

The type (sig) fully determines the function, so any value of type `IdFunc` must be the identity function. But `IdFunc` is just a regular type alias, which means we can use it in type signatures. For example we could have written:

```hs
id :: IdFunc
id x = x
```

Now the type variable is gone entirely. A much more interesting way to use `IdFunc` is as the domain of a function:

```hs
someInt :: IdFunc -> Integer
```

Since any value of type `IdFunc` must be the identity function, the `someInt` is a function that expects the identity function as its argument and returns an integer. Let's give it some (arbitrary) definition:

```hs
someInt id' = id' 3
```

The `someInt` has received a function `id'` about which it knows that it is the fully fledged polymorphic identity function. So it can instantiate its type variable as it likes, and so it does.

The `someInt` isn't even polymorphic! Rather it expects a polymorphic function as its arg. This becomes clear when we expand the type alias:

```hs
someInt :: (forall a. a -> a) -> Integer
```

It is completely monomorphic. Its type is not quantified. When we apply a polymorphic function like `id` we can *choose which types to instantiate as*.

On the other hand, `someInt` function does not give us such a choice. In fact it requires us to pass a sufficiently polymorphic function to it, such that *it can make that choice*. When we apply it, we need to give it choice.

It is like this: the identity function promises to work for all `a`. When you apply it, you demand `a` to be a certain type. However, the `someInt` function makes no such promise. *It wants us to pass it a function that makes a promise, such that it gets to demand something from it. We don't get to demand anything*.

And that is **rank-2 polymorphism**. You can have arbitrary-rank polymorphism by burying the quantifier in more levels of necessary parentheses. Example:

```hs
type IdFunc = forall a. a -> a

type SomeInt = IdFunc -> Integer

someOtherInt :: SomeInt -> Integer
someOtherInt someInt' = someInt' id + someInt' id
```

The following function is **rank-3 polymorphic**, because the quantifier is in the third level of necessary parentheses:

```hs
someOtherInt :: ((forall a. a -> a) -> Integer) -> Integer
```

## Case study: random numbers

(...)


## Scott encoding

The regular list data type is defined as a sum type, but here's a custom version of it:

```hs
data List a = Nil | Cons a (List a)
```

There are two ways to deconstruct this type in a principled fashion.
* The first one is called pattern-matching, which means removing one layer of constructors. You could use the usual case construct to do this, but it is syntactically heavy and does not compose well.
* The other way is with a function, like `uncons`, that accepts two *continuations* and a list. The continuations determine what we reduce the list into depending on which constructor is found.

```hs
uncons :: (a -> List a -> r) -> r -> List a -> r
uncons co ni (Cons x xs) = co x xs
uncons co ni Nil         = ni

-- Nil    ::                r     |          ys ≡ []
-- Cons   :: a -> List a -> r     |   x:xs = ys
-- list   ::      List a          |     xs
-- ret    ::                r     |          ys

isNil :: List a -> Bool
isNil = uncons
        (\_ _ -> False)  -- cont bound to co (called in cons case)
        True             -- cont bound to ni (called in nil case)
                         -- (the list is yet to be received)
```


* If the list is a `cons`, it's non-empty, we reduce it to `False`.
* If the list is a `nil`,  it's empty,  so we reduce it to `True`.

* But `uncons` can do case analysis thanks to the `List a` arg for it can be a `Nil` or `Cons _ _`. 

What if there's no such ctor-containing arg (e.g. Maybe, Pair, Either)? Then the shape of data repr itself must dispatch (select) the appropriate continuation, like Church a Boolean does; i.e. it selects the first cont if True, second cont if False.

* The `isNil` calls `uncons` with both continuations, one for each list case. The `uncons` then selects the appropriate one based on patter matching its third arg, `List a`.

* This implies that `uncons` actually repr a list, isn't it? hmm, well, not particularly, it seems more appropriate to say that it just dispatches the two conts according to the shape of the list it receives eventually. Other list related functions use it for that service, like `isNil` does.


name   | el | list | ret | aka              | val
-------|----|------|-----|------------------|-------
nil    |    |      | r   | ::             r |   []
cons   | a  | [a]  | r   | -> a -> [a] -> r | x:xs
(list) |    |  r   |     | ->       r       |   xs
(ret)  |    |      | r   | ->             r |   xs



The following is a more interesting example, but it's really just functional style pattern-matching:

```hs
listMap :: (a -> b) -> List a -> List b
listMap f = uncons (\x xs -> Cons (f x) (listMap f xs))
            Nil
```
