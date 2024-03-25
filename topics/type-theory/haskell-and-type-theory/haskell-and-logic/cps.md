<!-- TOC -->

- [Implication introduction and elimination](#implication-introduction-and-elimination)
- [Symbolic logic](#symbolic-logic)
- [Implication introduction](#implication-introduction)
- [](#)
- [About (de souffle)](#about-de-souffle)
- [Continuations as function types](#continuations-as-function-types)
- [Mapping a continuation](#mapping-a-continuation)
- [Running with the types](#running-with-the-types)
- [Stuck typing](#stuck-typing)
- [It's logical](#its-logical)
- [Symbolic logic](#symbolic-logic-1)

<!-- /TOC -->

## Implication introduction and elimination

In proposition (and other) logic, inference rule of implication introduction states that if you assume a `p` and work your way to a `q`, then you can conclude `p => q`. The "work your way" means you can apply inference rules, working with the proposition you already had.

Instead of comming up with some made up example for implication introduction, it's better if we return to the problem of defining Applicative's `<*>` method for the `((a -> r) -> r)` function type. Consulting the signature of `<*>` for ContT, we get the following signature:

```hs
kapp :: (((a -> b) -> r) -> r)    -- ContT r m (a -> b)
     -> ((a -> r) -> r)           -- ContT r m a
     -> ((b -> r) -> r)           -- ContT r m b
```

Note: `ContT` and particularly `Cont`, wrap the function type `((a -> r) -> r)` so the implementations should be very similar except for the un/wrapping that is necessary with newtypes.

However, since we're working with the bare continuation type here, the benefit is that we can bind the third argument to `kapp` on the LHS - we can't do that when working with a newtype wrapping since the data ctor would be in the way (this is common for many bare-type vs newtype pairs).

Another thing is, that we should definitely take advantage now of the associativity of function type ctor and simplify that signature:

```hs
kapp :: (((a -> b) -> r) -> r)    -- h
     -> ((a -> r) -> r)           -- k
     -> (b -> r)                  -- g
     -> r
kapp h k g = ...
```

And this is exactly the setup that proveked me to switch domains and try to Curry-Howard the shit out of this thing.

## Symbolic logic

Summary:
* we have at our disposal 3 function arguments
  - `h :: ((a -> b) -> r) -> r`
  - `k :: (a -> r) -> r`
  - `g :: b -> r`
* we need to produce:
  - `r`


## Implication introduction

Means, given the propositions on the LHS, you need to get the RHS. The propositions are commonly separated by a comma, but we can interpret that comma as a function ctor (->) looking at it from the Haskell's angle.

## About (de souffle)

The three functions should have signatures similar to the methods of FAM (Functor, Applicative, Monad) classes:

```hs
kmap  :: (a -> b) -> ((a -> r) -> r) -> ((b -> r) -> r))
($*)  :: (((a -> b) -> r) -> r) -> ((a -> r) -> r) -> ((b -> r) -> r)
($>=) :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
```


```hs
-- monad transformer ContT declaration
newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

-- ContT's FAM instances
instance Functor (ContT r m) where
  fmap f m = ContT $ \c -> runContT m (c . f)

instance Applicative (ContT r m) where
  pure x = ContT ($ x)
  f <*> v = ContT $ \c -> runContT f $ \g -> runContT v (c . g)

instance Monad (ContT r m) where
  m >>= k = ContT $ \c -> runContT m (\x -> runContT (k x) c)
```


## Continuations as function types

As the source code shows, continuations, in fact, have FAM instances, meaning we can start with the initial notion of continuations, as function types, and try to implement the similar methods but as standalone functions.

The basic type of continuations is a function type `(a -> r) -> r`. We'd like to start with this type and see how would the functions, similar to FAM methods, look like.

To start, we first analyse the implementation of the `Functor`'s `fmap` for `ContT` and see how to derive the signature for a similar but standalone function, we'll call `kmap`, also intended for mapping continuations.

```hs
-- ContT transformer
newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

instance Functor (ContT r m) where
  fmap :: (a -> b) -> f a -> f b
  fmap :: (a -> b) -> ContT r m a -> ContT r m b
  -- fmap f m = ContT $ \c -> runContT m (c . f)

-- Cont type alias
type Cont r a = ContT r Identity a

-- Cont newtype
newtype Cont r a = Cont { runCont :: (a -> r) -> r }

instance Functor (Cont r) where
  fmap :: (a -> b) -> f      a -> f      b
  fmap :: (a -> b) -> Cont r a -> Cont r b

-- fmap specialized to partially applied Cont, 'Cont r'
fmap @(Cont r)
    :: (a -> b)
    -> Cont r a  -- ((a -> r) -> r)
    -> Cont r b  -- ((b -> r) -> r)

-- cont-as-a-function-type has fmap-like signature
kmap :: (a -> b)
     -> ((a -> r) -> r)  -- Cont r a
     -> ((b -> r) -> r)  -- Cont r b
```

Like with the other type/newtype/transformer datatypes, continuations have the transformer only form in the source code, as `ContT r m a`. Knowing that `m` type param stands for an arbitrary monad, by removing it we arrive to the form that is assumed by the `Cont` type alias (it is also stated in the source code, although point-`a`-free), i.e. `Cont r a = ContT r Identity a`. Reimplementing it without the monad, it should look like `Cont r a = Cont ((a -> r) -> r)`. That is, it's a newtype wrapping a callback function that is easily recognized by that particular signature.

The `a` stands for a type that a plain, pre-CPS, function would have return (although that part cannot be observed in this minimal signature). Converted into CPS, that function now takes a callback, which it then applies to the would be returned value. Therefore, the callback's parameter is `a`. The callback returns some unknown (and at this time, unknownable) type, by convention labelled with an `r`. That type is also the "host" function's overall return type, here represented by the last `r`.

(why is the host function's overall output represented but not its input? probably because we can't possibly know what its input is, generically. But why is then its output repr at all? ...makes the air quotes, "generically"? It is what it is, you boring fuck. No, no no fuck you! no you, oh no, I'm diverging again...)


## Mapping a continuation

To map a continuation `(a -> r) -> r)` means to get at the `a`, which represents a function's original return type (before it was made to take a callback). We need to get at the `a` so we can apply the function `a -> b` (supplied as an arg) and get a `b`. The complication is that the said mapping will have to be done in a way that would allow us to somehow construct the return type, i.e. `(b -> r) -> r`.

```hs
-- signature #1
kmap :: (a -> b)
     -> ((a -> r) -> r)  -- Cont r a
     -> ((b -> r) -> r)  -- Cont r b

-- the signature above is actually the same as the one
-- below because (->) type ctor is right-associative
kmap :: (a -> b)          -- f
     -> ((a -> r) -> r)   -- k
     -> (b -> r)          -- g
     -> r
```

The first signature may be more appropriate to consider when dealing with continuations because it's more similar to the one we'll have when we later consider the `Cont` newtype. Then, we'll have a `Cont` data ctor in a way so we won't be able to "inline" (declare) the third argument. But now we can consider the function `b -> r` as the third argument and bind it to the parameter named `g`. The first two params, `f` and `k`, will be used is all implementations. This makes the implementation easier as we now chase only the `r` type as our goal, instead of shooting for the more complicated `(b -> r) -> r` type.

```hs
kmap :: (a -> b)          -- f
     -> ((a -> r) -> r)   -- k
     -> (b -> r)          -- g
     -> r
kmap f k g = ...
```

There! Half the job done! Now we only need to solve the RHS...

## Running with the types

It is evident from the setup that the final type is `r`. The whole thing has to be wired in a way that shits `r`'s. Aside from that final `r`, there are 3 more `r`'s, but actually all 4 `r` are the same value? No, that can't be right. Both `r`'s from `k` are the same value, and the `r` from `g` is the same value as the final `r`. 

```hs
kmap :: (a -> b) -> ((a -> r₁) -> r₁) -> (b -> r₂) -> r₂

kmap :: (a -> b)            -- f
     -> ((a -> r₁) -> r₁)   -- k
     -> (b -> r₂)           -- g
     -> r₂
kmap f k g = ...
```

It seems that the-would-be-returned-value `a`, normally passed to the callback (which then produces an `r` which is forwarded as the final `r`), needs to be mapped. But when it gets mapped with `f`, it turns to `b`. Fuck this is hard. Ok, let's just consider this as plain functions, no callbacks, no continuations. So, we have a unary but complex function `k :: ((a -> r₁) -> r₁)` whose parameter is a function `ar :: a -> r₁`. Dandy. Now, we need to somehow get at that `a` and somehow map it with `f`, which gives us something of a `b` flavour. Supposing we manage to pull this out, do we then just reassemble `k`'s remaining part. No, again. We need to dissemble `k`, get at its input parameter (let's call it `ar`) that, in its own astral plane, binds the argument function `a -> r`; then we dissemble that function `ar` to get at the input parameter `u` of the type `a`. Then `f` to the `u`, and `c` to the `k` and Bob's yer uncle. Then we put it all back together before Bob can say "blimey!". I guess the most important question is: are `r₁`'s and `r₂`'s the same frickin' value?

## Stuck typing

(when stuck, "consult" the source) Consulting the source code of `transformers` again, I was thinking about the introduction of that lambda `\c -> ...`. The place where it was introduced and the place where it was used. Also, that composition indicates an implicit variable, `x`, and since `f :: a -> b`, it is `x :: a`. But where exactly was it introduced? We need to make that shi..gnature explicit.

```hs
instance Functor (ContT r m) where
  fmap :: (a -> b) -> ContT r m a -> ContT r m b
  fmap f m = ContT $ \c -> runContT m (c . f)
  -- the dollar is redundant
  -- make composition explicit
  fmap f m = ContT \c -> runContT m \x -> c (f x)
```

Ta-da! It's all downhills from now! On a steep hill short of safety pins. While here, let's move the `runContT` noise to the LHS, it might provoke clarity. And let's restore those parenthesis, they were making things less scary.

```hs
instance Functor (ContT r m) where
  fmap :: (a -> b) -> ContT r m a -> ContT r m b
  fmap f m = ContT \c -> runContT m \x -> c (f x)
  -- unwrapping on the LHS
  fmap f (ContT m) = ContT \c -> m \a -> c (f a)
  -- parens n' dollars. nice n' impenetrable (but less scary)
  fmap f (ContT m) = ContT $ \c -> m (\a -> c (f a))
```

(by the way: is unwrapping through a pattern match the same as doing the `run_T`, effects-wise?)

However, this is not what we're after. We're still trying to finish the definition of our `kmap`. But since we're engineer-reversing it, we might as well have a go with the RHS of that last line above, only without that `ContT` and renaming ourr params to match theirs:

```hs
kmap :: (a -> b)          -- f
     -> ((a -> r) -> r)   -- k
     -> (b -> r)          -- g
     -> r
-- fmap f (ContT m) = ContT $ \c -> m (\a -> c (f a))
kmap    f        m  =         \c -> m (\a -> c (f a))
kmap    f        k             g =  k (\a -> g (f a))

-- ta-da!
kmap f k g = k (\a -> g (f a))
```

God knows why it works. Bu, but, what happend to the monad in their implementation? Aren't they wrapping `(a -> m r) -> m r` type? Aren't we merely the `(a -> r) -> r` type? Yet, yet somehow stealing their definition solved ours?! The definitions now look basically the same, save for theirs `ContT` un/wrapping, which is accounted for in having the `ContT` both on the LHS and RHS in their implementation. But what happend to the monad?


## It's logical

Having nothing left to do then undo, I've decided to switch domains: let's Curry-Howard the shit out of this thing! Maybe it'll prove worthy.

To summarize the situation so far:

we have at our disposal 3 arguments:
- `f :: a -> b`
- `k :: (a -> r) -> r`
- `g :: b -> r`

we need to produce:
- `r` as the return type

we need to come up with a clear set of steps that construct the solution, scrutinizing and justifying each little move: why it was needed, what brought it on, why it works, why the alternative doesn't, why oh why.

It would be best if the solution is obtained through a set of, practically, forced steps.

## Symbolic logic

Turning the given arguments into propositions and the return type `r` into the conclusion we're chasing, we get the following diagram:



```hs
kmap :: (a -> b) -> ((a -> r) -> r) -> (b -> r) -> r

kmap :: (a -> b)          -- f
     -> ((a -> r) -> r)   -- k
     -> (b -> r)          -- g
     -> r
kmap  f k g  =  k  (\x ->  g  (f x))
-- ^5           ^4   ^3    ^2  ^1
```

From right-to-left:
1. `(f x)`
  - x :: a
  - f :: a -> b
  - (f x)  :: b
  - f is applied to x producing some value y of type b
  - the var `x` is free here (it must've been introduced earlier)
2. `g (f x)`
  - x :: a
  - f :: a -> b
  - (f x)  :: b
  - g ::      b -> r
  - g (f x)     :: r
3. `(\x -> g (f x))`
  - (\ (x :: a) -> g (f x)) :: a -> r
  - this lambda introduces the previously mentioned free var `x`
  - the param `x` binds some future arg of type `a`
4. `k (\x -> g (f x))`
  -   (\x -> g (f x)) :: (a -> r)
  - k                 :: (a -> r) -> r
  - k returns the type `r`
5. k's return type `r` is forwarded as the overall return type `r`



```
x :: a¹
f :: a -> b
g ::      b -> r
- - - - - - - - - -
<a¹>  :: (a -> r)
k     :: (a -> r) -> r
<ret>             :: r
```




```hs
kmap f k g = k (\x -> g (f x))
--           ^1  ^2   ^3 ^4

f :: a -> b
x :: a
f x :: b
f x = y
```
