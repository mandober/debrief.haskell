# Continuations

https://stackoverflow.com/questions/3322540/how-and-why-does-the-haskell-cont-monad-work

The basic idea behind a continuation is that it represents the rest of a computation.

Consider an expression like `foo (bar x y) z`.

If we extract the subexpression `bar x y`, then it is not just the function `bar` we can apply; Rather, it is a subexpression we need to apply a function to, a function that represents the rest of the computation. In this case, the function that represents the rest of the computation is `\a -> foo a z` because if we apply it to the extracted subexpression, `bar x y` we'll get the original expression back.

```hs
e1 = foo (bar x y) z
s1 = bar x y
fs = \a -> foo a z

-- fs           s1        == e1
(\a -> foo a z) (bar x y) -- foo (bar x y) z
```

Now, it happens that this concept of "the rest of the computation" is useful, but it's awkward to work with, since it's something outside of the subexpression we're considering. To make things work better, we can turn things inside-out: extract the subexpression we're interested in, but then wrap it in a function that takes an arg that repr the rest of the computation: `\k -> k (bar x y)`.

```hs
e1 = foo (bar x y) z
s1 = bar x y
fs = \a -> foo a z

c = \k -> k (bar x y)

-- c fs == e1

(\k -> k (bar x y)) (\a -> foo a z) =
= (\a -> foo a z) (bar x y)
= foo (bar x y) z
```

This modified form gives us a lot of flexibility - not only does it extract a subexpression from its context, but it lets us manipulate that outer context within the subexpression itself. We can think of it as a sort of *suspended computation*, giving us explicit control over what happens next.

How do we generalize this? The subexpression is pretty much unchanged, so let's just replace it with a parameter to the inside-out function, giving us `\x k -> k x`, in other words, nothing more than a reversed function application.

```hs
(&) :: a -> (a -> b) -> b
(&) x f = f x
-- or
(&) x = \k -> k x
```

Now, it would be simple, albeit tedious and horribly obfuscating, to translate every piece of an expression to this form. Fortunately, there's a better way. As Haskell programmers, when we think building a computation within a background context the next thing we should think is: is this a monad? And in this case the answer is positive.

To turn this into a monad, we start with two basic building blocks:
- For a monad `m`, a value of type `m a` represents having access to a value of type `a` within the context of the monad `m`
- The core of our "suspended computations" is flipped function application, (&)

What does it mean to have access to something of type `a` within this context? It means that, for some value `x :: a`, we have already applied `(&)` to `x`, and now we have a function, which expects a function arg (which takes a type `a`) and applies that function to `x`.

Let's say we have a suspended computation holding a `Bool`. What type does this give us?

> :t (&) True
(&) True :: (Bool -> b) -> b

So for suspended computations, the type `m a` works out to `(a -> b) -> b`.

An interesting thing to note here is that a sort of "reversal" also applies to the monad's type: `Cont b a` represents a function that takes a function `a -> b` and evaluates to `b`. As a continuation represents "the future" of a computation, so the type `a` in the signature represents in some sense "the past" or the original, would-be-returned value.

So, replacing `(a -> b) -> b` with `Cont b a`, what's the monadic type for our basic building block of reverse function application? `a -> (a -> b) -> b` translates to `a -> Cont b a`, which is the signature of `return`.

From here on out, everything pretty much falls directly out from the types: there's essentially no sensible way to implement `(>>=)` besides the actual implementation. But what is it actually doing?

At this point we come back to what I said initially: the continuation monad isn't really doing much of anything. Something of type `Cont r a` is trivially equivalent to something of just type `a`, simply by supplying `id` as the arg to the suspended computation.

This might lead one to ask: if `Cont r a` is a monad but the conversion is so trivial, shouldn't `a` alone also be a monad? Of course that doesn't work as is, since there's no type constructor to define as a Monad instance, but say we add a trivial wrapper, like `data Id a = Id a`. This is indeed a monad, namely the identity monad.

What does `>>=` do for the identity monad? The type signature is `Id a -> (a -> Id b) -> Id b`, which is equivalent to `a -> (a -> b) -> b`, which is just simple function application again.

Having established that `Cont r a` is trivially equivalent to `Id a`, we can deduce that in this case as well, *`>>=` is just function application*.

Of course, `Cont r a` is a crazy inverted world where everyone has goatees, so what actually happens involves shuffling things around in confusing ways in order to chain two suspended computations together into a new suspended computation, but in essence, there isn't actually anything unusual going on! Applying functions to arguments, ho hum, another day in the life of a functional programmer.
