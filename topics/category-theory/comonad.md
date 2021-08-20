# Comonads
https://bartoszmilewski.com/2017/01/02/comonads/

Now that we have covered monads, we can reap the benefits of duality and get comonads for free simply by reversing the arrows and working in the opposite category.

Recall that, at the most basic level, monads are about composing Kleisli arrows: `a -> m b`, where `m` is a Functor that is a Monad.

If we use the letter 'w' (as upside down m) for the comonad, we can define *co-Kleisli arrows* as morphism of the type `w a -> b`.

The analog of the fish operator for co-Kleisli arrows is `(=>=)`.

For co-Kleisli arrows to form a category we also have to have an *identity co-Kleisli arrow*, which is called `extract`. It is the dual of `return`.

We also have to impose the laws of associativity as well as left- and right-identity. Putting it all together, we can define `Comonad` as below, although, in practice, we use slightly different primitives.

```hs
class Functor w => Comonad w where
  extract :: w a -> a
  (=>=)   :: (w a -> b) -> (w b -> c) -> (w a -> c)
```

## Programming with Comonads

Comparing monads with comonads, we see that a monad provides a way of putting a value in the context, using `return`. It doesn't give you access to a value or values stored inside. Data structures that implement some Monad may provide access to their contents, but that's considered a bonus. There is no common interface for extracting values from a monad. And in a monad like `IO`, you can never extract the contents.

A comonad, on the other hand, provides the means of extracting a single value from it, using `extract`. On the other hand, it doesn't provide the means to insert values. So, we can consider a comonad as a container that always comes pre-filled with contents, and it lets you take a look inside.

A Kleisli arrow takes a value and produces some embellished result (embellished with some context), but a *co-Kleisli arrow* takes a value together with the entire context and produces a result. It's an embodiment of *contextual computation*.

- Kleisli arrows: `a -> m b`
  - a -> m         b
  - a -> []        b  |> a -> [a]
  - a -> Maybe     b  |> a -> Maybe a
  - a -> (((->) e) b) |> a -> (e -> b) |> `a -> e -> b`


- co-Kleisli arrows: `w a -> b`
  -          w a -> b
  -         [] a -> b |> [a] -> a
  - (((->) r) a) -> b |> `(r -> a) -> b`


## The Product Comonad

The `Reader` monad gives an easy way to deal with the problem of implementing computations that need access to some constant read-only environment, `env`. Such computations are represented as pure functions of the form 
`(a, env) -> b`, but we used currying to turn them into Kleisli arrows, 
`a -> env -> b`.

However, that these functions already have the form of co-Kleisli arrows, so let's massage their args into the more convenient functor form.

```hs
data Product env a = Prod env a deriving Functor
```

We can easily define the composition operator by making the same environment available to the arrows that we are composing:

```hs
(=>=) :: (Product e a -> b)
      -> (Product e b -> c)
      ->  Product e a -> c
eab =>= ebc = \ (Product e a) ->
    let b = eab (Product e a)
        c = ebc (Product e b)
    in  c
```

The implementation of `extract` simply ignores the environment:

```hs
extract :: (Comonad w) => w a -> a
extract (Prod _ a) = a
```

Not surprisingly, the `Product` Comonad can be used to perform exactly the same computations as the `Reader` Monad. In a way, the comonadic implementation of the env is more natural, it follows the spirit of "computation in a context". On the other hand, monads have the more convenient do-notation.

The connection between the Reader monad and the Product comonad goes deeper, having to do with the fact that the Reader functor is the *right adjoint* of the *product functor*. In general, though, comonads cover different notions of computation than monads.

It's easy to generalize the `Product` comonad to arbitrary product types including tuples and records.

## Dissecting the Composition

Continuing the process of dualization, we can dualize monadic `bind` and `join`. Alternatively, we can repeat the process we used with monads, where we studied the anatomy of the "fish" operator, which is a more enlightening way.

The starting point is the realization that the composition operator must produce a *co-Kleisli arrow* that takes `w a` and produces a `c`. The only way to produce a `c` is to apply the second function to an arg of the type `w b`:

```hs
(=>=) :: (w a -> b) -> (w b -> c) -> w a -> c
f =>= g = g ... 
```

How can we produce a value of type `w b` that could be fed to `g` in order to get a `c` when we have at our disposal only `w a` and `f :: w a -> b`?

Well, apply `f` to `w a` to get a `b`. Ant then ...hmm. Embellish that `b` with a `w` to somehow get a `w b`. Right? No. Then we have no way of converting the resulting `b` to `w b`. *The comonad provides no means of lifting values*.

The solution is to define the dual of `bind`, which is called `extend`; using it we can implement composition.

Hmm, well, yes, extend would make a `w b`, notjust `b`, by applying `f` to `w a`. But how?

```hs
extend :: (w a -> b) -> w a -> w b

-- using extend we can implement composition:
f =>= g = g . extend f
```

At this point, in the analogous construction for monads, we used `fmap`. The only way we could use `fmap` here would be if we had a `w (w a)`. If only we coud turn the `w a` into `w (w a)` somehow... And for that there is the dual of `join`, called `duplicate`!

```hs
duplicate :: w a -> w (w a)

fmap :: Functor w => (a -> b) -> w a -> w b
```


Just like with `Monad`, we have 3 equivalent definitions for `Comonad`:
- using *co-Kleisli arrows*
- using `extend`
- using `duplicate`


Here is the definition of `Comonad` class from `Control.Comonad` module:   
https://hackage.haskell.org/package/comonad-5.0.8/docs/Control-Comonad.html

```hs
class Functor w => Comonad w where
  extract :: w a -> a

  duplicate :: w a -> w (w a)
  duplicate = extend id

  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
```

Provided are the default implementations of `extend` in terms of `duplicate` and vice versa, so you only need to override one of them.


The intuition behind these functions is based on the idea that, in general, a comonad can be thought of as a container filled with values of type `a` (the product comonad was a special case of just one value). There is a notion of the "current" value, one that's easily accessible through `extract`.

*A co-Kleisli arrow performs some computation that is focused on the current value, but it has access to all the surrounding values*. Think of the Conway's Game Of Life: each cell contains a value (usually just 1 or 0). A comonad corresponding to the Game Of Life would be a grid of cells focused on the "current" cell.

The `duplicate` takes a comonadic container `w a` and produces a container of containers `w (w a)`. *The idea is that each of these containers is focused on a different `a` inside `w a`*. In the Game Of Life example, you would get a grid of grids, each cell of the outer grid containing an inner grid that's focused on a different cell.

The `extend` takes a co-Kleisli arrow, `w a -> b` , and a comonadic container `w a` filled with `a`'s. It applies the computation to all of these `a`'s, replacing them with `b`'s. The result is a comonadic container filled with `b`'s, i.e. `w b`.

The `extend` does this *by shifting the focus from one `a` to another `a` and applying the co-Kleisli arrow to each of them in turn*.

In the Game Of Life, the co-Kleisli arrow would calculate the new state of the current cell. To do that, it would look at its context (presumably its nearest neighbors). The default implementation of `extend` illustrates this process: first we call `duplicate` to produce all possible focus points and then we apply `f` to each of them.


(ooh! well, fuck you too)
