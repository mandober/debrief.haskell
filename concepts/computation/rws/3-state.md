# State

The `State` monad deals with the scenario when we need a global mutable state that needs to be accessible and even mutated by the various functions in the module. Such state problem may be solved with pure functions just by adding the state as an extra parameter to all functions that need to access it.


>What if we just treated our "variable" as a normal parameter? We'd need each function we write to be able to access it, so our functions will get an extra state parameter. And they'll also need some way to update it, which, since our functions are pure, it must mean that they return the updated state as an extra value as well.


A generic function `f : a -> b` is modified so it takes a pair `(s, a)` and returns a pair `(s, b)`, where `s` is the type of the state value. The state `s` may be a simple scalar value, like an `Int`, or a complex data structure.

```hs
f  :: a -> b
f' :: (a, s) -> (b, s)
```

The original function `f : a -> b` becomes `f' : (a, s) -> (s, b)`. The modified function is still unary, however, since the components of a pair may be swapped, and the input pair curryied, it is usually converted into a binary function `f' : a -> s -> (s, b)`.

```hs
f :: a -> b
f' :: (s, a) -> (s, b)
f' :: (a, s) -> (s, b)
f' :: a -> s -> (s, b)
```

The function is parameterized by its normal input value `a` and the state `s`,and returns a pair made out of its usual output `b` along with a state `s`.

The input param, here only one, `a`, is not specific to statefulness, and functions may have more than one input param. However many inputs a function has - all those params will come before the state param - so we can extract the "core signature' from this as `s -> (s, a)`. We need such a core type in order to make a new data type out of it; then, we can define that type to be an instance of the classes of interest, primarily of the `Monad` class. That, in turn, will provide us with the functionality we want: to have the state be threaded through all the functions that need it - but automatically, so we don't need to manage it ourselves explicitly.

When we augment a function with the extra param that carries state we loose the ability to compose such functions. By extracting the core signature for statefull functions and promoting it into a new data type, and then defining it to be an instance of the various type class, especially `Monad`, will help us *restore composability*.











A *Kleisli arrow*, related to monads in general, is an embellished function, generically typed as `a -> m b`; return value of a regular function `a -> b` is "embellished" with an additional context `m`.

```hs
regular :: a -> b
kleisli :: a -> m b

-- if monad m is a pair functor, m = (,) s, this becomes:
state :: a -> (,) s b
state :: a -> (s, b)
```

This is how a function is embellished and extended to return a pair which also carries the state value.

However, we take the function `s -> (s, a)` to make a new type out of. Presumably it is the cut-off version of the whole function `a -> s -> (s, a)`.




## Refs

https://williamyaoh.com/posts/2020-07-12-deriving-state-monad.html
