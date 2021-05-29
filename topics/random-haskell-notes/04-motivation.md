## Motivation

One of the common Haskell's "katas" is to through the standard library and try to implement things yourself. And if you get stuck, the solution is readily peekable.

This was exactly what I was doing wrt continuations. While studying continuations in Haskell, I've came across the "official" code, located in the [transformers][trans] package.


implemented  got to the cont


I've been stuck trying to type check a function based on `Applicative`'s `<*>`. Giving up hope that I'll ever be able to do it myself, I begun to study the official definition. The continuation monad 

After giving up, I looked at the official solution from the `transformers` package: 





[rant ahead!] What's more depressing is that you know (having peeked at the official code) that everything needed to write the correct implementation is right there - all the ingredients in the form of identifiers are there, and you basically just need to put them in the right order. Initially, you hold onto your reasoning hat, but failing to type check for the 15th time, the hat goes out the window. Things quickly escalate and you find yourself in a frenzy, shuffling the identifiers around and spiralling away into combinatorics of madness. This was the condition induced by my failing attempts to implement the following function correctly. Now, it seems like much ado about nothing, but at the time I have completely lost faith I will ever be able to define this myself, let alone understand the decisions and the steps behind the solution.

```hs
cont_ap :: (((a -> b) -> r) -> r) -> ((a -> r) -> r) -> ((b -> r) -> r)
cont_ap h k = -- ?? how now brown cow
```

The function is named `cont_ap` since it is a standalone (out of class) version of `Applicative`'s `<*>` method. The package `transformers` defines the monad transformer variant of continuations as the `ContT` type. Similarly to other types, the base type `Cont` is obtained by applying the second type parameter of `ContT` to `Identity`.





and the related types (`Cont`) on the monad transformer variant, `ContT`.

in terms of the continuation type, `(a -> r) -> r`


For example, consider the type of continuations in its "bare" form as a function type `(a -> r) -> r`. However, continuations, like many other important types, are not often handled in this bare form; rather, they are handled in the type wrapper form as `Cont`, or more often as `ContT`.


As an example, I was trying to define its FAM instances (just for practice); that is, not really FAM methods (because you can't make this type, as is, an instance of a class), but the corresponding standalone functions that are similar to those methods as much as possible.

(..)



[trans]: https://hackage.haskell.org/package/transformers-0.5.6.2/docs/src/Control.Monad.Trans.Cont.html#line-168
