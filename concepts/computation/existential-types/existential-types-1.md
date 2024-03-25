# Existential types

An *existentially quantified type* is one that is hidden to the consumer (user), but directly chosen by the producer. The producer chooses the type, and the consumer is given a polymorphic type (for it can be any type at all). The consumer doesn't know what type they are working with. So what can the consumer do with an existential type? Well, nothing, of course! All they can do is apply the `id` to it (or some other, as generally a polymorphic a function) and that's about it. If the producer attached the `Show` constraint, then you can also show it, yey! If the producer attached the `Eq` constraint than you can compare it. So... you can use the operations that the producer supplied with the existential type to manipulate it. The producer can give you a nice package made of the existential type and the API (operations on it). Then you can make use of the type, while he keeps its implementation hidden. Encapsulation!

This is in direct contrast to the *universally quantified type* where the type is directly chosen by the consumer. The consumer chooses the type, and the producer has to handle any possible type that the consumer (user) asks for. For example, the function `read :: forall a. Read a => String -> a` is universally quantified over `a`: the caller of `read` gets to pick what type `a` is instantiated at. The burden is on the implementor of `read` to handle whatever type was picked. Meh, here, really, `a` can be any type... as long as it's a member of the `Read` class. And having a `Read` instance comes with a packet of methods of the `Read` class, which means no matter what type `a` gets instantiated as, parsing a string to that type was already implemented.

---
https://mail.haskell.org/pipermail/haskell-cafe/2007-July/028233.html

Consider the `ST` monad, which lets you use update-in-place, but is
escapable (unlike IO). ST actions have the form: `ST s α`.

Meaning that they return a value of type `α`, and execute in "thread" `s`.
All reference types are tagged with the thread, so that actions can only
affect references in their own "thread".

Now, the type of the function used to escape ST is:

```hs
runST :: ∀ α. (∀ s. ST s α) → α
```

The action you pass must be universal in `s`, so inside your action you don't know what thread, thus you cannot access any other threads, thus `runST` is pure.

This is very useful, since it allows you to implement externally pure things like in-place quicksort, and present them as pure functions 

```hs
∀ e. Ord e ⇒ Array e → Array e
```

without using any unsafe functions.

But that type of runST is illegal in Haskell-98, because it needs a universal quantifier *inside* the function-arrow! That is, the type has rank 2, but  Haskell98 types must have rank at most 1.
