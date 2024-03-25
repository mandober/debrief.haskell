# ST monad

The `ST` monad allows having a local mutable state.

The monadic `ST` type provides support for *strict state threads*.

The `ST` (*state transformer*) monad is used to have real mutable state in Haskell.

The `State` monad is pure in that it automatically threads the state through pure functions, but the `ST` monad, and the value it holds, are actually "dirty", being backed by mutable memory. This makes it a bona fide side effect, but because those side effects are not observable, it is allowed.

Unlike the `IO` monad, the ST monad can be escaped. Sometimes this is called *thawing* and *freezing* - the process of going into and out of the ST monad.

The type signature of ST uses existentially qualified type param to guarantee to the typechecker that the reference ST holds cannot leak outside the monad.

## Intuition about ST

ST lets we implement algorithms that are much more efficient with mutable memory used internally. But the whole "thread" of computation cannot exchange mutable state with the outside world, it can only exchange immutable state.

We can pass in normal Haskell values and then use ST to allocate mutable memory, then we initialize it and play with it; fianlly, we put it away and return a pure Haskell value.

ST is a monad that can hold mutable references and arrays, but has a "run" function that is referentially transparent.

ST is a lot like a lexical scope, where all the variables/state disappear when the function returns.

## Details

The `ST` type allows using update-in-place, but, unlike `IO`, it is escapable.

The ST actions have the form `ST s a`, meaning that they return a value of type `a`, and execute in a "thread" `s`. All reference types are tagged with their thread of execution, so that actions can only affect references in their own threads.

The type of the function `runST` that is used to escape ST:

```hs
runST :: forall a. (forall s. ST s a) -> a
```

The ST type uses an existential type for the state thread, `s`, so `s` cannot escape the context of ST.

The action we pass must be universal in `s`, so inside the action we do not have any information about the thread `s`, thus we cannot access any other threads, and therefore `runST` remains pure.

This is very useful, since it allows us to implement externally pure computations like in-place quicksort, and present them as pure functions without using any unsafe definitions.

```hs
quicksort :: forall e. Ord e ⇒ Array e -> Array e
```

However, the type of `runST` is illegal in Haskell98 because of the use of a universal quantifier inside the function-arrow, i.e. the type has rank 2 and Haskell98 only allows types of rank 1 at most.

Because we don't know the actual type, we can only do things to existential types that we can do to any type. *Existential types hide information from the consumers*. On the other hand, we can do things to a universal type as if it had any matching type of our choice, because it does not know, and does not care, about the actual use type. *Universal types hide information from the implementors*.

## ST interface

- `newSTRef` creates a new mutable cell, puts a value inside it
- `readSTRef` pulls out the ref
- `modifySTRef` updates the contained value 
- `writeSTRef` writes out a new value

Ubiquitous use of the `runST` is needed in order to escape the ST monad layer. But runST shoudn't be used yet if we first want to combine functions in ST. `makeArray'` does not have it so if we look at it in GHCi you don't see the value 10, but `«ST Action»`. If we haven't read the reference or frozen the vector, we cannot use `runST`. We'll get an error because that would leak the mutable value out of the monad.






## Refs

https://www.philipzucker.com/simple-st-monad-examples/
https://wiki.haskell.org/Monad/ST
http://www.haskell.org/pipermail/haskell-cafe/2007-July/028233.html
