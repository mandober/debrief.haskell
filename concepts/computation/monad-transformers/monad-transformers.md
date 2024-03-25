# Monad transformers

- monad type
- monad base type, monad transformer precursor
- monad transformer type, monad with a slot
- monad stack, monadic stack
- base monad (in the monadic stack)
- interleaving of monadic effects
- lifting operations
- targeting the specific monad in the stack
- issues with MTs
- alternative technics to MT and monad stacks

https://www.snoyman.com/reveal/monad-transformer-state
https://www.fpcomplete.com/announcing-new-unliftio-library
https://www.fpcomplete.com/the-rio-monad
https://www.fpcomplete.com/haskell/tutorial/fundeps/

https://www.fpcomplete.com/haskell/tutorial/monad-transformers/
In `EitherT e m a`, we call the `m` parameter the *base monad*. For very good reasons we always make the base monad type variable, `m`, the second-to-last variable in defining the type. We consider `EitherT` a transformer which is layered on top of the base monad.
...This is also why we always keep the last type variable the result type (`a`), and the second-to-last the base monad (`m`): it allows us to define the `MonadTrans` class.


## Definition

A **monad transformer** is a data type that combines the capabilities of two monads into a single one, exposing the capabilities of both.

Monad transformers are composable allowing us to build a monad stack, that has a *base monad* (usually `IO` or `Identity`) at its root with the other monads or monad transformers stacked on top of it. Such a monadic stacks allows us to interleave the effects and capabilities of multiple monads.

## Expansion slots in types

Data types that are instances of the FAM classes are usually called monads. For example, implementationally, the reader type is, in fact, just a function type wrapped in a `newtype` construct, and the writer type is similarly only a wrapped pair type. Functionally, the reader type solves the problem of having a global read-only environment that all functions can reference. The state type is an enhanced reader because it exposes a global mutable state. The writer type implements the requirement to maintain a global log of some kind. These data types implement the FAM classes, thereby deserving to be called monads. Each monad arose as a solution to a specific problem, and each one is equipped with a set of capabilities specific to the problem they solve.

In this form, however, we wouldn't be able to combine different monads. This is where the monad transformers come in. Each MT is based on a specific monad type, but the important difference between a MT and its base type is the fact that MTs have a "slot" into which another monad can be "plugged in". This is the basic mechanism how MTs compose monads in order to build a *monad stack*.

For example, consider the base `Reader` monad vs `ReaderT` monad transformer:

```hs
newtype Reader  r   a = Reader  (r ->   a)
newtype ReaderT r m a = ReaderT (r -> m a)
```

The slot is represented by the type param `m`.





A monad transformer is usually a monadic data type that carries some capability - e.g. the `Reader` exposes access to a global environment - and comes with a "slot" (in the form of the type param `m`) into which the monad with which it is combined is placed. Taking the reader as an example, the `Reader` data type offers only the access to a global environment, while the `ReaderT` data type is a monad transforming version of the reader with a free slot to "plug in" the other monad. Plugging in the `IO` monad yields a *monad stack* that offers the capabilities of the `IO` monad as well as the `Reader` monad. Plugging in another monad transformer, allows building a deeply nested monad stacks consisting of multiple monads, each one with its own set of capabilities. The entire monad stack then merges the behaviors of contained monads and incorporates their behavior.



A monad transformer is a data type, usually defined as a newtype. It is a monad that is based on another monad (referred to as the precursor monad); this fact is reflected in the similarity of their names: the transformers have the same name as their corresponding precursor only suffixed with a `-T` (e.g. `State` vs `StateT`, `Maybe` vs `MaybeT`, `IO` vs … gotcha! there's no `IOT` because the `IO` monad, if used, must be the base monad). Different monads are combined and their effects composed in a stack-like arrangement, with one monad nested inside another. The base monad is the most deeply nested monad, i.e. the one at the base of the monad stack. The outer monad is the exposed monad, in terms of which the whole monad stack is typed. However, for one monad to host another monad (which itself may be wrapping yet another monad, etc.), while retaining the capability to pinpoint and address any monad in this structure, we don't use vanilla monads but their t-suffixed transformer versions, all of which have a type parameter `m`, a slot for a monad they wrap. For example, the State monad transformer,`StateT s m a`, is parameterized by a state `s`, a monad `m`, and a value type `a`. Its precursor is the `State` monad, which is actually based on its transformer (with `m` filled by the `Identity` monad), rather than the other way around (which is the case with monads like Maybe, Either, list, etc.).

## Issues with MTs

Some issues with MTs include
- Combining distinct monads is usually unproblematic, but **using the same monad twice** (for whatever reason) can be a problem.
- Taking into account the fact that many apps have the similar requirements (global state, global env, debugging logs, I/O needs, handling of exceptions, etc.) are there recommended and best-practice scenarios for building such monad stacks?
- Each monad in the monad stack is a "guest" of the base monad, i.e. all operations target the base monad by default. To change the receiver, we need to *lift* the operations a suitable number of times in order to target the appropriate monad. There are some solutions to MT problem that do this automatically, though.
- Deeply nested monad stacks are often accused of being terribly slow?!


## Monad stack

A *monad stack* rarely consists of a single monad. As soon as we are dealing with I/O, we have the `IO` monad, and if we also need (as we usually do) some global environment, then the `Reader` monad must be in the stack as well. If that environment is mutable then we need the `State` monad instead. To be able to handle exception, we must also introduce the `Except` monad to the stack.

The *base monad* is the innermost monad, the one at the bottom of the stack, aka the primary monad around which the others are wrapped. This is usually the `IO` monad, or the `Identity` functor, but it might as well be any other monad.
