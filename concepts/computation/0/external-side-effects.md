# Effectful computation :: Introduction

## Side effects

Pure functions do not tolerate side effects. However, before we delagate any resamblence of effectful computations to the `IO` monad, we should first at least make sure no solution within a pure setting in possible. Because not all side effects are equal.

Some computation, like the one that requires the comunication with the outside world, is truly effectful. This is a class of computation that induces *external side effects*. It is as dirty as it gets, so it must be dealt using the `IO` monad, there's no other way about it.

On the other hand, there is a class of computations that are internal to the program - at least they do not necessarily require the use of the I/O. These could be classified as *internal side effects*, and they include things like configuration, logging, and having a global mutable state.




is easily solved just by declaring a global variable.

Imperative languages naturally solve things using side effects, they need not take any special considerations whether or not to use effectful computations - it is natual to do so. So, there, the internal effects are solved and implemented the same way as external. However, in purely FPL like Haskell, we do make special considerations regarding the kind of effects we are dealing with. 




cannot be dealt any other way besides using the `IO` monad, but there is a whole class of effectful computations build around the notion of a *global state* that would be better handled without using such a heavy machinery.


## External side effects


The `IO` monad is great for I/O, but we try to compute with pure functions for as long as possible, pushing the impurity into the IO eventually. Once a value is in the IO monad, it stays there forever - unlike most other monads, the IO monad is inescapable. It also spreads and infects other functions related to it, turining them into *IO computations*. That is why we try to steer clear of it for as long as possible, preferring to deal with pure functions instead. It is thus preferable to resolve problems without involving IO whenever possible.

The problem of *global state* is easily resolved in imperative languages merely 

Such variable is t hen accessible from within anywhere in the code. We'd like to model that in Haskell (but without involving the IO) because having a global value has many practical uses. For one, applications often need *configuration* that needs to be made accessible by all interested code agents. Programs also often need logging and tracing facilities.

>All these requirements may be subsumed and solved with a global variable that holds mutable value.

We can identify these three requirements which can be solved with a global mutable state:
1. exposing a configuration, i.e. having a readonly environment
2. having a global mutable state
3. logging and tracing

These three things do not necessarily involve input-output; these are not external side effects, so we'd like to implement them in pure setting. And it turns out that we can model such *internal side effects* using nothing but pure functions.


Effectful computations that involve a global variable include things like maintaining a global mutable state (`State` monad), readonly environment (`Reader` monad), and logging or tracing (`Writer` monad). These three can be implemented with pure functions and the mentioned monads.

Actually, since we also want to combine effects and capabilities of different monads, we rather implement these things using monad transformers which are precursor monads with a slot into which another monad can be plugged in. For example, `State` is precursor monad to the monad transformer `StateT`, but the latter has a slot `m` into which another monad can be plugged in, so the resulting type has capabilities of both. In this way, we can combine monads and build a monad stack, which contains all the different monads whose capabilities we need available in our application. At the base of a monad stack is the *base monad*, in which the computation takes place. By default functions target the base monad, so to address another monads in the stack, we must `lift` the function an appropriate number of times. This is the extent to which the package `transformers` goes.

Other MT-related packages start where the `transformers` package ends, trying to introduce further comfort with convenience methods, expecially when working with deeply nested monad stacks. These packages do not bring new functionality to the table, they only make the coding more pleasent.

The package `mtl` is considered the prime successor of the `transformers` package, reusing its definitions. Its goal is to increase the productivity by decreasing the amount of the boilerplate involved when working with large monad stacks. For one, it successfully cuts down the number of times we call the `lift` function (to target other monads in the stack). It does this by further abstracting the capability of each monad into their own class. For example, the `StateT`'s capability to deal with stateful computations are abstracted into the class `MonadState`. Then the State monad implements this class, thereby polymorphisizing its methods (`get` and `put`). In fact, the State monad is the only "proper" member of this class - all the other monads implement this class only to be able to delagate the stateful functions to the State monad. It is similar for other MTs and their capabilities, so there are also classes like `MonadReader` and `MonadWriter`. After that, we can freely call a function (without lifting it) that targets e.g. the Reader monad (e.g. like `asks`) and it will be "dispatched" automatically to the ReaderT MT.
