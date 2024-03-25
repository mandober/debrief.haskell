# Effectful computations


- modeling side effects
- threading the state
- RWS
- monad transformers
- addressing monads in a monad stack
- transformers style
- mtl style



## Modeling side effects

Modeling side effects in a pure language is a problem with different solutions depending on the effects. Some effects, primarily input and output, constitute "proper", "hard", external effects and require a different approach than the one taken to model "soft" or internal effects. Internal effects, despite being side effects still, and thus incompatible with pure functions, are internal to the system, which leaves room for different solutions.

In a mutable language, declaring a global variable is sufficient to implement a range of internal effects, from global mutable state, global configuration, to logging and tracing (e.g. tracing the execution of called functions). Despite being effectful these computations may nevertheless be implemented in a pure setting using a different approach. Instad of declaring a global variable that holds some state, we might pass that state around to all functions that need it as an extra function paramter. This is called *threading the state* because we must pass the state into each function but also have them return the, possibly modified, state.

There are several Haskell data types meant to model these situations
- `Reader`, models a global readonly environment, like an app configuration
- `Writer`, models logging and tracing
- `State`, models a global mutable state

These 3 data types are better known as reader, writer and state monads as they all implement the standard FAM class hierachy. Since everything happens within the IO monad (of the `main` function), these three, along with other monads, are combined in various ways to write programs. For example, the I/O requirements of an application are satisfied by the `IO` monad; the availability of the configuration by the `Reader` monad; the exeptions could be handled by the `Maybe` or `Either` (`Expect`) monads; the management of a global mutable state by the `State` monad; logging by the `Writer`. Each monad provides a certain *capability*, so an application has a buffet of monads to pick the capabilities it needs.

>The next issue then is how to combine monads?

This is the turf of *monad transformers* (MTs). A MT is cartain (base) monad with a slot in which another monad can be plugged in, so a saturated MT provides the capabilities of both monads. Plugging a saturated MT into another MT allows us to further combine them into so-called *monad stacks*. At the base of each stack is the *base monad*, in which the computation takes place.

A MT is an extended version of a *precursor monad*. For example, the state monad, `State s a`, is extended with a slot intended to plug in another monad, so it becomes a monad transformers `StateT s m a`. Since MT are more general than their precursor monads, precursors are defined in terms of their MT, by pluggin `Identity` into the `m` slot.

>The next issue then is how to address other monads in a monad stack?

At the base of a monad stack is the so-called *base monad*, in which the computation takes place. That is, all function calls target the base monad, meaning some extra calls are required to address other monads in the stack. This concept is called *lifting*, and it is similar to mapping where a function gets lifted into a data structure. Here, a function gets lifted (off the base monad) in order to target a different monad in the monad stack. All such plumbing can be done manually (aided by writing some helper functions), but it creates boilerplate code which is always better to abstract away (thereby also minimizing the opportunities to make mistakes).

Since each monad brings its own set of skills to the stack, and each monad has a specific implementation (it is implmeneted in a specic way and it implements the FAM methods in a specific way), the approach towards cutting down the boilerplate is necessarily (ad hoc) polymorphic. The two elementary solutions are named after their MT packages, `transformers` and `mtl`.







## Keywords

side effects
external side effects
internal side effects
modeling side effects with pure functions



immutability
mutabile values
effectful computation



modeling effectful computation

observable side effects
global variables
mutable state
readonly environment
logging, tracing
monad transformers
monad stack
base monad
lifting
