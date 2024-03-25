# Deriving Reader, Writer and State from the first principles

## TOC

<!-- TOC -->

- [TOC](#toc)
- [Pure functions and side effects](#pure-functions-and-side-effects)
- [External and internal side effects](#external-and-internal-side-effects)
  - [External side effects](#external-side-effects)
  - [Internal side effects](#internal-side-effects)
- [Modelling a global state](#modelling-a-global-state)
- [Threading the state through functions](#threading-the-state-through-functions)
- [The features](#the-features)

<!-- /TOC -->

## Pure functions and side effects

Haskell is a pure language - all functions are pure, i.e. free of side effects, yet a language to be useful must interact with the outside world, making side effects unavoidable. So how does one resolve this apparent contradiction of having both purity and side effects?

## External and internal side effects

Not all side effects are created the same. There are *external side effects*, which any interaction with the outside world falls under, but there are also *internal side effects*, which things like maintaining a global state, tracing function execution and similar, fall under.

### External side effects

There is no way to manage external side effects except through the `IO` monad. For example, writing a string to the console is definitely a side effect, there's no going around it (or is there?).

The "trick" that Haskell employs to deal with side effects but remain pure is to use *actions*, which are descriptions of actions - descriptions of effectful computations. Haskell, the language, does not deal with impurity (it can't), it only describes actions that need to be executed, so internally, purity is preseved.

This is because a description of an action is always the same for a given effect. Writing a string to the console has the same desciption (whatever that is and how it is represented), so a call to `putString`, parameterized by the string to print, always returns the same data (the same description). The returned information is not useful inside Haskell, so it is represented by the unit type `()`, wrapped in `IO`.

```hs
putString :: String -> IO ()
```

Internally, Haskell has only defined an action (potential), presented as a pure function. This allows Haskell to remain pure. The dirty job of excecuting this or any other action is outside Haskell - it is under the purview of the RTS (and what happens in RTS stays in RTS).

### Internal side effects

However, for internal side effects, which are things like maintaining a global state, tracing or logging the executions of functions, we need not use such a heavy machinery if they can be solved in a pure setting.

Some features, often needed in development, have been identified as falling under the category of internal side effects
- maintaining a global state
- maintaining a global environment
- tracing or logging

meaning it is possible to model them in a pure setting. We know that now, but it wasn't always evident; someone had to come up with a specific solution. But now that we know that this is doable, we can classify them as "internal side effects".

Thus, the *internal side effects* are qualified as those computations that are in impure PLs easily implemented as side effects, but need not be in general.
>That is, they may also be implemented in a pure setting without side-effects and without resorting to the `IO` monad.

## Modelling a global state

Taking the problem of maintaining a glabal state as an example: in an imperative language, we'd solve it by merely declaring a global variable. That would be sufficient for all parts of the code to have access to it. A global variable that holds some global state would not only be accessible, but modifiable as well, from within anywhere and by all parts of the code (in most PLs; some languages, e.g. Rust, permit multiple readers but forbid multiple writers to the same variable/resource).

>How to solve the problem of global state in a pure setting?

How do we implement global state, that is, having a piece of data, needed by various functions, be not only accessible, but modifibale as well, from within anywhere (from within any function) in the code?

The value is behind a globally accessible variable may be a simple scalar, like an integer, or a custom data structure. For example, we may use an integer as a global counter that keeps track of some event, and a custom data structure is more suitable to represent our app's configuration.

As already mentioned, in an imperative language, we'd simply declare a global variable, modifiable from anywhere, which consititutes a bona fide side effect, but in a purely-functional language the only allowed effect a pure function may have is its *main effect*, i.e. returning a value. A pure function requests all the values it needs through its parameters, and the args are supplied as copies (there is no call-by-reference here) when the function is applied. This means that the modification of a global (external) value passed into a function only affects the local copy of that argument.

## Threading the state through functions

With such constraints the solution imposes itself: to model a global state with pure functions, or rather, to thread that state through pure functions, we should *add an extra parameter*, 'state', to all functions that need to access it.

Instead of having a global variable to represent the state in an app, we have a 'state' parameter that gets passed around into and out of functions. Each function that takes a state, possibly modifying it, must also return it, so the *updated state* is passed into the next function (as opposed to some previous version of the state before that update).

Therefore, we add an extra input parameter - but also an extra output value - all functions that need to interact with the state. In fact, even if a function does not directly access the state, but is composed with a function that does, it must also be altered to receive an extra input param and to output an extra value.

This adjustment of the existing functions (before the state was implemented) cannot be avoided - all affected functions will have to change their prototype, i.e. their signature (aka, their API because adding an extra input param alone certainly changes that API).

>In brief, all functions that need access to a global state will have to be adjusted to take and return an extra value.

## The features

We are specifically trying to implement the following three features in a pure setting, in Haskell, and without resorting to the use of the `IO` monad
- maintaining a global state
- maintaining a global environment
- maintaining a global log

All three features are instances of the global state problem. First, a global state means having a globally available value that can be accessed and modified as needed. Second, a global environment is just like a global state, except it is read-only. Third, a global log is also like a global state, only the focus is on being able to write into it as opposed to being able to read from it.

In summary, these are the requirements for these features
- global state: read and write access  (+rw)
- global environment: read-only access (+r)
- global log: write-only access        (+w)

All 3 problems are solved in a similar manner, namely, by adding an extra input and output value to functions. Although all 3 problems can be solved in terms of the global state - e.g. with a global environment, we can just choose not to write to the global state - it would be more preferrable if each solution used only the minimum of capabilities it needs. Having a modifiable state instead of a true readonly environment only increases the risks of unexpected outcomes. Still, solving the global state should make it easier for us to implement the restrictions.

And now onto deriving the Reader, Writer and State data types from the first principles, staering with the State.
