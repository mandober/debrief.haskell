# IO Monad

## 1. IO and Purity

Haskell is a purely functional language. Haskell functions are pure; thay maintain referential transparency, that is, they always produce the same output given the same input and they never have side-effects. The popular example is a seemingly pure function that increments its input; it takes an integer, launches some nuclear missiles (side-effect) and returns the incremented number. That is an honest function - it only uses what it has declared it needs (it has declared that it needs an integer), but it's not pure because it has (devastating) side-effects. The litmus test for purity is to check if a function can be memozied.

Side-effects are actions that affect an external environment, an external environment relative to the environment that executed or issued those actions. Such effects modify the external environment, thereby violating the referential transparency, so they are forbidden practice for pure languages.

> And that poses a problem: if a pure language cannot change anything (cannot take input, cannot make output, etc), then how can it get anything actually done?

Side-effects are relative. They depend on the part of a system they occur in. What's side-effect for one pure function might not be a side-effect for the containing module. What's side-effect on a module level might not be a side-effect from the aspect of the whole program. The notion of side-effects depends on the size of a system, that is, on the part of system from where the observation is made.

A side-effect in a subsystem that is a part of a larger system might not be considered as such in that larger system because the larger system contains the entire subsystem, including the environment that was affected by that side-effect.

When we come to this point that side-effects are relative to the size and aspect of a system, then it's just a matter of choosing the system from which we'll make observations. Any system is contained inside a larger system. No matter how independent it may be, in the end all systems are contained in the mother of a sytem that is the world (a world?).

Theoretically, every larger system may posses all the information about its containing systems. So, the world (or universe) as the biggest system may be thought of as a *Laplace's demon*, having the complete information about everything it contains, down to the state of every last particle.

Now, if we create a function that takes some state of the world, along with some more modest arguments, and after performing some I/O computations returns the results along with the next state of the world, then we have a pure function that handles I/O impurities.

```hs
-- f takes an arg and the current state of the world, performs a computation,
-- and returns the results along with the next state of the world
f :: (a, World) -> (b, World)
f (x, world) = let (x', world') = ...

-- with currying the sig becomes very familiar:
f :: a -> World -> (b, World)
f :: a -> s -> (a, s)
```


## Forwarding impurity

Haskell makes programs that are executed by th OS on a machine. That is, Haskell is a pure language that produces a completely pure program that is exectuted by an impure run-time. As far as Haskell is concerned its programs *are* pure. What happens later, during runtime is none of its business.

This phylosophy can also be seen a Haskell programming where all the functions are pure and cannot deal directly with actions. The only function and the first function that is called when the program runs is `main`. Only `main` can deal with IO. That way the impure IO actions are squeezed out of pure functions and pushed towards and into the main function. This may seem like passing a problem onto somebody else, but it is a good thing since all the impurities are concentrated in a single space. In case of trouble, there is no need to examine every possible function in a program, you only need to look at the functions with signature that involves `IO` types, starting with `main`.

The situation is like the impurity is puched into main, but main itself is also pure! It is only on program execution that impurity starts to creep in.
