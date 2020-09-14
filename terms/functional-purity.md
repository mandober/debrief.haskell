# Functional Purity

- honest functions
- side-effects
- pure functions
- referential-transparency

In Functional Programming Languages (FPL), functions are the shit. They are the basic building blocks and the most fundamental means of abstraction. Data flows through functions from the start to the end of a program. Functions are composed to make complex programs.

**Honest functions** are those functions that only refer to the paramaters they have declared to do their job, never referring to items from an outside environment. In fact, these functions are exactly like functions on math - they declare zero or more parameters and they must always return something, at least the unit value, if their output is insignificant. The most important property is that for the same input a mathematical function always returns the same output, and so do Haskell's functions.

**Side-effect** are the actions that mutate the external environment and state. They include actions like printing out a string to the stdout, fetching a record from a database, dealing with files. Actually, any IO action is a side-effect. Side-effects break referential transparency, but Haskell has a mechanism to control them in a safe and efficient manner.

**Pure functions** are honest functions that do not have side-effects. The litmus test for pure functions is determining whether they can be memoized. Haskell's functions are lazy and once their output value is computed (for a certain input) the output can be memoized as to not waste time computing it again (for that same input). Having no external dependencies, it is easy to make a custom library of functions, as well as to copy and paste them from anywhere to anywhere else; they will always work because all they need in order to perform their calculation they carry within themselves (omnia mecum porto).

**Referential transparency** is powered by the purity of functions. When the referential transparency is maintained, a variable can always be replaced with its definition. That's exactly what functions are - equations consisting of a name (identifier) on the LHS equated with an expression on the RHS. A call to a function can always be replaced with a function's output value.

Further benefits of referential transparency come in the form of more advanced compiler optimizations, such as function inlining, and the time saved from having to deal with alises (i.e. whether two names actually point to the same value); aliases are exectly the things that cause so many problems in imperative settings.

Another benifit is that people can reason about code much more easily; not having to follow where a variable is declared, where initialized, where it is changed to another value, is a huge relief in itself.
