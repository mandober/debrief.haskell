# Side effects

Side effects are related to computation, usually in terms of functions, and they divide functions into *pure functions* and *effectful functions* (or computation). 


Side effects are changes to the environment outside a function (module, program). In fact, side effects are *relative*, so what is a side effect for one agent (function, program) may not be for another (usually, a bigger, encompasing agent). As long as functions are concerned, a side effect is any form of interaction (input-output) with the outside world, like writing to the console, prompting the user for input, connecting to a DB, etc. In the strict sense, side effects even include things like changing a memory location. But whether a change of memory location is considered a bona fide side effect actually depends on its observability. If the other parts of the code can observe such change, then it counts as a side effect; but if the change cannot be observed by other parts of the code, then it gets a pass.

This prompts an updated definition of a *side effect as any observable mutation*.

>Side effects are observable mutations to the environment.

Functions that do not exhibit side effects are called **pure functions**. A pure function always returns the same output for the same input.

>Functions remain pure by *explicitly requesting* all the resources they require to do their work through their *parameters*.

When a pure function is applied, it receives the copies of the requested resources (values, references). It can do whatever it wants to its arguments because mutation is only local to the function.

Some pure functions are even purer - these are called **combinators** - and they do not use anything from their environment (immediate or otherwise), not even the names provided by the stdlib.

A combinator may take one or more arguemnts, but then it only uses function abstraction and function application, à la lambda calculus, and nothing else - it may not even refer to the names provided by the standard library. Moreover, a combinator must never throw an exception (by, e.g. calling the Haskell's `error` function. ~~Can a combinator diverge? which would also be represented as bottom?~~). Thus, the combinators are the purest, the most honest functions.

Haskell is not a total language, so it does not require that (pure) functions be total - there are even a lot of partial functions in the stdlib, after all. A partial function is still counted as a pure, even though it is not *honest*. **Dishonest functions** have a habit to break their promise regarding their return value - they state one thing, but then it turns out, that is not the whole story. They may diverge (all function can) by explitily calling `error`, by falling into endless loops, etc. In Haskell, all these divergent states are signified by the same value, bottom, so they are indistiguishable from each other.

Besides pure and honest functions, there is one more related category of **isolated functions**, which are function that get the information about the outside world only via their params. So, combinators are definitely isolated, but pure functions are not necessarily isolated. And even impure functions can be isolated (in other PLs). Isolated functions are important in unit testing. A *unit test* is an automated test that test a unit in isolation from its dependencies.

The **standard library** (stdlib) is very specific when it comes to the purity of functions in relation to their environment. The stdlib allows functions to remain pure despite referencing "external references". A function may refer to something it did not explicitly requested. Normally, such things incude a range of primitive operations, from addition to higher-order functionals, all made available by the stdlib. A set of names (identifiers) is implicitly made available in every module, while other, less common names must be explicitly imported, after which they are readily accessible from within anywhere (in that module). The fact that these names are external to the function is aliviated by the guarantee that they are always the same (the same name always resolves to the same definition, from program to program), and they are available without failure.

This may be contrasted by a global (module-level) variable in a Haskell module: declaring such variable makes it available to every function, but this is not the same as when a function refers to a name from stdlib. The variable itself (its declaration), and the value behind it, is not guaranteed to stay the same (from module to module, and from program to program), the way stdlib names (values) are. A function refering to the addition operator can be transplanted into another module without worries - it will work and it will work the same. But a function referring to a module-local name does not have such properties,  justifying the *Reader monad*, which provides access to a global, readonly environment (first of all, it immediately places a request to such an external resource by declaring a new parameter).




## The ReaderT design pattern
https://www.fpcomplete.com/blog/readert-design-pattern/

By the way, once you've bought into `ReaderT`, you can just throw it away entirely, and manually pass your `Env` around. Most of us don't do that, because it feels masochistic (imagine having to tell every call to `logDebug` where to get the logging function). But if you're trying to write a simpler codebase that doesn't require understanding of transformers, it's now within your grasp.
