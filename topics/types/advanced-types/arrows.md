# Arrows

https://en.wikipedia.org/wiki/Arrow_(computer_science)
https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial
https://en.wikibooks.org/wiki/Haskell/Understanding_arrows

In CS, arrows or *bolts*, first proposed by John Hughes, are a type class used in programming to describe computations in a pure and declarative fashion.

**Arrows** are a generalization of monads providing a referentially transparent way of expressing relationships between logical steps in a computation, but unlike monads, arrows don't limit steps to having one and only one input.



https://www.haskell.org/arrows/index.html

Arrows are a new abstract view of computation, defined by John Hughes. They serve much the same purpose as monads -- providing a common structure for libraries -- but are more general. In particular they allow notions of computation that may be partially static (independent of the input) or may take multiple inputs. If your application works fine with monads, you might as well stick with them. But if you're using a structure that's very like a monad, but isn't one, maybe it's an arrow.


## Generalising monads to arrows - John Hughes

Monads have become very popular for structuring functional programs since Wadler introduced their use in 1990. In particular, libraries of combinators are often based on a monadic type. Such libraries share (in part) a common interface, from which numerous benefits flow, such as the possibility to write generic code which works together with any library. However, some interesting and useful libraries are fundamentally incompatible with the monadic interface: efficient parsing, building GUIs, and programming active web pages. On the other hand, they are a natural fit for arrows, which are *generalisation of monads*, with significantly wider applicability.

### 1. Introduction

One of the distinguishing features of FP is the widespread use of *combinators* to construct programs. A combinator is a function which builds program fragments from program fragments; in a sense the programmer using combinators constructs much of the desired program automatically, rather than writing every detail by hand. The freedom that FPLs provide to manipulate functions (i.e. program fragments) as first-class citizens supports combinator programming directly.
