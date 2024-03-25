# Specification and implementation

In computer programming, a **specification** is a mathematical description of the task a program is to perform, while an **implementation** is a program that satisfies that specification.

Specifications and implementations are quite different in nature and serve different purposes.

**Specification** expresses the programmer's *intent* or client's *expectations*, and it should be brief but clear. Specification communicates pre- and post-conditions and *invariants* a program should maintain.

**Implementation** expresses the definition of a program for execution by a computer. Implementation's goal is to be sufficeintly efficient in order to execute within the imposed or expected time and space limits.

The link between the two is the requirement that an implementation satisfies or meets the specification, and a (serious) programmer is obliged to provide a proof that this is actually the case.

Specifications and implementations relate to the whole program, but for the ease of reasoning and for the benefit of conducting viable proofs, a program is considered as being composed of smaller parts; namely, it is build by composing functions, which in turn are also composed of functions, and so on, down to the elementary parts which should be easier to comprehend and reason about.

Therefore, functions are the main entity to which specifications, and certainly implementations, pertain to. A specification of a function's value (returned value) states the nature of the intended relation between function's input and output.

## Stating specifications

- specs via comments
- specs via runtime checks
- specs via strong type systems
- specs via (external) STM solver
- specs via dependent type systems


The biggest issue when it comes to stating specifications is there is seldom a good (efficient) way to enforce them. Most languages lack proper support to efficiently state and enforce the specs, so *specs as comments* becomes the only means to that end. It is easy to imagine the problems that stem from such nonchalant approach.


An example is given by stating a function's specification as the comment: the result of the `increase` function should be greater than the square of its arg whenever the arg is greater than or equal to zero.

```hs
-- | [ increase x > x², forall x ≥ 0 ]
increase :: num -> num
```
