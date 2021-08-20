# Monadic computations

- general form of monadic computations is approx. `m a ~~> ω`
- monadic type `m a` represents a computation delivering an `a`


## From "Generalising monads to arrows" by J. Hughes

Although the idea of programming with combinators is quite old, the design of combinator libraries has been profoundly influenced in recent years by Wadler's 2000's introduction of the concept of a monad into FP. A monad is a kind of standardised interface to an abstract data type of *program fragments*.

A parameterised type `m`, which may be considered a function from types to types (a type ctor, `m :: * -> *`), is a monad if it supports the two methods, `return` and `>>=` (bind).

Intuitively, we think of a value of type `m a` as representing a computation with result of type `a`, *a program fragment*. The nature of the computation is captured by the choice of the type `m`.

The `return` operation constructs a trivial computation that merely delivers its argument as the result, `return a ↓ m a`


The `>>=` operation
combines two computations in sequence, passing the result of the rst as an argument
to the second { hence the type of the second argument of >>=: it is a function that constructs the second computation, rather than just a computation.



## Monadic computation

The general form of monadic computations has a parameterized type constructor, `m :: * -> *`, and a type variable, `a`, expressed as `m a`. The `m` type param stands for a type (type ctor) that was made an instance of the `Monad` class, the condition expressed as a constraint, `(Monad m) => m a`.

The monadic type `m a` usually denotes a computation that deliveres a value of type `a`. However, with monadic types, aside from delivering an `a`, the computations are allowed to produce some kind of an effect as well. They are *effectful computations*.

## Type signatures

The monadic type `m a` represents a computation delivering an `a`, ad hoc and very informally denoted by `m a ~~> a`. The curvey arrow is a made up notation intended to incorporate the possibility of an effectfull production.

Perhaps, this topic should start with pure, effect-free, functions. In fact, all "proper" (i.e. mathematical) functions have an effect - the main effect of a function is that it returns a value (and a proper function must return a value). All other effects are *side-effects*.

When considering a type signature for a very general notion of a function, it cannot be a signature like `a -> a` since it restricts the input and output to the same type.

The next iteration would then be to go with something like `a -> b`, even though no sensible function has this signature (it may be a part of an overall signature, however).

In any case, the signature `a -> b` is here to represent a computation that takes an argument and delivers some result. Unfortunatelly, the notation simply forces us to write some concrete identifiers for type variables, but `a`, and especially, `b`, are intended to be regarded as "some type, forgetaboutit".

This is because we'd like to consider the process of transformation itself, which is best represented by functions, but then functions require precise signatures.

## Input and output

Monads have made their appearance in Haskell in order to deal with effectful computations, most pressingly it was to deal with aspects of I/O operations in a pure FPL. Specialized to those kind of operations, the monadic type `m a` becomes `IO a`, meaning the notation `m a ~~> a` should be `IO a ~~> a`.

Monadic types may represent many kinds of, possibly effectful computations, but a special one is `IO` that is definitely effectful.

The general expression for monadic computations, i.e. that an `m a` delivers a result `a` may apply to IO as well, but very broadly because, unlike the other monadic types, the type `IO a` has a firm grasp on that `a` in there - the `a` can never escape from the `IO` monad. Nothing can escape from `IO`, muahhaha.

## The effects

Monads can express all kinds of computations and computional effects. Considering the general form of monadic computations, `m a`, the `m` can be a type ctor in the effectful context.

When `m` is the type ctor `Maybe` then the type `Maybe a` represents a computation that can fail (with an unimportant, so ignored, error message).

If the error message is important, exceptions may be repr by having `Either` for `m`, so a type `Either e a` repr a computations that either succeeds, delivering an `a`, or fails delivering an error message `e`.

When `m` is `[]` it can repr non-deterministic computations, which usually mean that they produce more then a single result.

```
         a ~~> b    pure computation
       m a ~~> b    embellished computation:
      IO a ~~> b      IO side-effects
      [] a ~~> b      indeterminism
   Maybe a ~~> b      optional result (result or unimportant error)
Either e a ~~> b      exceptions, either an error or a result
  Reader a ~~> b      fixed readonly input; shared environment
  Writer a ~~> b      logging, writing on the side (-effect)
   State a ~~> b      stateful computations
```
