# Exceptions

Haskell has managed without exceptions for a long time, so it's natural to ask whether they are relly necessary. We can identify 3 ways in which exceptions are typically used in languages that support them:
* **Disaster recovery** uses an exception to signal a rare error condition, such as division by zero or an assertion failure. In Haskell we may add pattern-match failure, when a function is applied to a value for which it is not defined. The programmer may also raise an exception, using a primitive such as the `raise` function. The exception handler typically catches exceptions from a large chunk of code, and performs some kind of recovery action. Exception handling used in this way provides a degree of modularity: one part of a system can protect itself against failure in another part of the system.
* **Alternative return** is the situation when an exception is returned from a function as an alternative return value, where no error condition is necessarily implied. For example, quering a structure about the presence of some element will always involve the posssibility of failure. The exception handler typically catches exceptions from a relatively narrower scope, and serves mainly as an alternative continuation for a call.
* **Asynchronous events**. In some PLs, an asynchronous external event, such as an interrupt, are reflected back into the language as exceptions. We call such things *asynchronous exceptions*, to distinguish them from the two previous categories, which are both *synchronous exceptions*.

Perhaps, more broadly, exceptions can be classified as recoverable and irrecoverable, where the latter signify some critical conditions that are best bailed out of with a panic.

## Exceptions as values

Before Haskell, no lazy FPL supported exceptions, for two big reasons:

* Lazy evaluation scrambles control flow. In a lazy FPL, evaluation is demand-driven: an expression is evaluated only when its value is required. As a result, programs don't have a predictable control flow. The only productive way to think about an expression is to consider the value it computes, not the way in which it computes it. Since exceptions are typically explained in terms of changes in control flow, exceptions and lazy evaluation do not appear very compatible.

* Exceptions can be explicitly encoded in values, so perhaps exceptions are not strictly necessary. For example, a function that performs division and expects two integers, can specify (perhaps in comments) that it assumes that neither argument is zero, also letting you know that a panic will ensue otherwise. This shifts the responsibility to the client that has to make sure the input is cosher; it also works in favor of conscienceous clients because the is-zero check is elided. This is called *optimistic programming*. The approach in which the implementor forces the check is called *defensibile programming*. Returning an optional result means the caller will have to check the returned value, regardless of whether they have already did. The advantage is that no extensions to the language are necessary to implement it. Also, a function's signature makes it clear whether it can raise an exception.

## Inadequacies of exceptions as values

Encoding exceptions works nice for the alternative-return use of exceptions, but badly for the disaster-recovery use case, and not at all for asynchronous events. There are several distinct problems:

* *Increased strictness*. When adding exception handling to a lazy program, it is very easy to accidentally make the program strict by testing function arguments for errors when they are applied instead of when they are used. For example, parser combinators typically don't return any result until the entire input has been parsed; this may be a major problem on very large input. This increased strictness is often easily avoided by using the *call-by-name translation* instead of the *call-by-value translation*, but it's surprising how easy it is to fall into this trap.

* *Excessive verbosity*. The principal feature of an exception mechanism is that exceptions propagate implicitly, without requiring extra clutter in the code between the place the exception is raised and where it is handled. In stark contrast, the explicit-encoding approach forces all the intermediate code to deal explicitly (or monadically) with exceptional values. The resulting clutter is absolutely intolerable for those situations where exceptions are used to signal disaster, because in these cases propagation is almost always required.

* *Built-in exceptions are un-catchable*. In Haskell, any cause that lets a computation diverge (divide by zero, pattern-match failure, any kind of error) are indistinguishable from one another, and all are treated semantically as bottom, `⟘`, evaluation of which halts the program. Haskell allows the explicit halting by calling the `error` function. Haskell has no way to catch and recover from such a synchronous error event. This is a serious problem when writing programs composed out of large pieces over which one has little control since there is just no way to recover from failure in any sub-component.

* *Poor efficiency*. Exceptions should cost very little if they don't actually occur. Alas, an explicit encoding into values forces a test at every call site, which could have a negative impact.

* *Loss of transformations*. Functions written in a monadic style have far less fewer transformations than their pure counterparts.

* *No asynchronous exceptions*. Asynchronous exceptions, by their nature, have nothing to do with the program itself, since they are out of a program's controll, arising from the external sources that cannot be handled.

## A new approach

The first design decision is forced by the fact that Haskell is a lazy language: exceptions are associated with data values, rather than with control flow. A value (of any type `a`) is either a normal or exceptional value that contains an exception. The data type `Exception` is the type of exceptions. It is a fixed, system-supplied, algebraic data type, defined perhaps like:

```hs
data Exception
  = DivideByZero
  | Overflow
  | UserError String
  -- etc.

raise :: Exception -> a

error :: String -> a
error str = raise (UserError str)

getException :: a -> IO (ExVal a)
```

For each type `a`, the function `raise` maps an `Exception` into an exceptional value of type `a`; meaning that every type has potential to be exeptional.

> It is **values** not **calls** that may be exceptional, and exceptional values may, for example, hide inside lazy data structures. To make sure they don't you need to force the evaluation (with e.g. `seq`).

- `getException` forces the evaluation of its argument to HNF. Before it begins this evaluation, it marks the evaluation stack in some way.
- `raise ex` simply trims the stack to the top-most `getException` mark, and returns `Bad ex` as the result of `getException`.
- if the evaluation of the arg to `getException` completes without the `raise`, then `getException` returns `OK v`, where `v` is the value of the arg.
