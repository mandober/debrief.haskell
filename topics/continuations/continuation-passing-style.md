# Continuation-passing style

- Continuation-passing style (CPS)
- Direct style
- mechanics of functions
  - input and output
  - return value
  - function's call site
  - function's caller



Continuation-passing style (CPS) is a particular way of dealing with functions' return values, that is contrasted with the usual, direct style.



In the nominal style, you call a function with some args. Imperative PLs love the explicit "return" statement - it instructs the function to ceise all activity and immediately yield control back to the caller, (only) optionally handing it a value; if no value is given, the return value usually defaults to something like 'undefined'. In FP, function bodies are expressions, so the value of the expression is just implicitly returned. Some Pls, Rust for example, crossover these approaches having both explicit and implicit 'return' mechanisms (no semicolon terminator - hence it's an exp and return is implicit).

Either way, a function returns the output value directly to the caller. After calling the function, the caller hangs at the call site, waiting for the function to run to completion - at which moment the function hands over the control back to the caller (possibly with a value in some, necessarily with a value in other p:langs). By the way, returning a value is *the main effect* of (pure) functions.

```hs
{-| Nominal style -}

-- Nominal fn def
nominal :: a -> (a -> b) -> b
nominal x f = f x

-- Nominal fn call
c1 = nominal 0 succ


{-| CPS style -}

-- CPS fn def:
cps :: a -> (a -> b) -> (b -> r) -> r
cps a f k = k $ f a

-- CPS fn call:
c2 = cps 7 succ (2 ^)
-- trivial:
c3 = cps 7 succ id
```

CPS is an alternative way to deal with functions in which functions take an extra parameter - a function parameter called *continuation*. Eventually, the continuation is applied to the nominal output of the function (instead of letting it return to the caller). Of course, a continuation function must also be passed (as an argument at the call site) when calling such functions.

In the example, the `nominal` function returns a type `b`, but when converted into the CPS, its signature is expanded with an additional argument `b -> r`. So, its original return type `b` gets converted into a type `r` by the continuation. The function has no way of knowing what the continuation will do to its original result, type-wise - maybe it will remain at the same type `b` (in which case `b ~ r`), or it may be realized as a different type, `r`. This implies the change in the "overall" return type - instead of being `b`, the return type becomes `r` (i.e. the type `r` which is the continuation's return type) becomes the overall return type of the function.

At the call site, passing the `id` as the cont. has a noop effect - it tells the function to just return the result unbullied, by applying the `id` to it.
