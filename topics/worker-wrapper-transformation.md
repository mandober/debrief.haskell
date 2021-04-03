# The worker/wrapper transformation
paper by Andy Gill, Graham Hutton, 2009


## The Basic Idea

Given a function `f`, defined in the form `f = body` where `body` is the RHS of the definition, and may include recursive calls to `f`.

At this syntactic level, the first step in applying the worker/wrapper transformation is to define appropriate functions `wrap` and `unwrap` that allow the function `f` to be redefined by the equation `f = wrap (unwrap body)` which is then split into two equations by naming the intermediate result (function) as `work`.

In this manner, `f` has been factorised into the application of a wrapper function `wrap` to a worker function `work`, where `work` is defined by applying `unwrap` to the body of the original definition for `f`.

If `f` is a recursive function, then the `body` mentions `f` (in the recursive calls), so now `f` and `work` are mutually recursive. The next step in the process is to eliminate such mutual recursion by inlining the new definition of `f`, i.e. `f = wrap work`, in the definition for `work`, thereby making `work` into a recursive definition independent of `f`.

The notation `body[wrap work/f]` means to replace every free occurrence of `f` in the `body` with `wrap work`, thereby inlining f.

```hs
-- original function
f = body

-- redefinition
f = wrap (unwrap body)

-- naming the intermediate fn
f = wrap work
work = unwrap body

-- but if f was recursive, inline it
f = wrap work
work = unwrap ( body[wrap work/f] )
```


## The worker/wrapper transformation

In order to give a precise treatment of the worker/wrapper transformation, and hence prove its correctness, we move from the informal syntactic approach, based upon inlining, to a formal semantic approach, based upon the use of fixed points.

We begin by defining a fixed point operator which takes a function and returns its fixpoint. This operator doesn't really find a function's proper fixpoint, e.g. given the square function over the ℕ domain, it won't return 1 as the square function's fixpoint. In fact, it sort of "cheats" by returning itself as a value i.e. as a function, which does act as a fixpoint. It's like saying that a fixpoint of `f` is a fixpoint of `f`. Which it is :) That's why every function in lambda calculus has a fixpoint.

It's because a fixpoint of `f` is `fix f`, where the `fix` function "calculates" the fixpoint of its argument function, such that when `f` is applied to the result (i.e. its fixpoint), the same result is returned right back, `f (fix f) = fix f`. Exchanging the sides of this equation, we get the definition of the `fix` operator.

```hs
fix :: (a -> a) -> a
fix f = f (fix f)
```

The property of this operator that we will use to formalise the worker/wrapper transformation is **the rolling rule** (Backhouse, 2002), which allows us to pull the first argument of a composition outside a fixed point, resulting in the composition swapping the order of its arguments, or *rolling over*:

fix (g ◦ f) = g (fix (f ◦ g))
