# The type of continuations

In Haskell, the core type of continuations is a function type `(a -> r) -> r`, as well as the proper data types `Cont` and `ContT`, which just wrap the function type in a newtype, with `Cont` differing from `ContT` by the monadic context of the latter.

## Bare function type

However, continuations are usually manipulated not as this "bare" type, but wrapped in a newtype `Cont` or `ContT` which also makes possible for them to become instances of various type classes.

In order to make continuations instances of type classes, the continuation function type is wrapped in a newtype `Cont`, or its more general variant `ContT` that also embeds a monadic context. In the end, instead of a separate newtype declaration, `Cont` is expressed as a type alias in terms of the more general `ContT` type, with the monadic context satisfied by the neutral `Identity` monad.

But before exploring these two newtypes, let's focus for a moment on the unadorned type of continuations, `(a -> r) -> r`.

The first thing to notice is that the type of continuations is a function type. When we discuss continuations, we refer to this specific type as "bare" (native or unadorned), so it is distinguished from the `Cont` and `ContT` that just wrap it in a newtype.

The function type `(a -> r) -> r` expects a unary function, `a -> r`, which itself takes a type `a` to a type `r`, that then becomes the type of the overall expression. 

We call this whole type a continuation, but we also call the nested unary function a continuation, and along with even more things that we casually refer to using the same word, the exact meaning of the term "continuation" becomes too overloaded and removed from the original intention.

---

Formally, a **continuation** represents the rest of the computation from the perspective of a specific subexpression. Namely, when evaluating an expression, the next subexpression to be evaluted is selected. Considering, for example, the expression `1 + 2 * 3 - 4`. We'll call the entire expression *the overall expression*. This one contains several subexpressions, and the evaluation of the entire expression proceeds by first selecting the next subexpression to be evaluated. Folowing the rules of precedence, here, the selected subexpression is `2 * 3`. The subexpression selected for evaluation is called **the current redex**, and it is denoted by brackets for emphasis, `1 + ⎡2 * 3⎤ - 4`.

```scheme
1 + 2 * 3 - 4
1 + ⎡2 * 3⎤ - 4
1 + ⎡∙⎤ - 4
⟨1 + ⎡∙⎤⟩ - 4
⟨∙⟩ - 4
```

This subexpression is the focal point in the explanation of continuations

is, so it has a very specific name, viz. **the current redex**.

The continuation is described from the persepective of the current redex as the rest of the computation (here, as the rest of the evaluation of the overall expression). To emphisize the continuation, the current redex also represents a "hole", as `1 + ⎡2 * 3⎤ - 4`



- `⎡∙⎤` marks a hole (current redex), replaces [∙]
- `⟨∙⟩` marks a delimited continuation

```
1 + 2 * 3 - 4
```

We'll refer to the type that a function in direct style would have returned, had it not been converted into the continuation-passing style, as *nominal return type* or *nominal result*. When converted into the CPS, the function must declare an additional formal parameter that will bind the function argument, referred to as a *continuation*. Dually, all the call sites will also have to be modified to pass a continuation function besides other arguments. When conveted from direct style to CPS the extra parameter is denoted by `k` and is usually declared last.


However, the function that is converted from the direct style into CPS is not mentioned at all - its only remnant is the type `a` that it would return.



## `Cont` and `ContT`

newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

-- (1)
type Cont r a = ContT r Identity a
-- (2)
type Cont r a =  Cont { runCont :: (a -> r) -> r }
