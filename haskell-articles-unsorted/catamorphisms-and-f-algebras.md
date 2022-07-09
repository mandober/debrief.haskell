# Catamorphisms and F-Algebras

> So I often hear the words "catamorphism" and "recursion schemes". What is that about?

[![Oleksii Avramenko](https://miro.medium.com/fit/c/56/56/1*mwwEwV-EEykJL8kjrfuwUQ.png)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/@olxc?source=post_page-----b4e91380d134--------------------------------)

![](https://miro.medium.com/max/1400/1*-Aj1jfUhVAnQVwraQ1FO1w.png)

> So I often hear the words "catamorphism" and "recursion schemes". What is that about?

Catamorphisms (or **cata**) are generalizations of the concept of a fold in functional programming. Given an F-Algebra and a recursive data structure a catamorphism will produce a value by recursively evaluating your data structure.

> What is an F-Algebra? Maybe you can show some code examples first?

The setup is not that straightforward so let's start simple. Let's say you have the following data structure to represent an expression:

Haskell

Scala

And a simple expression can look like this:

Having just an expression data structure is useless, of course you'll need a function to evaluate and get the result:

This is where catamorphism generalization comes in:

> 🤨, but what's the point? You just applying an argument to a function?

That's because it's `notReallyCata`. The real `cata` function is generic and does not depend on any particular data structure or evaluator function. See, creation of a recursive data structure and then folding over it is a common pattern that `cata` tries to generalize.

> Ok, then how the real cata looks like?

> 🤯

That's why we started with `notReallyCata`. We'll break down the implementation later until it clicks. But now let's continue with our `Expression` example. First, we need to get rid of recursion by introducing a type parameter:

All references to `Expression` are replaced with `a` type parameter so the data structure is no longer recursive.

> Why is there an `F` at the end of type constructors?

Glad you asked - that's a hint that `ExpressionF` can be a `Functor`:

Nothing fancy, just applying some function to the wrapped value preserving stucture.

> Not sure why we need that 🤔

It doesn't makes sense now but it will a bit later. Now, the way we create our expression haven't changed (except for constructor names):

But the resulting type is different:

`expr` collapses everything into a single `Expression` while `exprF` encodes information about the nesting level of our expression tree. Speaking about evaluation, this is how we can go about implementing eval for `ExpressionF`:

The main difference with original `evalExpr` is that we don't have recursive call to `evalExprF` (`ExpressionF` is not recursive, remember?). It also means that our evaluator can work only with a **single level** expression:

And won't compile on this:

Simply because `exprF` expepects `ExpressionF Int` and we're shoving `ExpressionF (ExpressionF Int)`.

To make it work we could define another evaluator:

> Looks kinda ad hoc, what if you have deeply nested expressions?

Yes, for arbitrary nested expression this approach is not scalable - each additional nesting level requires you to write specialized function.

There is a way to generalize this nesting with a new type:

> Fix? Looks like a recursive data structure that doesn't do much. How is it useful?

Let's first look at the expression before the equals sign: indeed `Fix` is a recursive data structure that has one type parameter `f`. This parameter has kind `* -> *` e.g. it also takes a type parameter. For example, you can't construct `Fix` providing `Int` or `Bool`, it has to be something like `Maybe`, `List` or… `ExpressionF`. This is why we introduced type parameter for `ExpressionF`. Next, after the equals sign we have a single type constructor `Fx` taking a single argument of type `f (Fix f)` which is basically an expression that constructs `f`'s value. In case of `Maybe` it would be `Maybe (Fix Maybe)` and then the whole thing is wrapped with `Fx` into type `Fix Maybe`.

The type signature is confusing to read at first because of type constructor can have the same name as the type itself plus self referencing. But there is not much more to it than just wrapping a higher order type into a data structure. Btw, `unfix` is an opposite to `Fx` and all it does is pattern matching on `Fx` and returns wrapped value, no big deal.

Now, we will replace every `ExpressionF` of our expression tree with `Fix ExpressionF`. Notice the difference in constructing expressions with and without `Fx` - they're basically the same, except we need to prepend `Fx $`:

The resulting type of a 'fixed' version is `Fix ExpressionF` so we're back to a recursive representation, but now we have to use `unfix` function to get our non recursive data structure back.

> What are the benefits of having `Fix`? Looks like it's the same approach as original `Expression` type but now we have this weird `Fix` and `unfix` nonsense?

Yes, but we're trying to generalize the process of folding, it requires introduction of additional abstractions, like `Fix` and `Algebra` that we'll discuss later. Bear with me, it should make more sense later.

So we have our 'fixed' data structure, how would evaluation function look like?

Given a `Fix ExpressionF` the only thing we can do with it is calling `unfix` which produces `ExpressionF (Fix ExpressionF)`:

The returned `ExpressionF` can be one of our `ValueF`, `AddF` or `MultF` having a `Fix ExpressionF` as their type parameter. It makes sense to do pattern matching and decide what to do next:

Yes, it looks the same as our very first recursive evaluator for `Expression` with addition of having to unwrap the expression with `unfix`. So why bother with `Fix` anyway?

Here's the key: we will re-use our original 'fix-less' evaluator for `ExpressionF` and somehow distribute it over the `Fix ExpressionF` stucture. So this should be a function taking two arguments - the evaluator and the structure to evaluate:

Let's try figure out the implementation - the first logical thing to do is to use `unfix` to get `ExpressionF` and then maybe pass it to `evaluator`:

Obviously this doesn't work, `evaluator` expects `ExpressionF Int` and not `ExpressionF (Fix ExpressionF)`. By the way, remember that `ExpressionF` is a `Functor`? This is where it gets handy - we can use `fmap` to apply the same process to the inner level of our expression tree:

Take a moment and think about what happens: we're passing a recursive function `almostCata evaluator` into the `fmap`. If the current expression is `AddF` or `MultF` then this function will be passed one level deeper and `fmap` will be called again. This will happen until we reach `ValueF`, fmapping over `ValueF` returns value of type `ExpressionF Int` and that's exactly what our `evaluator` function accepts.

By looking at `almostCata` we can see that it doesn't really have anything specific to `ExpressionF` or `Int` type and theoretically can be generalized with some type parameter `f`. The only constraint should be having a `Functor` instance for `f`, because we're using `fmap`:

And that's the final version of `cata`. Here's the full implementation with some usage examples:

> I guess that's cool. But why tho?

A lot of concepts in category theory and functional programming are pretty abstract and sometimes it's hard to find immediate practical application for certain idea. But looking for abstractions and generalizations is useful for finding patterns and elegant solutions to problems that otherwise require ad-hoc implementation.

By the way, by generalizing our `ExpressionF -> Int` function to `Functor f => (f a -> a)` we discovered another important concept called **F-Algebra**. Basically F-Algebra is a triple of functor `f`, some type `a` and evaluator function `f a -> a`. Note that `a` here not polymorphic - it has to be a concrete type, like `Int` or `Bool` and it's called a **carrier type**. For any endo-functor `f` you can create multiple F-Algebra's based on it. Take our expressions example - endo-functor `f` is `ExpressionF`, `a` is `Int` and evaluator is `evalExprF`. But we can change the carrier type and produce more algebras:

> That's just different evaluators that can be passed into `cata`, right?

Yes, we're picking different carrier types and choosing our implementation. But there the trick - there is a mother of all evaluators that we can create by picking our carrier type to be… `Fix ExprF`.

> Evaluating to `Int` or `Bool` totally makes sense but what would this `initialAlgebra` evaluate? When do I need to have `Fix` of something as a result of my evaluator?

Of course you won't write something like that yourself, just want to show you the deeper meaning behind f-algebras and cata. In fact, we already have an implementation for such evaluator and thats exactly `Fx` constructor:

> Wait, `Fx` is an evaluator? That's crazy.

Yes and it does the most simple thing you can do - save the expession into a data structure. While all other evaluators (`algebra0`, `algebra1`) produced some value by reducing the expression (like doing sum or concatenation) `Fx` just wraps the expression without loosing any data.

This is why we introduced `Fix` in the first place - you first evaluate your original data structure with `Fx` into initial algebra `Fix f` and then using `cata` the 'real' evaluation happens by `fmap`ing your concrete evaluator over inital algebra.

From category theory point of view, all algebras based on the same endo-functor form a category. This category has an initial object which is our initial algebra created by picking the carrier type as `Fix f`. There are some great blog posts by Bartosz Milewski that I highly recommend checking out if you want to get deep categorical understanding.

> It's still pretty hard to comprehend, I don't think I fully understand the concept

It's always better to do hands on: try re-implementing `Fix` and `cata` on your own, think about possible data structures and algebras. For example, a `String` can be represented recursively (as a `Char` head and tail of `String`), the `length` of a string can be computed with `cata`. Here's some great resources for further reading:

*   [Understanding F-Algebras](https://www.schoolofhaskell.com/user/bartosz/understanding-algebras) and slightly different [F-Algebras](https://bartoszmilewski.com/2017/02/28/f-algebras/) by [Bartosz Milewski](https://bartoszmilewski.com/)
*   [Catamorphisms in 15 minutes](http://chrislambda.github.io/blog/2014/01/30/catamorphisms-in-15-minutes/) by Chris Jones
*   [Pure Functional Database Programming with Fixpoint Types](https://www.youtube.com/watch?v=7xSfLPD6tiQ) by Rob Norris
*   [Catamorphisms](https://wiki.haskell.org/Catamorphisms) on Haskell wiki
*   [Practical recursion schemes](https://jtobin.io/practical-recursion-schemes) by [Jared Tobin](https://jtobin.io/)


[Source](https://medium.com/@olxc/catamorphisms-and-f-algebras-b4e91380d134)
