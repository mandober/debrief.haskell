---
downloaded:       2022-01-08
page-url:         https://www.quora.com/What-is-continuation-passing-style-in-functional-programming
page-title:       What is continuation-passing style in functional programming? - Quora
article-title:    What is continuation-passing style in functional programming?
---
# What is continuation-passing style in functional programming? - Quora

Answer (1 of 2): In my view, these sorts of questions need an answer in two parts: definition (what CPS is) and motivation (why we care). Lets take a look at both.

CPS
Continuation-passing style is a particular way of defining and calling functions. Both the definition and the use-site of a func...
In my view, these sorts of questions need an answer in two parts: definition (what CPS is) and motivation (why we care). Lets take a look at both.

CPS

Continuation-passing style is a particular way of defining and calling functions. Both the definition and the use-site of a function have to cooperate to use CPS.

Normal functions take in some arguments, do some computation and return the result. The actual process of returning the final value is implicit, entirely handled by the language. The idea with continuation-passing style is to make "returning" an explicit action, giving us more control of the final control flow. Functions written in CPS take in an extra parameter called a callback, which is a function itself; instead of returning the result of their computation, they call the callback on it.

The callback is called a continuation because it tells the program how to continue after getting the result. A good way to think about it is that the callback takes on the role of the return statement—except now it's a first-class value of the language.

Let's take a look at a simple example to illustrate the idea. I'll use JavaScript syntax for now. Here's a normal function for adding two numbers:

2.  function add(a, b) { 

5.   return a + b; 

8.  } 

To rewrite the function into CPS, we would need to take a callback and use that instead of `return`.  

2.  function add\_cps(a, b, done) { 

5.   done(a + b); 

8.  } 

The other change comes when we get around to using the function. We can just call a normal function and store the result in a variable:  

2.  var result = add(1, 2); 

5.  // use result here 

Since the CPS version does not return anything useful, this approach does not work. Instead, we give `add_cps` a callback that takes `result` as an argument:

2.  add\_cps(1, 2, function (result) { 

5.   // use result here 

8.  }); 

That's the core of CPS: functions take a callback argument and we bind their result to an argument of a function rather than a normal variable.

One last note: we can extend the CPS transformation to normal values rather than just functions. With functions, instead of returning the result, we take a callback—and we can think of normal values as functions that just return immediately. This means that if we normally have an `x`, in CPS we would have a function that takes a callback and passes `x` into it immediately:

2.  // a variable x in CPS 

5.  function x\_cps(done) { 

8.   done(x); 

11.  } 

Why

The next, and perhaps more interesting question, is: why? After all, CPS seems pretty awkward, especially with JavaScript's more verbose syntax—if we use several CPS functions in a row, we will get a lot of nested functions!

The main reason is that CPS gives us full control over control flow. Normally, control flow is handled by the language: statements are executed one after the other, and we can only jump around using built-in control flow like if-statements, loops, functions, exceptions and return statements. In CPS, we don't have statements one after the other: instead, each "statement" has an explicit function call for the next one. This means that we can do whatever we want with it, including storing it in a variable and using it multiple times, skipping the next statement or following a different continuation entirely.

For example, we can emulate exception handling in CPS by passing around an extra "error" continuation. (With the mnemonic name `raise`.)  

2.  function couldError(raise, done) { 

5.   // call raise on error 

8.  } 

The important part is that `raise` can be used from arbitrarily nested callbacks, and will skip all the ones after it is called—just like the language's built-in `throw` does for normal statements.

By holding onto and passing around callbacks like this, we can implement any sort of non-local control we would like. Since the entire program (in CPS) is sequenced in terms of callbacks rather than statements, this is actually similar in power to a `goto` statement.

CPS is also useful for cooperative multithreading and non-blocking IO. The language can still be single-threaded from the perspective of statements—executed one after the other—and only switch tasks between callbacks. Node.JS is a great example of this style of programming most directly: code doing IO with Node is written in CPS.

If you're curious for more examples in a similar vein, take a look at [By example: Continuation-passing style in JavaScript][1] on Matthew Might's blog. (This article helped me get some of the details in this post correct.) His blog has a bunch of other articles about CPS and continuations—it's probably one of the best places to learn about the subject.

A neat feature of CPS is that all CPS functions are in tail recursive form. This means that, in a language with proper tail calls, CPS completely elides the language's stack: in essence, you end up making your own stack instead of relying on the implicit call stack as you would normally.

As the name implies, CPS is related to first-class continuations in languages like Scheme, which provide an operation called "call with current continuation", often abbreviated as `callCC`. A good way to think about it is that the primitive `callCC` operation in these sorts of languages exposes a callback (ie a continuation) like in CPS without having to write the actual program in the style, giving you the same powerful control flow as described above. Writing our program in CPS lets us express `callCC` without especial language support.

The way `callCC` works is a bit convoluted: it takes in a user-supplied function, to which it passes a continuation as the sole argument. The continuation represents the rest of the expression after the call to `callCC`, letting the user-supplied function jump there whenever it wants—or do other things, like storing the continuation for future use or passing it to some other function or even just returning it directly. (This last trick is a neat idiom for getting access to a continuation directly in the rest of your code.)

I find it easiest to think about `callCC` by looking at what its type would be in a functional language. Here's one version: it takes a function that takes a continuation and runs that function, returning its result. The result type can be anything we want.

2.  callCC :: ((Cont → p) → p) 

This is nice, but how do we really represent a continuation? Well, let's think about what the continuation would look like as a callback in CPS. It would be a function that takes the result of the whole `callCC` expression—a `p`—and return, well, whatever the next expression's type is. This means that `Cont` is a function that takes a `p` and gives us something else, say a `q`:  

2.  callCC :: ((p → q) → p) → p 

  
Now let's take a look at how to express this in CPS. At first blush, we might be tempted to write something like this:  

2.  function callCC(f, done) { 

5.   return f(done); 

8.  } 

However, this isn't quite right. We have to transform everything into CPS, which means that `f` has to take a callback too; instead of returning, we would have to call `done`. Moreover, the procedure we pass into `f` should be in CPS too, wrapping the actual continuation callback. Here's what the correct implementation looks like, with all the relevant callbacks:

2.  function callCC(f, done) { 

5.   f(function (x, done\_f) { done(x) }, done) 

8.  } 

Since we added a bunch of callbacks, it also means that the type of the CPS-transformed `callCC` is different from the normal one. It looks like this:

2.  callCC :: ((p → ((q → r) → r)) → ((p → r) → r)) → ((p → r) → r) 

At first, this is a bit confusing. But let's take a look at what's really going on: we've taken the old `callCC` type and replaced `p` and `q` with functions in the form `((p → r) → r)` whenever they're returned. Hey, that looks familiar: what we've done is instead of taking a `p` or `q` directly, we have taken a callback that expects a `p` or `q`. In other words, we've CPS transformed each value! That is, instead of just returning a `p`, we return a function that looks something like this, a form we've seen above:  

2.  function p\_cps(done) { 

5.   done(p); 

8.  } 

It might be easier to think about if we replaced `((p → r) → r)` with `CPS p`:  

2.  callCC :: ((p → CPS q) → CPS p) → CPS p 

Now it looks pretty similar to our non-CPS `callCC` above. Hopefully this helps you untangle exactly how the actual CPS implementation of `callCC` works.

Compilation

CPS is also used as an intermediate representations in certain compilers, most notably for functional languages as an alternative to single static assignment (SSA). Any program can be systematically translated to CPS, which lets us express all the control flow in the program uniformly making static analysis and optimization simpler. Matthew Might's blog again has a great article about this: [How to compile with continuations][2]. If you're even more curious, you could take a look at Appel's Compiling with Continuations, a classical book on the subject (which, admittedly, I haven't read myself).

A particularly neat, albeit somewhat non-standard, use of CPS can be found in the Chicken Scheme compiler. The entire Scheme program is transformed into CPS before being translated to C function calls. The C function calls never return, which lets Chicken Scheme use the C stack as the first generation of the Scheme heap, simplifying the runtime and providing very fast heap allocations (just incrementing the stack pointer). You can read more about this on their website (appropriately at [call-cc.org][3]): [A guide to the CHICKEN compilation process][4].

Logic

CPS also interacts in an interesting way with formal logic through the Curry-Howard isomorphism. In particular, it relates to the difference between classical logic and constructive logic.

The distinction between the two kinds of logic comes down to the law of the excluded middle, which states that P∨¬P—every proposition is either true or its negation is true. This is an axiom in classical logic but not always true in constructive logic. Additionally, it turns out that the law of the excluded middle is equivalent to Pierces Law (((P→Q)→P)→P), so if we have a proof of this, we have a proof of the law of the excluded middle. (Here's a [nice proof][5] of the equivalence.)

Hey, that law looks familiar! It's the type of the normal (ie non-CPS) `callCC` we talked about earlier:

2.  callCC :: ((p → q) → p) → p 

Immediately, this gives us an interesting conclusion: including a primitive `callCC` in your language changes the logic it corresponds to from a constructive system to a classical one.

However, many languages—like the standard typed λ calculi—do not include a primitive `callCC`, and correspond to constructive rather than classical logics. However, as we saw, we can implement an equivalent for `callCC` if the program is first transformed into CPS. Remember our typing abstraction earlier: the CPS transform turned normal types `a` into `CPS a`. This means that we can embed classical logic into constructive logic by looking at propositions in the form `CPS a` instead of directly looking for `a`.

`CPS a` expands to `forall r. (a → r) → r`. We can replace `r` with any type we want, so lets replace it with `⊥`—the empty type which corresponds to the proposition "false". This gives us `(a → ⊥) → ⊥`.

What does the proposition `(a → ⊥)` mean? It means that `a` is false—since we can't construct a value of type `⊥`, and `a → ⊥` tells us we could, if we had a value of type `a`, `a` has to be empty itself. In constructive logic, `a → ⊥` is the way we write negation (`¬a`), which is not a primitive in the system. Using this translation, we get another way of writing out `CPS a`: `¬¬a`.

In other words: CPS corresponds to double negation. It's important to note that in constructive logic, ¬¬x is not the same as x—the law of double negation elimination (¬¬P→P) is yet another axiom equivalent to the law of the excluded middle and Pierce's law. (Here is [a nice proof of this equivalence][6].)

So, from the perspective of logic, the CPS transformation algorithm is a proof of "[Double-negation translation][7]", which tells us that for every classical proof of P, we have a constructive proof of ¬¬P.

For a much more thorough treatment (which differs slightly in some details), take a look at ["A Formulas-as-Types Notion of Control"][8].

Monads

CPS is related in interesting ways to monads and the monadic style of functional programming.

As a quick intuition about why the two styles are related, take a look the bind operator for monads, which is the most often used operator to sequence monadic computations:  

2.  (>>=) :: m a → (a → m b) → m b 

  
The way you use it is taking a value and passing it into another function... which sounds a lot like taking a value and passing it into a callback. At heart, the bind operator plays a very similar role to CPS in terms of making control flow first class and therefore more flexible and expressive.

In fact, we can neatly wrap up a CPS transform into a monad itself. A good way of thinking about this is in terms of `¬a`, which is syntactic sugar (in pseudocode) for `a → ⊥`:  

2.  instance Monad (¬¬) where 

5.   return :: a → ¬¬a 

8.   (>>=)  :: ¬¬a → (a → ¬¬b) → ¬¬b 

in this instance, the type of `>>=` is a bit hard to think about, so it's easier to look at `join` and `fmap`, which can be composed to define `>>=`:  

2.  join :: ¬¬(¬¬a) → ¬¬a 

5.  fmap :: (a → b) → (¬¬a → ¬¬b) 

  
It should be easy to verify that all of these are true propositions in constructive logic. (At least I hope it's easy to verify, because I don't want to check it myself :P.)

For a bit more generality, lets allow ourselves to replace the `⊥` type with anything else, giving us the following `CPS` type:

2.  data CPS r a = CPS ((a → r) → r) 

A value of `CPS r a` is just like the CPS-transformed values we talked about before:

2.  function x\_cps(done) { 

5.   done(x) 

8.  } 

  
or, in Haskell syntax:  

2.  x\_cps = CPS (\\ done → done x) 

With this type in mind, let's take a look at how to implement `return`, `fmap` and `join`.

`return` takes a normal value (`x`) and wraps it in a CPS value expecting a callback, just like `x_cps` above:

2.  return x = CPS (\\ done → done x) 

`fmap` takes a normal function and maps it over a value that's already in CPS form. We do this by creating a new `CPS` value. We then call the input cps value with a callback that first applies `f` and then calls the new `CPS` value's callback.

2.  fmap f (CPS x\_cps) = CPS (\\ done → x\_cps (\\ x → done (f x))) 

Finally, `join` flattens a nested CPS value, by creating a callback that calls both of the input callbacks:

2.  join (CPS outer) = CPS (\\ done → outer (\\ (CPS inner) → inner done)) 

We could always implement `>>=` in terms of `join` and `fmap`, but I think it's also worth going through its definition as well. We have a CPS value as an input, as well as a function that produces a CPS value; we need to create a single value equivalent to passing the result of the input into the function:  

2.  CPS input >>== f = 

5.   CPS (\\ done → input (\\ x → let CPS res = f x in res done)) 

This code is hard to understand just by reading about it; I highly suggest playing around with it in `ghci` to get a sense of how it typechecks and works.

Once we have our `CPS` monad, we can write code in CPS form using do-notation—which looks just like normal code! All the callbacks are handled implicitly internally, giving us a more readable program with the flexibility CPS gives us. We can also define `callCC` using this and have all the same fun that our Scheme friends do with their `callCC`. The [Haskell wikibook chapter on CPS][9] has some examples of this in action.

The `CPS` monad actually exists in the Haskell standard library as `Cont`, but I like the `CPS` name better.

An interesting note is that if we only had do-notation for `CPS`, we could still use the notation for other monads with a couple of helper functions. This is explained well in sigfpe's blog post ["The Mother of all Monads"][10].

One particular choice we've taken with `CPS` is making the result of the continuation—`r` from `CPS r a`—part of the type. This gives us some additional flexibility but also lets us do things that may not be desirable. For example, if we know that `r` is `Int`, we can just return a `0` instead of using the callback that's passed in. A solution to this problem is to make `CPS` universally quantified over all possible types `r`:  

2.  data CPS a = CPS (forall r. (a → r) → r) 

Since we don't know what `r` is, we can't do anything but call the callback at the end. This leads to the `Codensity` monad, which can be used to improve the performance of computations using other monads by reassociating the bind operations. Edward Kmett has a series of posts on this that are worth reading: [Free Monads for Less (Part 1 of 3): Codensity][11].

Anyhow, there's the primer on most of what I know about CPS. It's one of the most fascinating ideas in computer science and well worth playing around with.

[1]: http://matt.might.net/articles/by-example-continuation-passing-style/ "matt.might.net"
[2]: http://matt.might.net/articles/cps-conversion/ "matt.might.net"
[3]: http://call-cc.org/ "call-cc.org"
[4]: http://wiki.call-cc.org/chicken-compilation-process "wiki.call-cc.org"
[5]: http://math.stackexchange.com/questions/447098/why-peirces-law-implies-law-of-excluded-middle "math.stackexchange.com"
[6]: https://www.proofwiki.org/wiki/Double_Negation_Elimination_implies_Law_of_Excluded_Middle "www.proofwiki.org"
[7]: http://en.wikipedia.org/wiki/Double-negation_translation "en.wikipedia.org"
[8]: http://www.cs.umd.edu/class/fall2014/cmsc631/papers/griffin-callcc.pdf "www.cs.umd.edu"
[9]: http://en.wikibooks.org/wiki/Haskell/Continuation_passing_style "en.wikibooks.org"
[10]: http://blog.sigfpe.com/2008/12/mother-of-all-monads.html "blog.sigfpe.com"
[11]: http://comonad.com/reader/2011/free-monads-for-less/ "comonad.com"
