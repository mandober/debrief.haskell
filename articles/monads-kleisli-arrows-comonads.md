---
downloaded:       2022-01-03
page-url:         http://blog.sigfpe.com/2006/06/monads-kleisli-arrows-comonads-and.html
page-title:       A Neighborhood of Infinity: Monads, Kleisli Arrows, Comonads and other Rambling Thoughts
article-title:    Monads, Kleisli Arrows, Comonads and other Rambling Thoughts
---
# A Neighborhood of Infinity: Monads, Kleisli Arrows, Comonads and other Rambling Thoughts

(Below I use 'arrow' to mean an arrow in a category (or an ordinary Haskell function) but an Arrow (with capital A) is one of the objects defined by Hughes. I'm not really going to say much about Arrows.)
(Below I use 'arrow' to mean an arrow in a category (or an ordinary Haskell function) but an [Arrow][1] (with capital A) is one of the objects defined by Hughes. I'm not really going to say much about Arrows.)

A while back I looked at a [paper][2] on comonads and streams but couldn't really see what comonads were offering. Just recently, I was thinking about a design for a dataflow language for microcontrollers and found that Arrows captured some of the functionality I wanted. But after writing some Haskell code I realised that I was writing the same 'glue' over and over again. So it was becoming clear that arrows were too general and I needed a class that filled in the glue for me automatically. I wrote the code and it looked familiar. I realised that I had rediscovered comonads and they now seemed entirely natural.

First, let me review monads. There are countless web sites that purport to be introductions to monads but my way of looking at them seems a little different to most of those accounts. (It's not in any way unusual, just not in the beginner's guides.) I like to think in terms of Kleisli arrows. I find this perspective unifies the different applications of monads such as encapsulating side effects or doing simple logic programming using the list monad.

A Kleisli arrow is simply an arrow (ie. in Haskell, a function) a→m b where m is a monad. What monads allow you to do is compose these things. So given f::a→m b and g::b→m c there is an arrow a→m c. To me this is the raison d'être of monads but as far as I can see, the standard interface defined in Control.Monad doesn't actually provide a name for this composition function. (Though the [Control.Arrow][3] interface does provide a Kleisli arrow composition function.)

So here's why I like to think in terms of Kleisli arrows: Consider what is almost the prototypical example of a Haskell monad - the [Writer][4] monad. Suppose you have something that is conceptually a function f::a→b but you want it to output a list of items to a log as a side effect. In a functional programming language there is no way out - you're pretty well forced to return the thing you want to log along with the object of type b. If your log is a list of objects of type d, then you need to modify f to return an object of type (b,\[d\]) instead of b. But here we have a catch, if we have f::a→(b,\[d\]) and g::b→(c,\[d\]) (ie. conceptually a function of type b→c producing a log of type \[d\]) then we want to compose these things. But the argument to g is no longer the return type of f. We need some plumbing to concatenate these functions. In this case the plumbing needs to take the output of f, split off the log keeping it for later, pass the remainder to g, and then concatenate the log from f before the log of g. And this is what monads do, they provide the plumbing. (If you knew nothing about monads and wrote the obvious code to plumb these things together, concatenating the logs, probably the first thing you wrote would look just like part of the definition of the Writer monad, except the Writer monad is generalised to monoids instead of lists.)

Let's work through the details of composing Kleisli arrows: we want to compose f::a→m b and g::b→m c. The obvious thing to do is add a gluing map between them m b→b. But that's uninteresting as it just throws away the fanciness. Instead we use the fact that m is a functor (part of the mathematician's definition of monad) to lift g to a function m b→m (m c). This now composes nicely with f but the catch is that we end up with something twice as fancy. However, part of the definition of monads is that there is a map m (m c)→m c. (Twice as fancy is still just fancy.) And that can now be used to finish off the composition.

Consider another example of a monad, the [list monad][5]. The idea is that we want a function to return multiple values, not just one. So instead of f::a→b we have f::a→\[b\]. But suppose we have another one of these things, g::b→\[c\]. How do we concatenate these? Conceptually what we want to do is run g on each of the return values of f in turn and then collect up all of the results in one big list. This is exactly what the list monad does, it provides the plumbing to concatenate functions in this way.

In both cases we have f::a→m b and g::b→m c and we get a function a→m c. Or more informally, monads give a way to compose functions that map ordinary types to fancy types providing the glue that allows the fancy output of one function to connect to the on-fancy input of the next. And I like to view things this way because a functional program is a concatenation of lots of functions - so it's natural to think about monads as simply a new way of building programs by concatenation of functions.

Anyway, I was thinking about stream functions. A stream function from a to b is simply a function \[a\]→\[b\]. (Strictly speaking we're only considering infinite lists - streams.) This doesn't quite fit the pattern of non-fancy→fancy, it's more like fancy→fancy. And that's what the Arrow interface allows us to do. But I'm not going to talk about Arrows here except to say that I started using them to write some stream code. But then I noticed that I was only interested in *causal* stream functions. This is a function where the nth element of the output depends only on the first n values of the input. This pattern fits many types of processing in dataflow applications such as audio processing. In order to compute the nth element of the output of a causal f::\[a\]→\[b\] we need only compute a function f::\[a\]→b. To compute the entire stream we repeatedly use this function to generate each element in turn. So, for example, if the input is the stream \[x1,x1,x2,...\] then the output is \[f \[x1\],f\[x1,x2\],f\[x1,x2,x3\],...\]. In other words a stream function is really a function f::\[a\]→b but we need special glue to concatenate them because the nth element of the output concatenation should look like g \[f \[x1\],f\[x1,x2\],f\[x1,x2,x3\],...,f \[x1,...,xn\]\].

If you followed that then you may have noticed the pattern. We want to compose two functions that map fancy types to non-fancy types to produce a new function that maps fancy types to non-fancy types. It's the exact opposite of what monads do. And this is exactly what comonads are about: they are the correct abstraction to use when writing glue for fancy-to-non-fancy functions. It all seems so natural I'm astonished to find that [Control.Comonad][6] *isn't* a part of the standard Haskell distributions.

Let's look at the details more closely. Let's still use m to represent a comonad. We need to compose f::m a→b and g::m b→c. m is a functor (by definition of comonad) so we can lift f to a function of type m (m a)→m b. This composes directly with g. And to finish it off we use the function m a→m (m a) provided by the definition of a comonad.

And in even more detail for the case of (lists considered as) streams. The lift operation is simply given by the usual map function. You lift a function f by applying it to each element in the stream in turn and returning the stream of results. The function m a→m (m a) is more interesting. It maps \[x1,x2,x3,...\] to \[\[x1\],\[x1,x2\],\[x1,x2,x3\],...\]. In other words it maps a stream to its list of 'histories'. My use of the loaded word 'history' should be a hint about where causality comes in. If we lift a function f::\[a\]→b to act on this list of histories we get \[f \[x1\],f\[x1,x2\],f\[x1,x2,x3\],...\]. In other words, a comonad gives exactly what we need to work with streams.

Anyway, one of the cool things about monads is the special syntactic sugar provided by Haskell that allows us to write what looks like imperative code even though it's actually functional. I've been trying to figure out what similar sugar might look like for comonads. But I can't quite figure it out. I can see roughly what it'd look like. You'd be able to write lines of code like  

```
codo    b <- 2*(head a) -- double the volume    c <- 0.5*head b+0.5*head (tail b) -- simple FIR filter
```

  
so that even though it 'looks' like b is merely twice the head of a, the compiler would produce the appropriate glue to make b actuually be the stream whose head is 2\*(head a). In fact, you can do something a bit like this using Arrow syntax. But I can't quite fill in the details in such a way that it nicely parallels the syntax for monads.

(Silly me...I think I've just figured it out now. The 'codo' block is different from a 'do' block because it needs to define a coKleisli arrow, not an element of the comonad. Hmmm...)

And just some final words: I believe Arrows are the wrong approach to [functional reactive programming][7]. Comonads are *much* more appropriate because they model causal functions much more closely - and causal stream functions are what FRP is all about.

[1]: http://www.haskell.org/arrows/
[2]: http://lambda-the-ultimate.org/node/988
[3]: http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Arrow.html
[4]: http://cvs.haskell.org/Hugs/pages/libraries/mtl/Control-Monad-Writer.html
[5]: http://cvs.haskell.org/Hugs/pages/libraries/base/Data-List.html
[6]: http://www.eyrie.org/~zednenem/2004/hsce/Control.Comonad.html
[7]: http://www.haskell.org/frp/
