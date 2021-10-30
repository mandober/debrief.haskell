# Streams and Monad Laws

> by Nicolas Wu

by Nicolas Wu

* * *

Posted on 21 October 2010

Tags: [Haskell](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20200516190427/http://zenzike.com/tags/Haskell.html), [Bogus](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20200516190427/http://zenzike.com/tags/Bogus.html)

* * *

> Warning: The proof that this definition of `>>=` satisfies the monad laws is actually bogus, since it uses induction rather than coinduction. Thanks to Jeremy Gibbons for pointing this out. I hope to post another article that spells out the details at some point.

When I first encountered the definition of monadic binding for streams, I wondered if there might be alternative definitions. This article is about how the two unfair versions of monadic bind do not satisfy the monadic laws, and how the fair definition does.

For convenience, streams are represented as lists in this article, since this provides us with all the list operators on our type:

For those of you who don’t know this off the top of your head, here’s the definition of the monad (ignoring the fact that we can’t write a class instance of a type synonym):

    instance Monad Stream where
      return x = repeat
      xs >>= f = diag (map f xs)

Here `repeat` and `diag` have the following definitions:

    diag (xs:xss) = head xs : diag (map tail xss)

In our definition of `xs >>= f`, `f` has type `a -> Stream b`, so when this is applied to each of the elements in xs, we end up with a stream of streams. The function `diag` takes the diagonal of this stream of streams and returns this as our output.

Though this is somewhat aesthetically pleasing, since every row and column gets proportional representation in the output, I was somewhat bemused as to why this _specific_ choice had to be the right one.

Two unfair alternatives come to mind; you could even just take the first element of each list in an attempt to at least access every stream in the stream once:

    xs >>= f = map head (map f xs)
             = map (head . f) xs

Or alternatively, you might decide to simply take the first stream and return that:

    xs >>= f = head (map f xs)

Both of these are type correct, so which one should we use for the definition of `(>>=)`?

Monad Laws
----------

The monad laws come to the rescue! Every monad must obey the three monadic laws, otherwise it’s not a valid monad. The laws are that for any monad we must have:

    return x >>= f  =  f x                      -- [left identity]
    mx >>= return   =  mx                       -- [right identity]
    (mx >>= f) >>= g = mx >>= (\x -> f x >>= g) -- [transitivity]

Before check our two alternative definitions of `(>>=)` for correctness we’ll first prove a little law that makes our lives a lot easier later on:

    map f (repeat x) = repeat (f x)             --[map-repeat]

Our proof goes like this:

    map f (repeat x)
      == {- repeat -}
    map f (x : repeat x)
      == {- map -}
    f x : map f (repeat x)
      == {- induction hypothesis -}
    f x : repeat (f x)
      == {- repeat -}
    repeat (f x)

Let’s see if our first bogus definition stands up to scrutiny under the judgement of the monadic laws.

### Left Identity

First we’ll show that our first bogus definition doesn’t stand up to the left identity law:

    return x >>= f 
      == 
    map (head . f) (repeat x)
      == {- map-repeat -}
    repeat ((head . f) x)

Oh dear. It seems that our left identity law can’t be proved unless our function `f` is a `repeat (g x)` for some `g`. Clearly this isn’t always going to be the case, for example, consider `succs`:

    succs :: Int -> Stream Int
    succs x = [x+1 ..]

We should probably have guessed this straight away, since it’s obvious that for all `f`:

    map f (repeat x) = repeat (f x)

Okay, so our proof scraps our first suggestion, but what about the second one?

### Right Identity

Actually, our second suggestion passes the first law, but let’s take a look at whether it passes the second law:

    mx >>= return 
      ==
    head (map repeat mx)
      == {- let mx = (x:xs) -}
    head (map repeat (x:xs))
      == {- map -}
    head (repeat x : map repeat xs)
      == {- head -}
    repeat x

Aha! We’ve reduced the LHS of the right identity law to `repeat x`, where `x = head mx`, and it’s pretty obvious that this isn’t always the case, since we would need `mx = repeat x` for all `mx`, and this isn’t the case for `[1..]`.

So look at that, we didn’t even need to think very hard to prove that those two suggestions don’t play well with the monad laws.

What about the real definition of `(>>=)` that uses `diag`? Can we prove that one? I should rather hope so.

Proving Stream Monad Laws
-------------------------

At this point, I think that most readers can stop; what follows is completely routine, and the details aren’t particularly interesting.

For those of us who like calculations, let’s flex our mental muscles and get on with business!

First, another reminder of our definition: > xs >>= f = diag (map f xs) > where diag (xs:xss) = head xs : diag (map tail xss)

Before getting into the meat of the proof, we prove a small property that helps us a little:

    diag (repeat xs) = xs    --[diag-repeat]

Intuitively, it seems right, so here goes the inductive proof:

    diag (repeat xs)
      == {- repeat -}
    diag (xs : repeat xs)
      == {- diag -}
    head xs : diag (map tail (repeat xs))
      == {- map-repeat -}
    head xs : diag (repeat (tail xs))
      == {- induction hypothesis -}
    head xs : tail xs
      == {- (:)-head-tail -}
    xs

This sets us up nicely for the monad law proofs.

### Left Identity

The left identity law comes quite naturally from our `diag-repeat` property:

    return x >>= f
      ==
    diag (map f (repeat x))
      == {- map-repeat -}
    diag (repeat (f x))
      == {- diag-repeat -}
    f x

### Right Identity

The right identity drops out naturally too:

    mx >>= return
      ==
    diag (map return (repeat mx))
      == {- map-repeat -}
    diag (repeat (return mx))
      == {- diag-repeat -}
    return mx

### Transitivity

Now the toughie. Sadly, this one takes a lot more work to prove, but I’m sure there’s got to be a shorter version. What am I missing?

So, here we go, starting with the LHS.

    (mx >>= f) >>= g
      == 
    diag (map g (diag (map f mx)))
      == {- let mx = (y:ys) -}
    diag (map g (diag (map f (y:ys))))
      == {- map -}
    diag (map g (diag (f y : map f ys)))
      == {- diag -}
    diag (map g (head (f y) : diag (map tail (map f ys))))
      == {- (.), map-map -}
    diag (map g ((head . f) y : diag (map (tail . f) ys)))
      == {- (>>=) -}
    diag (map g ((head . f) y : ys >>= (tail . f)))
      == {- map -}
    diag (g ((head . f) y) : map g (ys >>= (tail . f)))
      == {- (.) -}
    diag ((g . head . f) y : map g (ys >>= (tail . f)))
      == {- diag -}
    head ((g . head . f) y) : diag (map tail (map g (ys >>= (tail . f))))
      == {- (.), map-map -}
    (head . g . head . f) y : diag (map (tail . g) (ys >>= (tail . f)))
      == {- (>>=) -}
    (head . g . head . f) y : (ys >>= (tail . f)) >>= (tail . g)

Now that this has been pretty simplified, we hack away at the RHS and see what we end up with. For this part we’ll be using a funny little rule that helps with reduction

    map f xs = (f . head) xs : map f (tail xs)   --[map-head-tail]

    mx >>= (\x -> f x >>= g)
      == 
    diag (map (\x -> f x >>= g) mx)
      == {- let mx = (y:ys) -}
    diag (map (\x -> f x >>= g) (y:ys))
      == {- map -}
    diag ((\x -> f x >>= g) y : map (\x -> f x >>= g) ys)
      == {- \-ap -}
    diag (f y >>= g : map (\x -> f x >>= g) ys)
      == {- diag -}
    head (f y >>= g) : diag (map tail (map (\x -> f x >>= g) ys))
      == {- map-map -}
    head (f y >>= g) : diag (map (tail . (\x -> f x >>= g)) ys)
      == {- (>>=) -} 
    head (f y >>= g) : ys >>= (tail . \x -> f x >>= g)
      == {- (>>=) -}
    head (diag (map g (f y))) : ys >>= (tail . \x -> f x >>= g)
      == {- map-head-tail -}
    head (diag ((g . head) (f y) : map g (tail (f y)))) : ys >>= (tail . \x -> f x >>= g)
      == {- (.) -}
    head (diag ((g . head . f) y : map g ((tail . f) y))) : ys >>= (tail . \x -> f x >>= g)
      == {- diag -}
    head (head ((g . head . f) y) : map tail (map g ((tail . f) y))) : ys >>= (tail . \x -> f x >>= g)
      == {- head -}
    head ((g . head . f) y) : ys >>= (tail . \x -> f x >>= g)
      == {- (.) -}
    (head . g . head . f) y : ys >>= (tail . \x -> f x >>= g)

If you’re still reading, you’ll notice that we’re nearly done. We just need to prove that the tails of the expressions we have match up, since the heads are already there.

That is, we need to show that:

    (ys >>= (tail . f)) >>= (tail . g) = ys >>= (tail . \x -> f x >>= g)

So here we go:

    xs >>= (tail . \y -> f y >>= g)
      == {- (.)-\ -}
    xs >>= (\y -> tail (f y >>= g))
      == {- assume [1] -}
    xs >>= (\y -> (tail . f) y >>= (tail . g))
      == {- induction hypothesis -}
    (xs >>= (tail . f)) >>= (tail . g)

This makes use of the assumption:

    tail (f y >>= g) = (tail . f) y >>= (tail . g)   --[1]

We finally prove this to finish up:

    tail (f y >>= g) 
      == {- (>>=) -}
    tail (diag (map g (f y)))
      == {- map-head-tail -}
    tail (diag ((g . head) (f y) : map g (tail (f y))))
      == {- (.) -}
    tail (diag ((g . head) (f y) : map g ((tail . f) y)))
      == {- diag -}
    tail (head ((g. head) (f y)) : diag (map tail (map g ((tail . f) y))))
      == {- tail -}
    diag (map tail (map g ((tail . f) y)))
      == {- map-map -}
    diag (map (tail . g) ((tail . f) y))
      == {- (>>=) -}
    (tail . f) y >>= (tail . g)

Finally finished! I wish that the proof could be somewhat shorter; this is my first attempt, so I wouldn’t be surprised if there’s a short-cut that I missed. If you see somewhere that can be improved, then let me know!


[Source](https://web.archive.org/web/20200516190427/http://zenzike.com/posts/2010-10-21-streams-and-monad-laws)