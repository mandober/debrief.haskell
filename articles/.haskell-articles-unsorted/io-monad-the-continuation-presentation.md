# IO Monad: The Continuation Presentation

> I’ve been programming Haskell for several years now, and I have not yet partaken in the rite of passage: writing a monad tutorial. Well, finally I can come of age, for I am giving a monads tu…

I’ve been programming Haskell for several years now, and I have not yet partaken in the rite of passage: writing a monad tutorial. Well, finally I can come of age, for I am giving a monads tutorial talk to the CU ACM group next Thursday.

I’m thinking of starting with the IO monad, since that was the motivation for adding monads to Haskell in the first place. It also gives people something concrete to think about before I segue into more abstract monads. But I don’t want to use GHC’s IO monad as an example:

> newtype IO a = IO (RealWorld -> (a, RealWorld))

Because it’s too “low-level”. I barely want to talk about laziness, so a construction which is just there to force evaluation order seems kind of hacky. If you don’t think of it as a construction to force evaluation order, then it is too abstract: what do you mean a function which takes the whole world as an argument? Also, it uses a function as its primary data structure, and the talk will be given to people who are still a little uncomfortable with first-class functions (it will have to introduce them at some point though, but I want to ease into it).

My current plan is to eventually get to the GADT form:

> data IO a where
>     PutChar :: Char -> IO ()
>     GetChar :: IO Char
>     Bind    :: IO a -> (a -> IO b) -> IO b
>     Return  :: a -> IO a

And then focus on Bind and Return as a segue into the idea of a monad. But yes, you heard me right, to explain an advanced feature, I’m going to use an _even more_ advanced feature! That’s only because I find GADTs very intuitive; in fact, when I was first learning Haskell on the Pugs project, seeing a GADT helped me to understand normal ADTs.

But of course I must ground this GADT. Bind is a weird-looking function, how the hell did I come up with that? I want to arrive at it by necessity, so I’m going to start with a simpler ADT form. I’ll introduce purely-functional IO as a data structure, initially looking like this:

> data IOTree
>     = PutChar Char
>     | IOTree \`Then\` IOTree
>     | NoOp

And then realize that we have no way to do input. At this point I expected to have to switch to the polymorphic variant to handle input, but then this hit me:

> data IOTree
>     = PutChar Char
>     | IOTree \`Then\` IOTree
>     | NoOp
>     **| GetChar (Char -> IOTree)**

To my surprise, with this I was capable of writing all the IO functions that I could using the polymorphic variant, but with some slightly strange-looking type signatures.

> getLine :: (String -> IOTree) -> IOTree
> getLine f = getLine' id
>     where
>     getLine' s = GetChar $ \\ch ->
>         case ch of
>             '\\n' -> f (s "")
>             \_ -> getLine' (s . (ch :))

And then I saw it. That type signature looks awfully familar. If I replace the types with variables it’s completely obvious: (a -> r) -> r. I’m working _explicitly_ in the continuation monad. Sure enough, I can build a monad out of IOTree.

> newtype IO a = IO { runIO :: (a -> IOTree) -> IOTree }
> instance Monad IO where
>     return x = IO ($ x)
>     IO m >>= f = IO $ \\c -> m (\\a -> runIO (f a) c)

That’s a unique presentation of the IO monad that I’ve never seen before. It feels “smooth”, there are no huge leaps anywhere except for where the hell we came up with the idea of a monad. But that reason is enough for me not to want to use it. Another reason against it is that the implementation of \>>= was not at all obvious to _me_ (it took some typechecker runs), so how are FP noobs going to understand it?

So I’m still not sure how I’m going to motivate the polymorphic form. Probably something like introducing the idea of IO “holding” a value, and then we need a variable for the type of value it holds. That would also allow me to introduce Functors, which I want to do as a “monad indicator”; i.e. functors are pretty easy to intuitively spot, and there’s a decent chance that if you have a functor, you have a monad.

Stepping away from pedagogy for a moment, I find it very interesting what continuation passing style did to this example. It seems that CPS is a nice way to build up data structures “procedurally” without hand-crafting a different monad for each one. For instance, Writer can be naturally specified as:

> type Writer w a = Cont w a
> tell :: (Monoid w) => w -> Writer w ()
> tell w = Cont $ \\c -> w \`mappend\` c ()

Anyway, I’d love to hear any advice anyone has about presenting the IO monad, or monads in general. The focus is not so much “so you’re trying to learn Haskell but are perpexed by monads, here’s how to understand them”, but more “you’ve barely heard of Haskell, here’s why monads are awesome”. I plan to talk about IO, List, Suspend\[1\], and close with STM (where Haskell rocks everyone else’s world).

[1] The Suspend monad is what Fregl's Event monad is built on. It’s a type of coroutine which only consumes values:

> data Suspend v a
>     = Return a
>     | Suspend (v -> Suspend v a)


[Source](https://lukepalmer.wordpress.com/2008/03/29/io-monad-the-continuation-presentation/)
