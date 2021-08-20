# Four Tips for New Haskell Programmers

> The Haskell programming language is widely considered to have a fairly steep learning curve--at least compared with mainstream languages.  I...

The Haskell programming language is widely considered to have a fairly steep learning curve--at least compared with mainstream languages.  In my experience with Haskell and specifically helping newcomers I've noticed a few common issues that seem to come up again and again.  Some of these issues might be more avoidable if the Haskell community did a better job communicating them.  Four points I have noticed are:

1.  Read Haddock API docs
2.  Pay attention to type class instances
3.  Learn about kinds
4.  Learn monad transformers

#### Read Haddock API Docs

I mention this point at the risk of stating the obvious.  If you are going to become a proficient Haskell programmer, it's absolutely essential that you get used to reading the API docs for the packages you use.  I often hear newcomers ask for tutorials demonstrating how to use packages.  Our community would definitely be better off with tutorials for every package, but it would also be better if newcomers would pay more attention to the API docs.  Now I know you're probably thinking, "yeah, but most packages are poorly documented."  That is true, but Haskell's type signatures tell you a lot more about a function than other languages.  For instance, consider the following example:

`readChan :: Chan a -> IO a`

A Chan is essentially a queue.  You can put values in and get them out in FIFO order.  When you are trying to understand the above function, one of the first things you might wonder about it is whether it blocks or not.  But if you think about it a little more, you'll realize that this function **has** to block.  If it didn't block, then there might not be a value and there would be no way to return something of type a (because Haskell has no null pointers).  A non-blocking version would have to have a type signature like this:

`tryReadChan :: Chan -> IO (Maybe a)`

So even with no prose documentation added at all, you can still learn a lot from the type signatures.  This is more significant in Haskell than other languages because of Haskell's purity and its strong type system.  Also, I would recommend that you bookmark the url "http://hackage.haskell.org/package/".  Actually, better yet, type it into your web browser frequently so it is the first thing given to you by the autocorrect when you start typing "hack...".  Once you auto-complete that url, you can just type the name of the package you are looking for and you'll immediately get to the most recent API docs for that package.  It's way faster than hitting control-f and searching through the package list on that page.

Also, bookmark [http://www.haskell.org/ghc/docs/latest/html/libraries/index.html](http://www.haskell.org/ghc/docs/latest/html/libraries/index.html) because it has links to documentation for the core libraries.

#### Pay Attention to Type Class Instances

I can't emphasize this point enough.  One of the most common questions I get from beginners is how to run an IO function in some random monad.  This information is trivially visible in the API docs, but for some reason beginners never seem to notice.  Take for example the [Snap monad](http://hackage.haskell.org/packages/archive/snap-core/0.8.0.1/doc/html/Snap-Core.html#t:Snap).  Go ahead, click that link and look at the documentation.  The clue that you can run IO actions inside that monad is tucked away near the end of the "Instances" block.  It's one little innocuous line `MonadIO Snap`.  Newcomers might not be familiar with MonadIO, but if they click the link they'll see that it defines one function `liftIO :: IO a -> m a` that converts any function in the IO monad to a function in the current m monad.  Those instance lines contain a treasure trove of information.  Don't neglect them.

#### Learn About Kinds

In Haskell all values have a type.  Analogously, all types have a kind.  This topic is often not brought up until later, but I feel that understanding kinds gives beginners a big advantage in understanding type signatures.  I'm not going to go into it in detail here, but I think it's an important concept that is too often ignored.

#### Learn About Monad Transformers

This is another one of those topics that is often put off until later.  When I was learning monads, I distinctly remember getting the impression that monad transformers were a much more complicated beast and that I didn't need to learn about them at that time.  But when I finally did learn about monad transformers a lot of things became clearer for me.  Also, monad transformers are used all the time in real world applications.  Transformers are a much simpler concept than monads, especially if you know about kinds.  There's no reason not to learn them at the same time.


[Source](http://softwaresimply.blogspot.com/2012/04/four-tips-for-new-haskell-programmers.html)