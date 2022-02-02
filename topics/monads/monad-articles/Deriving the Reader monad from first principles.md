---
downloaded:       2021-12-12
page-url:         https://williamyaoh.com/posts/2020-07-19-deriving-reader-monad.html
page-title:       Deriving the Reader monad from first principles | William Yao
article-title:    Deriving the Reader monad from first principles
---
# Deriving the Reader monad from first principles | William Yao

Next on our tour of the RWS trio: the Reader monad. If you haven’t read the previous post on the State monad, this post should be understandable even without having read it, though I do recommend going back and reading about State first.
[« Previous post][1]   [Next post »][2]

Next on our tour of the RWS trio: the Reader monad. If you haven’t read the [previous post on the State monad][3], this post should be understandable even without having read it, though I do recommend going back and reading about State first.

Once again, these posts will be targeted towards people who already have some knowledge about monads, but perhaps are a little fuzzy on specific instances. If you didn’t have the Reader monad already implemented for you, how would you go about deriving it from first principles?

Just like the previous post, this article is designed around you actively working along with the code samples and examples. So make sure you have an editor and REPL handy. If you’re already itching to get implementing and don’t care about the detailed explanations, here’s the skinny version: given the following datatype, implement the Functor, Applicative, and Monad instances for `Reader`, as well as the listed function signatures.

```
newtype Reader cfg a = Reader { runReader :: cfg -> a }

ask  :: Reader cfg cfg
asks :: (cfg -> a) -> Reader cfg a
```

For anyone who wants a little more explanation, let’s dive into it, shall we?

---

Let’s say that your code needs some kind of global runtime config. A typical scenario might be giving your code database connection strings that differ between your different deployment environments, and thus the easiest thing is to pass them around at runtime.

Given that we’ve already looked at the `State` monad, there’s no reason you couldn’t just use that (i.e. some kind of global variable) to solve this problem. But doing that is a bit unsatisfying, because we can already see one typical characteristic of configs like this: they’re read-only. You set them once at the beginnning of your program, then the value stays the same throughout the rest of the program’s lifetime. So we’d rather not open ourselves up to potential problems by allowing our config to be mutated.

How would we go about providing this kind of “read-only global” in Haskell? Just like with our exploration of `State`, let’s see if we can’t just solve the problem longhand using normal functions. If there *is* a pattern or potential abstraction, the easiest way to see it is to do things the boilerplate way first.

What if we just added an extra parameter to all of our functions? They don’t need to make any updates to our config, so that seems like it could work. Any functions that we need to call, we’ll simply pass the config we’ve received downwards.

So maybe we end up with something like this. The exact details of what’s inside our “config” aren’t particularly important, but let’s keep it simple. Let’s say we’re doing some very basic [A/B tests][4], and we want a boolean for each test to know whether it’s enabled or not.

```
import Data.Char as Char

-- Our config data. We're very particular about certain
-- letters of the alphabet, see.
data ABConfig = ABConfig
  { don'tUseLetterE :: Bool
  , don'tUseLetterL :: Bool
  }

-- Uppercase the string, obeying current tests.
toUpperStr :: ABConfig -> String -> String
toUpperStr cfg str =
  filter passesFilters (fmap Char.toUpper str)
  where filters :: [Char -> Bool]
        filters =
          [ if don'tUseLetterE cfg then (/= 'E') else const True
          , if don'tUseLetterL cfg then (/= 'L') else const True
          ]

        passesFilters :: Char -> Bool
        passesFilters c = all (\f -> f c) filters
```

Notice how if we remove the `ABConfig` parameter from our function type, we get a function of type `String -> String`, which is exactly what you’d expect from a normal string uppercase function.

We can then use this uppercase function in other definitions:

```
welcomeMessage :: ABConfig -> String -> String -> String
welcomeMessage cfg motd username =
  "Welcome, " ++
  toUpperStr cfg username ++
  "! Message of the day: " ++
  toUpperStr cfg motd
```

__Exercise 1__: Stop here and implement a function `fullName` that takes in a first name, last name, and nickname, and outputs their “full” name, with the nickname in-between the other two names, in quotes. Use `toUpperStr` to transform all three name components.

For example, `fullName someCfg "Cinder" "Chonk Boy" "Block"` should output the string `"CINDR \"CHONK BOY\" BOCK"`, assuming the config disables both E and L.

Simple enough; all parts of our code can now read the config data they need. As long as we add an extra parameter to all our functions, this solution scales indefinitely. Any part of our code, whether high-level or low-level, can now implement a test based on our config. And if we need more tests, all we have to do is add extra fields to our config type.

Truthfully, function parameters alone are a pretty decent solution to our initial problem of needing a global config. Sure, it’s a bit tedious to have to modify all our function types and manually pass an extra parameter on every function call, but it’s not *that* bad. If I were to rate the annoyingness of this solution from 1 to 10, it’s probably around a 3; kind of irritating, but basically tolerable. Certainly not to the degree of boilerplate we saw with `State`, where we had to practically mangle our functions into unintelligibility for our longhand solution.

But it’s still *an* annoyance. It doesn’t feel like we should have to manually pass our “global” through, especially since most of our functions will probably not do anything with it other than pass it to *other* functions. And there’s still the possibility of accidentally mixing up parameter order, if the type of our config coincides with something else a function needs. So let’s see if we can’t do better.

---

Once again, we think of dipping into monads. And once again, we need some datatype to implement our instances for. What should that type be?

Let’s look at the signatures for the functions we’ve written so far. What’s the “core type”, the “essence” of a global config?

```
toUpperStr     :: ABConfig -> String -> String
welcomeMessage :: ABConfig -> String -> String -> String
fullName       :: ABConfig -> String -> String -> String -> String
```

The String parameters don’t seem like they have much to do with our problem, since they’re specific to each function in question. Neither does the String return, so let’s make that abstract. And clearly we need *some* kind of config to be passed in, but its specific type doesn’t matter that much. So in the end, the type we’re left with is:

```
aFunctionWithConfig :: config -> a
```

Which is a bit eyebrow-raising. All that reduction, and we’re left with… basically a function `a -> b`. But more on that later. For now, we’ll roll with this type: the core of a configurable function is, delightfully tautologically, one that takes in a config and produces a value.

---

We’ve got a type. Let’s put it inside a data definition and give it a shot.

```
data Reader cfg a = Reader { runReader :: cfg -> a }
```

Again, we don’t technically know whether this will form a valid monad just by looking at it. All we can do is give it a shot, try to write the instances, and see if what we end up with obeys the monad laws.

But before we dive into implementing, why would doing this transformation and rewriting all our functions to use this datatype even help us in the first place?

Recall the functions that a monad gives you: `return` (or `pure`), and `(>>=)`. Look at their abstract types:

```
return :: Monad m => a -> m a
(>>=)  :: Monad m => m a -> (a -> m b) -> m b
```

How do the types of these functions relate to our actual datatype?

![](https://williamyaoh.com/img/ba44afe0-8812-6408-8290-b1f5ed5b661c-reader-bind-diagram.png)

Our config value has been abstracted away underneath the `m` type parameter in `(>>=)`; as we wanted, the only thing we need to worry about, the only thing we have access to, is the “normal return value” of any functions we call. If we construct our functions using `(>>=)`, the boilerplate of passing the config parameter to each function should disappear.

So *assuming* we correctly implement `(>>=)` and our functor/applicative/monad instances to do that parameter passing for us, we might end up with something like this:

```
toUpperStr :: String -> Reader ABConfig String
toUpperStr str = Reader (\cfg ->
  let filters :: [Char -> Bool]
      filters = [ if don'tUseLetterE cfg then (/= 'E') else const True
                , if don'tUseLetterL cfg then (/= 'L') else const True
                ]
      passesFilters :: Char -> Bool
      passesFilters c = all (\f -> f c) filters
  in filter passesFilters (fmap Char.toUpper str))

welcomeMessage :: String -> String -> Reader ABConfig String
welcomeMessage motd username =
  toUpperStr motd >>= (\upperMOTD ->
    toUpperStr username >>= (\upperUsername ->
      Reader (\_ ->
        "Welcome, " ++
        upperUsername ++
        "! Message of the day: " ++
        upperMOTD)))

fullName :: String -> String -> String -> Reader ABConfig String
fullName firstname nickname lastname =
  toUpperStr firstname >>= (\upperFname ->
    toUpperStr nickname >>= (\upperNick ->
      toUpperStr lastname >>= (\upperLname ->
        Reader (\_ ->
          upperFname ++ " \"" ++ upperNick ++ " \"" ++ upperLname))))
```

Which is… well, if anything, our function deinfitions have gotten quite a bit uglier, haven’t they? We’ll deal with that problem later. But for now, notice that in `welcomeMessage` and `fullName`, all mention of the config parameter inside the function body is gone. `(>>=)` is now the one responsible for ensuring that all our functions are using the same config value. So whatever implementation we write for monadic bind, *that’s* where we move the parameter passing that previously we were doing manually.

__Exercise 2__: Implement the Functor, Applicative, and Monad instances for our newly-defined Reader type.

One thing I find very helpful when writing instances for these classes is to explicitly write out what the type of each typeclass function is supposed to be, when applied to our type. For instance, the type of `fmap` would be:

```
fmap :: (a -> b) -> Reader cfg a -> Reader cfg b
```

Show Hint

Hint

The instance for Functor is fairly straightforward, but if you’ve never seen them before, the instances for Applicative and Monad can be a bit tricky. Remember that the return value of both `(<*>)` and `(>>=)` should also be Reader values; don’t be afraid to construct such values directly. Remember that you can deconstruct the passed-in Reader values as well. You’re currently *implementing* the abstractions, not using them, so it’s perfectly fine to work with the “internal” representation.

Keep in mind what we mentioned earlier: the point of `(>>=)` (and similarly, `(<*>)`) is to do the exact same parameter passing that we previously did manually.

Since this is the most important exercise in the entire post, let’s look at the solution a little more closely. The most important thing to notice is the definitions of `(<*>)` and `(>>=)`. See how we’re passing the `cfg` parameter twice, to both of the functions contained within the Reader values we get? That’s the entire secret of how the same configuration gets threaded through our whole stack of functions.

__Exercise 3__: Since we’re using monadic binds, we’ve abstracted over our config parameter and don’t have a convenient way to access it. This is fine for functions that don’t use it, but annoying for functions like `toUpperStr` that need to inspect the config. We could directly use the `Reader` constructor to write these functions, but having to break open the internals of our abstraction just to write something so simple seems wrong. But we can instead write a “primitive” Reader function to provide the functionality that’s specific to our monad.

Define the function `ask`:

Don’t overthink this.

__Exercise 4__: It’s common to apply a function to transform the config, or otherwise project out specific fields. Let’s provide a convenience function for this case that applies a given function to the config and returns the result.

Define the function `asks`:

```
asks :: (cfg -> a) -> Reader cfg a
```

Show Solution

Solution

You can either write it in terms of `ask`:

```
asks :: (cfg -> a) -> Reader cfg a
asks f = do
  cfg <- ask
  pure (f cfg)
```

Or write it directly using the `Reader` constructor:

```
asks :: (cfg -> a) -> Reader cfg a
asks f = Reader f
```

Which you might notice is just:

```
asks :: (cfg -> a) -> Reader cfg a
asks = Reader
```

It’s worth revisiting the diagram we saw before to understand the solution here. What’s going on with this usage of `ask` in the do-notation solution?

![](https://williamyaoh.com/img/ba44afe0-8812-6408-8290-b1f5ed5b661c-reader-ask-diagram.png)

Notice how `ask` duplicates the config value from `Reader`’s first type parameter (which our end-usage functions can’t access) to its second type parameter (which they *can* access), thus letting us call the given function on it.

__Exercise 5__: Now that we have everything we need, rewrite the three string manipulation functions from before using `ask`, `asks`, and do-notation. Try running them with different configuration values.

If you’ve gotten this far, hopefully everything should be clicking into place. To recap, a diagram, relating everything we’ve seen thus far:

![](https://williamyaoh.com/img/ba44afe0-8812-6408-8290-b1f5ed5b661c-reader-full-diagram.png)

__Bonus Exercise__: It’s useful to be able to run a sub-function as if it was using a different config, like a “local environment” that reverts once we return to the current function. Can we provide a way to do that with our Reader?

Implement `local`:

```
local :: (cfg -> cfg') -> Reader cfg' a -> Reader cfg a
```

---

*You can skip this section if you feel like you understand how everything works sufficiently at this point.*

Let’s take one last look at how everything fits together. We’ve written all the individual pieces, but how does it all combine to produce something that “magically” ensures that all our functions get the same configuration parameter?

Initially we saw that we could get the behavior we wanted using nothing but pure functions and values. Here’s where we circle back around and see that what we’ve written is *also* nothing but pure functions and values.

Now that we’ve done the implementation, reflect back on the “ugly-monadic” versions of our functions that we wrote before implementing.

```
toUpperStr :: String -> Reader ABConfig String
toUpperStr str = Reader (\cfg ->
  let filters :: [Char -> Bool]
      filters = [ if don'tUseLetterE cfg then (/= 'E') else const True
                , if don'tUseLetterL cfg then (/= 'L') else const True
                ]
      passesFilters :: Char -> Bool
      passesFilters c = all (\f -> f c) filters
  in filter passesFilters (fmap Char.toUpper str))

welcomeMessage :: String -> String -> Reader ABConfig String
welcomeMessage motd username =
  toUpperStr motd >>= (\upperMOTD ->
    toUpperStr username >>= (\upperUsername ->
      Reader (\_ ->
        "Welcome, " ++
        upperUsername ++
        "! Message of the day: " ++
        upperMOTD)))

fullName :: String -> String -> String -> Reader ABConfig String
fullName firstname nickname lastname =
  toUpperStr firstname >>= (\upperFname ->
    toUpperStr nickname >>= (\upperNick ->
      toUpperStr lastname >>= (\upperLname ->
        Reader (\_ ->
          upperFname ++ " \"" ++ upperNick ++ " \"" ++ upperLname))))
```

With the hindsight of knowing what `(>>=)` is actually doing in our case, can you see how at each usage point, it’s linking together the Reader on its left side and the Reader inside the function on its right side? How it’s taking the functions contained inside each Reader value and combining them to produce one single function?

While this code probably isn’t what a human would write, we’re able to understand how it all fits together. And the code we see here isn’t all that dissimilar from what using do-notation would eventually desugar to, either. So we get the best of both worlds: readable code using do-notation, along with all the benefits of representing things as first-class data.

Finally we’ve managed to get our code to do the same parameter passing that previously we were doing by hand. That it’s possible to abstract this away using nothing but pure functions and values seems on the one hand mundane, on the other hand sublime.

---

Two monads down, one more to go. As a recap of what we’ve covered in this post: we wanted some way of having an easily-accessed read-only config for our code. We first tried to solve the problem as simply as we could, using just function parameters. That approach had some small, but annoying issues, that we then found a suitable way to abstract over, by finding a “core” type from our functions. Ultimately our solution still consisted entirely of the pure functions and data that we know and love from functional programming, but also gave us the syntactical convenience we wanted. Again, you can look at the Reader abstraction two ways: either as a sequence of “actions”, or as a normal data structure that you can pass from function to function, store in other data structures, and so on. It just looks a little different from the ones you might have seen so far.

One final insight that we glossed over is the fact that the type we ended up with as the “core” of our Reader is just a function `a -> b`. And in fact, what we’ve implemented here is exactly the monad instance defined on functions. Try it out yourself! One way to look at this isomorphism is that “configuration”, at its core, is simply function parameters. Any “external” data that your function needs, whether it comes from an API, a file, the console, it all can be represented by function parameters. And if you reduce what you need to mere parameters and do the same inspection as we’ve done here, who knows what patterns you might find for your specific problem?

Looking back on the problem we started with, to the first solution, to the eventual Reader abstraction, you might be thinking, “That’s it? That’s all it buys me?” And it’s true, compared to the massive boilerplate reduction we saw with the State monad, the minor syntactical convenience we gained from Reader seems kind of anticlimactic. It’s only a small gain, and for that reason, it’s often perfectly fine *not* to use it, even in situations where it might be suitable. If you’re okay with manually passing the extra parameter through, then just do it that way! More generally, there’s always a longhand way to solve the problem you have, and no one is forcing you to use an abstraction if you don’t want to. Ultimately the decision is yours: if you don’t feel the complexity added is worth the abstraction gained, don’t use it.

Unlike with our State implementation, this implementation of Reader is basically fine to use in real code, as long as you change the type definition of Reader to a `newtype`. Still, once we’ve learned what we needed to learn, there’s not much reason to reinvent the wheel. If you’d like to use this functionality in your own code, use an actual implementation from [`transformers`][5].

And that wraps up the Reader monad. Are there other concepts in the Haskell ecosystem that you feel like you struggle with? Found this helpful, or otherwise have questions or comments? [Talk to me!][6]

[« Previous post][7]   [Next post »][8]

---

### Before you close that tab...

Want to become an expert at Haskell, but not sure how? I get it: it's an endless stream of inscrutable concepts and words, as if you've stepped into some strange bizarro world. Where do you even start? Why does any of this matter? How deep do these rabbit holes *go*?

I want to help. What if you always knew exactly what the next signpost on your journey was, if you always knew exactly what to learn next? That's why I created a Roadmap to Expert for you: a checklist of everything you need to know, sorted by difficulty, broken down into individual, easily-digestible chunks. Best of all: it's free! Just sign up for my email list below.

And there's more where that came from: Only a fraction of what I write ends up on this blog. Sign up and get advice, techniques, and templates for writing real, useful programs, straight to your inbox.

Absolutely no spam, ever. I respect your email privacy. Unsubscribe anytime.

[1]: https://williamyaoh.com/posts/2020-07-12-deriving-state-monad.html
[2]: https://williamyaoh.com/posts/2020-07-26-deriving-writer-monad.html
[3]: https://williamyaoh.com/posts/2020-07-12-deriving-state-monad.html
[4]: https://www.optimizely.com/optimization-glossary/ab-testing/
[5]: http://hackage.haskell.org/package/transformers
[6]: https://williamyaoh.com/contact.html
[7]: https://williamyaoh.com/posts/2020-07-12-deriving-state-monad.html
[8]: https://williamyaoh.com/posts/2020-07-26-deriving-writer-monad.html
