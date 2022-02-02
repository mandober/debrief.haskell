---
downloaded:       2022-01-01
page-url:         https://wiki.haskell.org/Simple_monad_examples
page-title:       Simple monad examples - HaskellWiki
article-title:    Simple monad examples - HaskellWiki
---
# Simple monad examples - HaskellWiki

This page is designed to show some simple examples of using monads, specifically using Maybe.
This page is designed to show some simple examples of using [monads][1], specifically using [Maybe][2].

I personally found that I reached monad-enlightenment once I contrived this simple example while playing around to see the "guts" of a monadic expression:

Just 5 \>>= (\\ x \-> if (x \== 0) then fail "zero" else Just (x + 1) )

Which results in:

All you really need to know, is that the `(>>=)` operator either returns `Nothing` if it is passed `Nothing` on its left-hand side; or if its left-hand side is a `Just …` it strips off the `Just`, and passes the contents into the function supplied on its right-hand side. Simple!

### Some simple exercises

What would the following snippets resolve to?

-   Just 0 \>>= (\\ x \-> if (x \== 0) then fail "zero" else Just (x + 1) )
    

-   Nothing \>>= (\\ x \-> if (x \== 0) then fail "zero" else Just (x + 1) )
    

---

More examples can be found in the reference guide [A tour of the Haskell Monad functions][3], by Henk-Jan van Tuyl.

---

[1]: https://wiki.haskell.org/Monad "Monad"
[2]: https://wiki.haskell.org/Maybe "Maybe"
[3]: http://members.chello.nl/hjgtuyl/tourdemonad.html
