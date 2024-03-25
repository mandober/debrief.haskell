---
downloaded:       2022-01-01
page-url:         https://wiki.haskell.org/Operational
page-title:       Operational - HaskellWiki
article-title:    Operational - HaskellWiki
---
# Operational - HaskellWiki

The operational library makes it easy to implement monads with tricky control flow.
## What is it?

The *operational* library makes it easy to implement monads with tricky control flow.

This is very useful for: writing web applications in a sequential style, programming games with a uniform interface for human and AI players and easy replay, implementing fast parser monads, designing monadic DSLs, etc.

For instance, imagine that you want to write a web application where the user is guided through a sequence of tasks ("wizard"). To structure your application, you can use a custom monad that supports an instruction `askUserInput :: CustomMonad UserInput`. This command sends a web form to the user and returns a result when he submits the form. However, you don't want your server to block while waiting for the user, so you have to suspend the computation and resume it at some later point. Sounds tricky to implement? This library makes it easy.

The idea is to identify a set of primitive instructions and to specify their operational semantics. Then, the library makes sure that the monad laws hold automatically. In the web application example, the primitive instruction would be `AskUserInput`.

Any monad can be implemented in this way. Ditto for monad transformers.

A thorough introduction to the ideas behind this library is given in ["The Operational Monad Tutorial"][1], published in [Issue 15 of the Monad.Reader][2].

## Releases and Resources

-   Download
    -   [operational][3] - latest version on hackage
    -   [source code][4] on github
-   Documentation
    -   [The Operational Monad Tutorial][5] - Introductory document explaining the concept.
    -   [Library documentation][6] - How to use the libary proper; documents changes with respect to the tutorial.
    -   [API reference][7] on hackage.
    -   **[Example code][8]** - Extensive collection of working code examples.
-   Feedback and Contact
    -   Maintainer: [Heinrich Apfelmus][9] <apfelmus at quantentunnel de>
    -   [Issue Tracker][10]
    -   [Ask a question on StackOverflow][11]

[1]: http://apfelmus.nfshost.com/articles/operational-monad.html
[2]: http://themonadreader.wordpress.com/2010/01/26/issue-15/
[3]: http://hackage.haskell.org/package/operational
[4]: https://github.com/HeinrichApfelmus/operational
[5]: http://themonadreader.wordpress.com/2010/01/26/issue-15/
[6]: https://github.com/HeinrichApfelmus/operational/tree/master/doc
[7]: http://hackage.haskell.org/package/operational/
[8]: https://github.com/HeinrichApfelmus/operational/tree/master/doc/examples#readme
[9]: http://apfelmus.nfshost.com/
[10]: https://github.com/HeinrichApfelmus/operational/issues?sort=created&direction=desc&state=open
[11]: http://stackoverflow.com/questions/ask?tags=monads+free-monad+haskell
