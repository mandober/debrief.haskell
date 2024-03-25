# Wingman for Haskell

Home: 
https://haskellwingman.dev/

Releases: 
- https://github.com/haskell/haskell-language-server/releases
- https://downloads.haskell.org/~hls
- https://downloads.haskell.org/~hls/haskell-language-server-1.7.0.0/
- https://github.com/haskell/haskell-language-server
- https://haskell-language-server.readthedocs.io/en/latest/
- https://github.com/haskell/vscode-haskell/releases
- https://marketplace.visualstudio.com/items?itemName=haskell.haskell

Focus on the important stuff - delegate the rest. Wingman writes the boring, auxiliary code, so you don't have to. Generate functions from type signatures, and intelligently complete holes.

Wingman for Haskell automates away the old hole-driven design workflow. You probably know the one - where you put down a hole, and ask the compiler what type it has. After a little bit of thinking, you add a little bit of code around the hole, and then ask the compiler again. Over time, the expression gets written, but it seems like the compiler is doing most of the work. Sometimes it can feel like all you're around for is to write down what the compiler says.

Instead, imagine a world where you can just ask "hey, fill in this hole for me, please." Wingman is that world. A typechecker is nothing but a series of rules that determine what type an expression has. By running these rules backwards, we can instead look at what expressions could produce a certain type. Search through a few thousand well-typed expessions, score them, and give back the best one.

As you might expect, running the search is the easy part. Keeping it fast and making sure it returns good results is where the challenge comes in. And often, these goals are in tension. Always returning undefined would certainly be well-typed and fast, but it's clearly not what you had in mind.

Wingman tries really hard to write the code you would have written youself. It prefers linear solutions, ensures productive recursion, and has a reasonable knowledge of common Haskell idioms - including eta-reduction and every-day combinators.

Of course, sometimes human ingenuity is necessary to get a function out the door, and no automated tool can do the job. Even when you're not asking Wingman to fill holes for you, it can still help. By running small pieces of the code synthesizer, it can perform case splits, introduce lambdas and, in general, help move you closer to the important part of the problem.
