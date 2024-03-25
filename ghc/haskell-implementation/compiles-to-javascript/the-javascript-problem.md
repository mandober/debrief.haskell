---
downloaded:       2022-01-08
page-url:         https://wiki.haskell.org/The_JavaScript_Problem
page-title:       The JavaScript Problem - HaskellWiki
article-title:    The JavaScript Problem - HaskellWiki
---
# The JavaScript Problem - HaskellWiki

The JavaScript problem is two-fold and can be described thus:
## The problem

The JavaScript problem is two-fold and can be described thus:

1.  **JavaScript, the language.** JavaScript, the language, has some issues that make working with it inconvenient and make developing software harder :

-   lack of module system (only pre-ES6),
-   weak-typing,
-   verbose function syntax1 (pre-ES6),
-   late binding2, which has led to the creation of various static analysis tools to alleviate this language flaw3, but with limited success4 (there is even a static type checker5),
-   finicky equality/automatic conversion,
-   `this` behaviour,
-   and lack of static types.

1.  **We need JavaScript.** Using it for what it is good for, i.e. providing a platform for browser development, but not using the language *per se*, is therefore desirable, and many are working to achieve this, in varying forms. There are various ways to do it, but we ought to opt for compiling an existing language, Haskell, to JavaScript, because we do not have time to learn or teach other people a new language, garner a new library set and a new type checker and all that Haskell implementations provide.

## Mainstream alternatives

### CoffeeScript

It makes many aspects of JavaScript sane and convenient, and you get a compilation check that verifies syntax, however it still suffers greatly from weak-typing.

### TypeScript

Structural typing with traditional generics on top of JavaScript. Of all the alternatives, TypeScript's advantage is that it makes no changes to JavaScript. Existing JavaScript code that passes jshint is valid Typescript code. TypeScript also adds features from the latest JavaScript standards that it can compile down to older versions of JavaScript. TypeScript is by far the easiest JavaScript variant to learn. The downside is that one might desire a better language than just JavaScript + types.

TypeScript defaults to dynamic typing when it can't figure the type out. However, it now has a \`noImplicitAny\` setting that will give a compilation error if it can't figure out the type.

Structural sub-typing seems a good fit for JavaScript. Typescript did suffer from null acting as a valid value for any type until 2.0, which introduced the \`strictNullChecks\` compiler option.

## Haskell -> JS

### UHC

Original blog post [here.][1] Quickstart guide [here.][2] A more in-depth discussion about the current capabilities of the backend [here.][3] For an example of using the JavaScript compilation for a real app see this [blog post][4], there is also a port of wxAsteroids to the browser (see [github][5] or a [blog post][6]).

-   Beta.
-   Only works for UHC, but promising.
-   UHC compiles enough of Hackage to be very useful.
-   Doesn't produce an explosion of code, seemingly.
-   Fairly substantial JS/DOM/W3C/HTML5 API.
-   Currently works.

### Fay

Website: [https://github.com/faylang/fay/wiki][7] Discussion on Reddit: [Fay slides][8]. The package is on [Hackage][9]. Fetch with Git:

 git clone [git://github.com/faylang/fay.git][10]

-   Compiles a subset of Haskell, needs more
-   Currently works.

### GHCJS

The GitHub page is [here.][11]

-   Works.
-   Incomplete.
-   Nicely designed.
-   Compiles most pure Haskell libraries no problem.
-   FFI to JS works, and the author, sviperll is a helpful guy.

#### Libraries

-   The [reflex][12] library provides [reflex-dom][13] for building web GUIs using GHCJS. ([Presentation][14], [Slides][15])

-   The [miso][16] library provides an Elm-like interface for building web GUIs using GHCJS. Allows for isomorphic javascript.

#### Apps

-   [markup.rocks][17] - Pandoc + GHCJS + Reflex. [Reddit Thread][18], [Source code][19].

#### Further reading

-   [Lenz blog: Chapter 4 - GHCJS and the Client][20]

### Haste

[Website][21], [Hackage][22]

-   Seamless, type-safe single program framework for client-server communication
-   Easy JavaScript interoperability
-   Generates small, fast, minifiable code.
-   Lightweight concurrency, Cabal integration, FFI and GHC extensions supported.
-   Cross platform.
-   Works.

### [JMacro][23]

On the Haskell wiki (see above) and on [Hackage][24]

-   Mature, Maintained
-   Not Haskell but an EDSL \_in\_ Haskell nonetheless.
-   JMacro Panels provides a purely Haskell combinator library that generates dynamically updating html and js with asynchronous client-server communication.
-   Syntax is a fusion of Haskell and JavaScript
-   Untyped, but with syntactic correctness (at least) enforced at compile-time.
-   Embeddable through quasi-quoting
-   Support for various forms of code-generation

### Others

-   [Haskell interpreter in JS][25] — An interpreter. Haven't tried but is apparently dead.
-   YHC JS backend — Beta-ish. Apparently works, but I was unable to compile YHC, so haven't tried yet. I would be interested in anyone's experience using it. There's [an old wiki page][26] about Yhc's JavaScript support, but Yhc itself is a dead project.
-   Emscripten — not Haskell→JS, but compiles LLVM/Clang output to JavaScript. Could possibly be used for GHC→LLVM→JS compiling, which I tried, and works, but would have to also compile the GHC runtime which is not straight-forward (to me) for it to actually run.
-   HJScript — Beta. EDSL, not Haskell→JS. Works. Not *very* annoying to program in, but is JS semantics, not Haskell. Hackage package [here.][27]
-   Some have also tried writing a Haskell→JS compiler to make a more direct JS-aware translation of code (to not have huge code output a la GHCJS, YHC, Emscripten).
-   I've tried [compiling via JHC and Emscripten][28] a while ago, which worked, but IIRC the output was rather slow.
-   It's also possible to compile Hugs via Emscripten, which works (with minor tweaks), but again, it's too slow.

## FP -> JS

### Ur/Web

[http://www.impredicative.com/ur/][29]

Perhaps the problem with Ur is that they are selling both a backend and a frontend together. Being a new language, the backend is lacking in libraries to be practical for many tasks. However, there is an RSS reader that is using Ur for the front-end and Haskell for the backend: [https://bazqux.com/][30]

### Opa

Similar to Ur/Web, write one language in the front-end and backend: [http://opalang.org/][31] Haven't tried it. No idea what its type-system is like.

### OCaml

The OCaml -> JS compiler is supposed to be good, it is now used at Facebook for an internal in-browser code editor. [http://ocsigen.org/js\_of\_ocaml/][32]

### GorillaScript

[http://ckknight.github.io/gorillascript/][33]

immutable by default, global type inference, macros, what coffeescript should have been. The syntax is similar to coffeescript

### Roy

[Roy][34]: meld JavaScript semantics with functional languages. Experimental, but has many bread-and-butter Haskell features. Roy is written in JS.

### PureScript

[PureScript][35] aims to provide a type system for a fragment of JavaScript. It includes many features which are similar to features of Haskell, such as type classes and RankNTypes, and its syntax mirrors that of Haskell very closely, but it is a fundamentally different language with the execution model of JavaScript. PureScript is written in Haskell. The project has a focus on the generation of efficient, readable JavaScript.

### Idris

Idris is a compiled language with dependent types, implemented in Haskell, with backends for both LLVM and JavaScript. Experimental.

-   Full dependent types with dependent pattern matching where clauses, with rule, simple case expressions, pattern matching let and lambda bindings
-   Dependent records with projection and update
-   Type classes
-   Monad comprehensions
-   Syntactic conveniences for lists, tuples, dependent pairs do notation and idiom brackets
-   Indentation significant syntax
-   Extensible syntax
-   Tactic based theorem proving (influenced by Coq)
-   Cumulative universes
-   Totality checking
-   Simple foreign function interface (to C)
-   Hugs style interactive environment

Links:

-   [Website idris-lang.org][36]
-   [Dependent Type in haskell wiki][37]
-   [WP (en) Dependent type][38] (with Idris listed under language comparison)

### Elm

[Elm][39] is a reactive pure functional programming language implemented in Haskell, which takes heavy inspiration from Haskell's syntax. It's considered a direct competitor to PureScript, since it's basically a more beginner-friendly yet pared down "Haskell for the web". Unfortunately, this desire to be beginner-friendly has left Elm without Higher Kinded Types. It has a simple foreign function interface for JS interop, called "ports".

## Links

-   [What is the state of "The JavaScript Problem"? What is the currently preferred way to solve in a real world application?][40] (reddit, 2014-06-20)
-   [Yesod - JavaScript Options][41]
-   [Chris Done Blog][42] - Tag: JavaScript

## Footnotes

1.  Its support for closures is commonly noted as being one of JavaScript’s redeeming features.
2.  Early binding allows for static verification of the existence of method-signature pairs (e.g. v-tables). Late binding does not give the compiler (or an IDE) enough information for existence verification, it has to be looked up at run-time.
3.  There are several hinting libraries, which developers insist are indispensable tools when developing JavaScript seriously, such as JavaScript lint, JSLint, and JSure.
4.  “Any non-trivial analysis is very difficult due to JavaScript’s dynamic nature.” — Berke Durak, Ph.D., author of jsure.
5.  Google Inc. thought it necessary to develop a compiler, Google Closure, which does type-checking and limited inference.

[1]: https://github.com/atzedijkstra/javascript-runtime-for-UHC
[2]: http://chrisdone.com/posts/2012-01-06-uhc-javascript.html
[3]: http://www.norm2782.com/improving-uhc-js-report.pdf
[4]: http://alessandrovermeulen.me/2012/01/26/getting-rid-of-javascript-with-haskell/
[5]: http://uu-computerscience.github.io/js-asteroids/
[6]: http://www.rubendegooijer.nl/posts/2013-04-06-haskell-oop.html
[7]: https://github.com/faylang/fay/wiki
[8]: http://www.reddit.com/r/haskell/comments/11yrpi/fay_slides/
[9]: http://hackage.haskell.org/package/fay
[10]: git://github.com/faylang/fay.git
[11]: https://github.com/ghcjs/ghcjs
[12]: https://reflex-frp.org/
[13]: http://hackage.haskell.org/package/reflex-dom
[14]: https://www.youtube.com/watch?v=mYvkcskJbc4
[15]: https://obsidian.systems/reflex-nyhug/#/step-1
[16]: http://hackage.haskell.org/package/miso
[17]: http://markup.rocks/
[18]: https://www.reddit.com/r/haskell/comments/35ax22/ive_compiled_pandoc_with_ghcjs_and_built_an/
[19]: https://github.com/osener/markup.rocks
[20]: http://blog.wuzzeb.org/full-stack-web-haskell/client.html
[21]: http://haste-lang.org/
[22]: http://hackage.haskell.org/package/haste-compiler
[23]: https://wiki.haskell.org/JMacro "JMacro"
[24]: http://hackage.haskell.org/package/jmacro
[25]: https://github.com/johang88/haskellinjavascript
[26]: http://www.haskell.org/haskellwiki/Yhc/Javascript
[27]: http://hackage.haskell.org/package/HJScript
[28]: http://lpaste.net/84342
[29]: http://www.impredicative.com/ur/
[30]: https://bazqux.com/
[31]: http://opalang.org/
[32]: http://ocsigen.org/js_of_ocaml/
[33]: http://ckknight.github.io/gorillascript/
[34]: http://roy.brianmckenna.org/
[35]: http://purescript.org/
[36]: http://idris-lang.org/
[37]: https://wiki.haskell.org/Dependent_type "Dependent type"
[38]: http://en.wikipedia.org/wiki/Dependent_type
[39]: http://elm-lang.org/
[40]: http://www.reddit.com/r/haskell/comments/28o7my/what_is_the_state_of_the_javascript_problem_what/
[41]: https://github.com/yesodweb/yesod/wiki/JavaScript-Options
[42]: http://chrisdone.com/tags/javascript
