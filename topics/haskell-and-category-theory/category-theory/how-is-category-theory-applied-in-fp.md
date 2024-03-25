---
downloaded:       2022-01-08
page-url:         https://www.quora.com/How-is-category-theory-applied-in-functional-programming
page-title:       How is category theory applied in functional programming? - Quora
---

# Category theory and functional programming

>How is category theory applied in functional programming?

The most immediately obvious relation to category theory is that we have a category consisting of types as objects and functions as arrows. We have identity functions and can compose functions with the usual axioms holding (with various caveats).


Personally, I think calling every language with a pulse (ie any language with lambdas) "functional" is not very useful. When I say "functional", I usually mean something like Haskell or at least ML, but not Java, Python, JavaScript or even Common Lisp. Scheme and Clojure are somewhat functional, but I don't talk about them too much, although I do have a fair amount of experience with the former.

Okay, honestly, I just mean "Haskell". Except when I don't. Yeah, that's linguistics for you. You'd understand if you spent less time using imperative languages and more time reading my mind³.

Some others, of course, prefer a much more promiscuous definition of "functional programming"!

Functional Programming is just imperative programming but less

This is perhaps the most pernicious myth because it's widespread, exceptionally misleading and often unvoiced. People casually assume that functional programming is just like "normal" programming in a straitjacket. It isn't! At all. Instead, functional programming provides a new basis for programming. You express things differently. Often completely differently. In fact, much of the time, there isn't even a one-to-one mapping between things you would do imperatively and things you would do functionally.

This is most evident in Haskell as it's the only common language that's functional first. This actually lets Haskell be significantly more expressive than mixed languages--all sorts of things from [deterministic parallelism][1] to rewriting rules (ie vector fusion) to STM to laziness to large swathes of its libraries are all only really possible because of Haskell's functional core. At the same time, this is what makes Haskell so different from other languages. It's an entirely distinct way of thinking, a new foundation--not a different size of the same sausage as so many other languages (and BMWs⁵).

This brings me to a little sub-myth: the difference between Haskell and impure functional languages is one of degree, not kind. This is decidedly untrue. In my experience, Haskell's purity and laziness come with a major practical and philosophical shifts even from similar languages like OCaml or Scala. Proponents of multiparadigm languages like to claim it's the best of both worlds, but it really isn't; in many ways, Haskell is more expressive than a mixed language could ever be.

For example, Haskell makes it very easy to reify computations as data, which can lead to more modular code. It's very easy to decouple the definition of a computation from its evaluation. Lists are as often replacements for loops as they are data structures; trees can represent complicated recursive functions. In turn, this makes folds and unfolds over arbitrary types more powerful in Haskell than other languages.

Trying to emulate Haskell-style functional programming in a language like Scala is not very easy. Edward Kmett--one of the leading open source Haskell developers--even went as far as writing a new JVM language to overcome these limitations; take a look at his [meticulously detailed post about why Scala was insufficient][2].

The moral: impurity is not a complete win; it makes significant sacrifices not just in safety but in expressiveness. Of course, impurity also makes some sorts of code easier, but much of that is covered in Haskell through features like state threads (ST) or just IO.

All this is also ignoring some of the other Haskell features I mentioned that depend on purity or laziness like vector fusion, rewrite rules or deterministic parallelism.

Functional Programming is inherently difficult

Functional programming is very different from what most people are used to. Many people find it difficult to pick up because it's like learning to program all over again. Remember what learning your first language was like, and imagine living through most of that again. Of course it'll seem difficult!

Going from Java to C# is just trivial. Going from Java to Python requires a slight shift in mentality, but it's basically the same thing. Even going from C to Java isn't that bad--in terms of concepts, Java adds on top of the same foundation as C. You have variables, control structures, statements and expressions. It's a gradual progression from the very first language you learned to the new imperative language du jour.

Functional programming is nothing like that. It really pulls the rug out from underneath your feet. The most fundamental ideas are completely replaced. No more statements. No more loops. No more variables. Hell, no more execution--instead of running a functional program, you evaluate it. In fact, with a language like Haskell, the order of evaluation is below your level of abstraction and does not affect what your program does--the order of execution, which controls when effects happen, is completely separate to how expressions are evaluated! This means the order your program is written in largely stops mattering, which is very weird to imperative programmers because the imperative mindset involves holding the order of your program at the back of your mind at all times.

Functional Programming is inherently complex

...and "complex" is the same as "difficult". Rich Hickey's "Simple Made Easy"² talk does a wonderful job of delineating between "complex" and "difficult". The first is a property of a system--roughly, how big it is. The second is a property of a person--how difficult something is depends heavily on experience and education.

Some people find functional programming difficult, to be sure. But this does not mean it's complex! In fact, you could fit the core evaluation rules and typing rules for a language like Haskell on a single sheet of paper. Sure, you'd have to use a very concise mathematical notation, but this only works because there are so few rules! The same is true for ML. Admittedly, any language used in the real world--including Haskell--quickly accretes additional complexity. But, at the very least, functional languages still retain a minimal, simple and well-defined core based on the λ-calculus--something imperative languages cannot really claim.

One important notion is the difference between simplicity of implementation and simplicity of semantics. Functional languages aim for the latter: they vie for more consistent behavior at the expense of a more complex runtime or compiler. Imperative languages (with Google's Go as an extreme example) often take the opposite tack, valuing simplicity of implementation over simplicity of semantics. They embrace inconsistency and undefined behavior in return for a simpler implementation and, hopefully, a simpler mapping to hardware.

Functional Programming is bad for GUIs

Functional programming is awesome for GUIs. We just do them differently. We have a brand new⁶ paradigm for GUI code: functional reactive programming (FRP). FRP makes GUI code simpler, more modular and more declarative.

GUI code is all about modelling time. WIth an imperative language, time is modelled implicitly, using mutable state and callbacks. It is a distinctly second-class citizen. You cannot talk about time directly, and instead end up in a morass of nested callbacks and heavily coupled global state. If you're very careful, it becomes semi-global state. You can't just take a variable and say "when x is larger than 7, make this red; otherwise make it blue". Instead, you have quite a bit of ceremony and external structure to wrap x into a model, or something, with event listeners and custom accessors...

With FRP, this is exactly the sort of thing you can say. This is what I mean by making time "first class": you can write code that directly references behavior of values over time.

Check out [What is Functional Reactive Programming?][3] as well as my game of life example [FRP | jelv.is][4] (with the full code at [Reactive-Life][5]).

FRP lets us do more than write nice reactive GUI code: it's also great for things like music or even robotics. It's a definite step up from using callbacks and state for all of these applications!

Haskell is exclusively functional

No. Haskell is a fully multiparadigm language. It easily supports imperative programming--people jokingly call it "the best imperative language" or an "idealized Algol"¹--and logic programming. It could even support OOP except nobody cares enough. Haskell code can even [look like C][6] with a bit of effort!

The only difference is that, unlike every other multiparadigm language, Haskell is functional first. All other languages give you an imperative base and layer functional functionality on top of it. Haskell gives you a functional base and layers imperative or logic functionality on top of that.

Since this is how Haskell is different from other languages, this is what non-Haskellers really latch onto. This leads to a bunch of nonsensical arguments against functional programming starting from straw men like "Haskell does not allow mutation".

You need a lot of math to use functional programming

You really don't. Sure, functional languages are designed on a mathematical foundation--in the same way imperative languages are designed on a computer architecture foundation. And yet you don't have to know an ALU from a register to use C!

I came to functional programming without knowing anything beyond basic calculus. Moreover, I've never been particularly good at math. It didn't hold me back in the slightest. I picked up practical monads, applicatives and functors well before I understood any of the theory.

In fact, I learned the relevant math through Haskell. But you don't even have to do that, if you really hate it. You can learn Haskell's abstractions like functors and monads just like you would learn about Java Beans, Lua coroutines or Scheme macros. In fact, the idea of a functor was one of the easiest CS things I ever learned--it's just any type you can map a function over.

Functional programmers have to be exceptionally smart and good at CS

I'm a functional programmer. Enough said :).

Functional programming isn't some dark, unholy magic. (Well, maybe Coq is :P.) In many ways, functional programming actually helps you overcome a lack of brute intelligence. The Haskell type system drastically restricts the sorts of wrong functions you can write, which makes it possible to just bash your way through hard problems by mindlessly running into the type system.

* I find Haskell to be my best language for programming drunk⁷. All the stupid mistakes--and some rather non-stupid mistakes--are caught by the compiler. So I can just twiddle with my code until it typechecks. Then it works. Usually. Far more often than it has any right to! Many of the tools Haskell gives you exist to overcome your own fallibility.

* Functional programming is also surprisingly accessible to beginners.

* Another great example is IMVU. One of my friends is on their team, helping move a fair number of rank-and-file programmers over to Haskell. For web development. Again, people can become productive in a fairly short time. And, at the risk of sounding arrogant, the average level of talent at a company like IMVU isn't going to be quite like one of the top trading firms in the world.

That said, my experience is that the Haskell community is disproportionately smart and good at CS. Not because Haskell forces you to be, but because the community is rather self-selecting. And smarter people seem more likely to pick Haskell up voluntarily. In fact, it's just like [The Python Paradox][7] of yesteryear--and yet note how Python is widely regarded as one of the easiest languages to learn!

---

Footnotes⁴:

¹ This is a great example of [ha ha only serious][8] -something that at first seems just a joke but actually has a deeper meaning. Haskell makes for a good imperative language because it has "imperative actions" as first-class citizens; you don't even have to wrap statements in lambdas to pass them around! It also makes adding expressive imperative control structures, from things like `when` to `callCC` as libraries.

Also, the Jargon File is an incredible resource. It's perfect if you're actually interested in the anthropology of CS and the "hacker" culture.

² It's a wonderful talk. I love the conceptual framework he defines, even if I disagree with some of hist conclusions. Static types--especially ones based on Hindley-Milner with no subtyping--are not actually complex. In fact, in some ways, they're simpler than the dynamically typed system Clojure uses!

Here's a link to the talk itself: [Simple Made Easy][9]

³ A fun and easy way to become a better programmer, I maintain.

⁴ Remember how I mentioned that order does not matter in a functional program? Yeah, that's totally my excuse for not numbering the footnotes in any appreciable order. After all, I am a functional programmer at heart!

⁵ Two totally different models (7 series behind, 5 series in front). Most programming languages are sort of like this.

⁶ Actually, just like functional programming itself, FRP has been around for a while, at least since 1997. But it's only really been catching on very recently.

[⁷]: I've used a surprising number of different languages when drunk, so I can make a fair comparison. In the crazy world of San Francisco, we have fun by solving [Project Euler][10] problems and drinking. What exciting lives we live.


[1]: http://www.quora.com/Haskell/What-are-the-novel-ideas-and-profound-insights-in-the-design-of-the-Haskell-programming-language/answer/Tikhon-Jelvis?srid=p9P3&share=1

[2]: http://www.reddit.com/r/haskell/comments/1pjjy5/odersky_the_trouble_with_types_strange_loop_2013/cd3bgcu

[3]: https://www.quora.com/What-is-Functional-Reactive-Programming

[4]: http://jelv.is/frp/

[5]: https://github.com/TikhonJelvis/Reactive-Life

[6]: http://augustss.blogspot.com/2007/08/programming-in-c-ummm-haskell-heres.html

[7]: http://paulgraham.com/pypar.html

[8]: http://www.catb.org/jargon/html/H/ha-ha-only-serious.html

[9]: http://www.infoq.com/presentations/Simple-Made-Easy-QCon-London-2012

[10]: http://projecteuler.net/
