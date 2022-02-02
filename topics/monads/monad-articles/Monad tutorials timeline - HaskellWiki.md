---
downloaded:       2022-01-01
page-url:         https://wiki.haskell.org/Monad_tutorials_timeline
page-title:       Monad tutorials timeline - HaskellWiki
article-title:    Monad tutorials timeline - HaskellWiki
---
# Monad tutorials timeline - HaskellWiki


  
[][1]

This is a comprehensive timeline of monad tutorials and related articles.

Please update this list as it becomes outdated! If you find a tutorial, article, post, comment, or message that stands on its own as an explanation of monads, then please take a moment to paste the link somewhere on this page (register a throwaway account, if you prefer). The date, author, and blurb can be added later. This will greatly help others who are using this list as a resource for learning about monads.

## before 2000

-   1992-02 (published date) [The essence of functional programming][2] - Philip Wadler
    
    "This paper explores the use monads to structure functional programs. No prior knowledge of monads or category theory is required."
    

-   1999-02 [What the hell are Monads?][3] - Noel Winstanley \[2800 words\]
    
    "Once upon a time, people wrote their Haskell programs by sequencing together operations in an ad-hoc way." ... "For our purposes, a monad is a triple of a type and **then** & **return** operators defined over it so that the following laws apply: ..."
    

-   1995-05 [Monadic IO in Haskell 1.3][4] - Andrew D. Gordon and Kevin Hammond
    
    "We describe the design and use of monadic I/O in Haskell 1.3"
    

-   1992-08 [Monads for Functional Programming][5] (pdf) - Phil Wadler, a designer of Haskell \[9100 words\].
    
    "Shall I be pure or impure?" ... "A *monad* is a triple (*M*, *unit*, *★*) consisting of a type constructor *M* and two operations of the given polymorphic types."
    

## year 2000

-   2000-01 [Monads for the working Haskell Programmer][6] - Theodore S. Norvell \[4200 words, Haskell98, Gofer\]
    
    "...monads can be used to implement several other programming features including: consuming input, producing output, exceptions and exception handling, nondeterminisim."
    

## year 2002

-   2002 [Yet Another Haskell Tutorial][7] (Chapter: Monads) - Hal Daumé III
    
    "The definition of a monad is a slightly trimmed-down version of our Computation class. The Monad class has four methods ..."
    

## year 2003

-   2003-08 [All about Monads][8] - Jeff Newbern \[22600 words\] (Japanese translation: [モナドのすべて][9])
    
    A comprehensive introduction. Covers monad transformers and some common monads. An appendix presents monads as assembly lines.
    
    "A monad is a way to structure computations in terms of values and sequences of computations using those values."
    

## year 2004

-   2004-07 [A Schemer's Introduction to Monads][10] - Dave Herman \[1700 words, Lisp, Scheme\]
    
    "The main insight of monads is that all side effects, from mutation to I/O to non-termination, have one thing in common: order of evaluation matters." ... "So monads are about talking about effects in the context of a pure semantics."
    

-   2004-07 [Monads as Containers][11] ([Russian translation][12]) - Cale Gibbard \[2900 words\]
    
    "A monad is a container type together with a few methods defined on it. Monads model different kinds of computations." ... "it is more natural sometimes to begin with map (fmap), return and join"
    

-   2004-08 [Monads in Perl][13] - Greg Buchholz \[2200 words, Perl\]
    
    "Essentially a monad is a hidden data structure (Fig. 1) which automatically passes state around for us. "
    

## year 2005

-   2005-07 [Monads in Ruby][14] - MenTaLguY
    
    Presents monads in a friendly language, starting from Identity
    
    "They let you chain pass \[bind\] operations together to make little computational pipelines, with rules of your choosing. They don’t manipulate values themselves — that’s the job of the blocks (functions) you plumb together using the monad."
    

-   2005-11 [Of monads and space suits][15] - Eric Kow
    
    Functions are space stations, parameters are astronauts and monads are space suits that let us safely travel from one function to another.
    

## year 2006

-   2006-03 [Understanding Monads][16] - Eric Kow
    
    Monads as nuclear waste containers, an adaptation of monads as space suits with a new metaphor suggested by Paul Johnson
    
    "What we need is some way to capture the pattern 'do X and then do Y, where Y may be affected by X'. Monads are the way we do this." ... "In a sense, each monad is its own little minilanguage specially suited for its particular task."
    

-   2006-07 [The Monadic Way][17] - Andrea Rossato
    
    A two-part tutorial. The first part shows you how build a simple evaluator, and the second part shows you how to "take the complexity" out of it by using techniques such as monad transformers
    
    Revised 2006-09 to include [Meet Bob The Monadic Lover][18]: "what monads look like and what they are useful for, from the perspective of a ... lover."
    

-   2006-08 [You could have invented monads! (and maybe you already have)][19] - Dan Piponi
    
    "Writing introductions to monads seems to have developed into an industry," Dan (sigfpe) observes. He argues that monads are not "something esoteric in need of explanation", but walks you through the process of reinventing monads to solve some very basic and practical problems.
    
    "So now I can reveal what a monad is. The triple of objects (m,unit,bind) is the monad, and to be a monad they must satisfy a bunch of laws such as the ones you've been proving."
    

-   2006-10 [Ask Reddit: What the hell are monads?, an answer][20] - tmoertel \[700 words\]
    
    "I think of monads as computational environments in which you get to make up the rules." ... "monads are ... a general model of computation that lets you pick and choose the environmental features that you want for your computations."
    

-   2006-10 [Monad Transformers Step by Step][21] - Martin Grabmüller
    
    Monad transformers are rarely covered in introductory tutorials. This "is not a paper about implementing transformers, but about using them to write elegant, clean and powerful programs in Haskell". Available as a 12 page PDF or .lhs file.
    

-   2006-11 [There's a Monster in my Haskell!][22] Andrew Pimlott
    
    This delightful "tutorial" presents monads as monsters which devour values, use them to feed other monsters and regurgitate them when slain.
    

-   2006-12 [Maybe Monad in Java][23] - Tony Morris
    
    Monads can also be useful in Java!
    

## year 2007

-   2007-01 [Think of a monad][24] - Don Stewart (reposted on Eric Kow's blog)
    
    Don integrates some pre-existing monadic metaphors, shedding light on monads in a truly comprehensive manner (illustration by Eric)
    

-   2007-02 [Understanding Monads. For Real][25] - Karsten Wagner
    
    A monad is like a macro
    

-   2007-02 [Crash Course in Monads][26] Monads for Mathematicians - Vlad Patryshev
    
    Author's Description: This crash course starts with an EASY! introduction to categories and functors, then we define a monad, then give some basic examples of monads in categories, then present monadic terminology as used in programming languages.
    

-   2007-03 [Monads in 15 Minutes][27] - Eric Kidd
    
    Eric boils monads down to 15 minutes, using backtracking and Maybe as motivating examples. Eric uses `join`, which seems quite rare for monad tutorials (cf Cale's *Monads as containers*)
    
    Then I lie down in a dark room with a warm wet cloth over my eyes.
    

-   2007-04 [The Real Monad Transformer][28] - Henning Thielemann
    
    Not a tutorial either, but an important aid in demystifying monads
    

-   2007-08 [Monads as computation][29] - Cale Gibbard
    
    A very straightforward presentation of monads. Notable for its "The whole point" section, which conveys why we bother with all this monad business.
    

-   2007-08 [Understanding Monads][30] (2) - Apfelmus
    
    Wikibook rewrite of the original monads tutorial. Less fluff, more pedagogy. \[In progress at the time of this writing\].
    

-   2007-08 [Monad (sans metaphors)][31] - Claus Reinke
    
    From a discussion about monad tutorials on Haskell Café (the name is due to haskellwiki user 'Green tea').
    

## year 2008

-   2008-01 [An explanation by 808140][32] \[1588 words\]
    
    "The best way to grok monads is to ... familiarize yourself with several common monads and their uses." ... "All that makes a monad, really, is that you can define two functions like unit and bind on them."
    

-   2008-03 [Real World Haskell, Chapter 14: Monads][33] - Bryan O'Sullivan, Don Stewart, and John Goerzen
    
    "We aim to show you that a monad is often an obvious and useful tool to help solve a problem."
    

-   2008-06 [Monads][34] (in Russian, [English translation][35]) - Eugene Kirpichov \[8200 words ru, 10000 en\]
    
    "A monad is a triple (m, return, >>=), where: ..." ... "As we progressed, we saw that monads are most commonly used for two different purposes: structuring the control flow and describing imperative effectful computations (IO, State, IndentIO), and structuring data flow (Maybe, List, Dist)."
    

-   2008-09 [What is a monad?, an answer][36] - JacquesB (other answers at link)
    
    "An alternative term is computation builder which is a bit more descriptive of what they are actually useful for." ... "In layman's terms, a monad is just a type for which the >>= operation is defined."
    

-   2008-12 [From Monoids to Monads][37] - Dan Piponi

## year 2009

-   2009-01 [Abstraction, intuition, and the “monad tutorial fallacy”][38] - Brent Yorgey \[meta\]
    
    Commentary on monad tutorials and why many may be so unhelpful. "What I term the 'monad tutorial fallacy,' then, consists in failing to recognize the critical role that struggling through fundamental details plays in the building of intuition."
    

-   2009-03 [A Monad Tutorial for Clojure Programmers][39]
    
    "Monads are about composing computational steps into a bigger multi-step computation."
    

-   2009-03 [How you should(n’t) use Monad][40] - beelsebob
    
    "When we have functions that produce values that are hidden inside boxes, we have a problem. ... Monads add a single function called join, which is used to flatten out the layers of boxes: ..."
    

-   2009-06 [The Greenhorn's Guide to becoming a Monad Cowboy][41] - Hans van Thiel \[9600 words\]
    
    Covers basics, with simple examples, in a *for dummies* style. Includes monad transformers and monadic functions. "Actually, programming monads is much like cattle driving! So, let's get started."
    

-   2009-10 [The State Monad: A Tutorial for the Confused?][42] - Brandon Simmons
    
    This is written for someone who has a good understanding of the Maybe and List monads, but has gotten stuck trying to understand State... State monad is just an abstraction for a function that takes a state and returns an intermediate value and some new state value.
    

-   2009-11 [What Does Monad Mean?][43] - Tony Morris
    
    "The \[*monad*\] concept will be presented in a way with the objective of supplying enough understanding to apply the practical implications and will not address the underlying mathematics or category theory."
    

-   2009-11 [What a Monad is not][44]
    
    A desperate (futile?) attempt to end the eternal chain of monad tutorials.
    

## year 2010

-   2010-03 [State Monad Tutorial][45] - Byron Johnson
    
    "My goal is to teach others to help them understand the State monad. ... After this point, you should be able to easily understand the other monads."
    

-   2010-04 [Programming with effects – the story so far][46] - Patai Gergely
    
    Explains the relationships between the various abstractions over side effects, namely applicative functors, arrows, and monads.
    

-   2010-07 [I come from Java and want to know what monads are in Haskell][47] - Tim Carstens \[Java\]
    
    Translates a simple Java class into a stack of monad transformers, with a metaphor about how monads are like conversations, and why this idea should be familiar to OO programmers.
    
    "What I am going to talk about is how to use monads to do something in Haskell that is easy to do in Java." ... "This is what different monads do: each comes with its own set of operations that are legal within the context that the monad is modeling."
    

-   2010-08 [Learn You a Haskell, A Fistful of Monads][48]
    
    "monads are just applicative functors that support >>=. The >>= function is pronounced as *bind*."
    

-   2010-08 [Yet Another Monad Tutorial, part 1][49] ([2][50], [3][51], [4][52], [5][53], [6][54], [7][55], [8][56]) - Mike Vanier \[7100 words p1, 45000 total\]
    
    "Monads are a generalization of functions, function application, and function composition to allow them to deal with richer notions of computation than standard functions."
    

-   2010-10 [A monad is just a monoid in the category of endofunctors, what's the problem?, an answer][57] - pelotom (other answers at link)
    
    "The original sentence is this: All told, a monad in X is just a monoid in the category of endofunctors of X, with product × replaced by composition of endofunctors and unit set by the identity endofunctor."
    

-   2010-11 [Monads and Effects in Purely Functional Programming (slides)][58] - Johan Glimming

## year 2011

-   2011-01 [Monads for the Curious Programmer: Part 1][59], [Part 2][60], [Part 3][61], and [Monads in C++][62] - Bartosz Milewski \[3400, 4300, 3400, and 5200 words\]
    
    "Monads are hard to describe because they don’t correspond to anything in our everyday experience" ... "A monad is an endofunctor together with two special families of morphisms, both going vertically, one up and one down"
    

-   2011-03 [monads in R: sapply and foreach][63] - Ferdinand Jamitzky
    
    "Monads are a powerful way of structuring functional programs"
    

-   2011-04 [Translation from Haskell to JavaScript of selected portions of the best introduction to monads I¢ve ever read][64] - James Coglan \[3200 words, Javascript\] (cf. 2006 "You Could Have Invented Monads")
    
    "Monads are really about composing functions"
    

-   2011-04 [Understanding Haskell Monads][65] - Ertugrul Söylemez \[12400 words\]
    
    "A monad is a wrapper type around another type (the inner type), which adds a certain structure to the inner type and allows you to combine computations of the inner type in a certain way."
    

-   2011-10 [The Dead Simple, No Chit Chat, Zero-Analogy Haskell Monad Tutorial][66] - Dan Burton \[1800 words\]
    
    "Monads are simply Applicative Functors, which also define some way of flattening nested monadic values, and shoving monadic values into monadic functions. Applicative Functors are..."
    

## year 2012

-   2012-02 [Futures: Monads with the Context of Asynchronous Programming][67] - mr23
    
    "it's best to think of a monad as representing some context. What do I mean by context? ..."
    

-   2012-02 [The Day Python Embarassed Imperative Programming][68] - The 27th Comrade
    
    "Monads are conditional function calls"
    

-   2012-02 [Scala Monads][69] - Dan Rosen, Marakana \[scala, video\]

-   2012-04 [The Less Travelled Monad Tutorial: Part 1][70], [Part 2][71] - mightybyte
    
    "a monad is a certain type of context that provides two things: a way to put things into the context, and function application within the context"
    

-   2012-04 [Why Do Monads Matter?][72] - Chris Smith
    
    "... The category that they form is called a Kleisli category, and it’s basically another way of looking at monads."
    

-   2012-08 [Monads in Pictures][73] - johnw
    
    "This is not a tutoral on monads, nor will I use any math terms here. This is for people who have learned enough about monads to use them, but want to get a better picture of what they’re doing and why they exist."
    

-   2012-11 [Monads à la Mode][74] - Cameron Swords and Daniel P. Friedman
    
    "The purpose of this article is to provide a concise introductionto monads for anyone who has an understanding of Scheme and simple types."
    

## year 2013

-   2013-01 [Monads in Haskell: An Introduction][75] - Benjamin Kovach
-   2013-03 [A Simple Monad Tutorial][76] - Leah Hanson
-   2013-04 [Functors, Applicatives, And Monads In Pictures][77].
-   2013-04 [A newbie in Haskell land][78]
    
    "I have identified three kinds of monads ... Monad as control of the sequencing ; Monad as control of side effects ; Monad as container"
    
    -   other articles:
        -   [(->) monad][79]
        -   [Haskell Study Plan][80]
        -   [Category Theory and the category of Haskell programs : parts 1][81], [2][82] and [3][83]
        -   [Coproduct of free monads and web development][84]
        -   [A Web Monad][85]
        -   [Meaning and monads][86]
-   2013-05 [Kleisli Composition à la Up-Goer Five][87].
-   2013-06 [Three Useful Monads][88]
-   2013-07 [Monads Made Difficult][89]
-   2013-08 [From Adjunctions to Monads][90]
-   2013-08 [Functors and monads for analyzing data][91]
-   2013-08 [Pure Functions, Laziness, I/O, and Monads][92]
-   2013-08 [Error Handling][93]
-   2013-08 [State monad][94]
-   2013-10 [Monads and Side Effects in Haskell][95] - Alan Davidson
-   2013-10 [A gentle intro to monads ... maybe?][96] - Sean Voisen
-   2013-12 [Monad transformers][97]
-   2013-12 [The tao of monad][98]
-   2013-12 [Monads in 15 minutes][99] - Nikolay Grozev
-   2013-12 [The First Monad Tutorial (slides)][100] - Philip Wadler

## year 2014

-   2014-01 [Monads in Haskell: Algebra][101]
-   2014-01 [Yet Another Monad Tutorial in 15 Minutes][102]
-   2014-01 [Using Monads in Haskell][103]
-   2014-01 [Monads in Haskell: Lists &c][104]
-   2014-01 [PROGRAMMING WITH EFFECTS][105]
-   2014-02 [monads made difficult][106]
-   2014-03 [Error Handling][107]
-   2014-03 [Monads are hard because...][108]
-   2014-04 [How continuation monad works][109]
-   2014-04 ["Mostly functional" programming does not work: Informal Introduction to Monads][110]
-   2014-10 [Refactoring Ruby with Monads][111]

## year 2015

-   2015-07 [Dr Frankenfunctor and the Monadster][112]
    
    "Or, how a 19th century scientist nearly invented the state monad."
    
-   2015-08 [Monads Demystified][113] - Josh Haberman
-   2015-11 [Briefly on the purpose of Functors, Applicatives and Monads][114]
-   2015-11 [A 5-Minute Monad Tutorial][115] - Andrew Hirsch

## year 2016

-   2016-04 [Monads are confusing. Let us help][116] - Kelley Robinson
-   2016-09 [A quick intro about Monads][117] - franzejr
-   2016-10 [Monads (forget about bind)][118] - Joakim Ahnfelt-Ronne

## year 2017

-   2017-03 [Functors & Monads: An Introduction][119] - Miguel Fonseca

-   2017-11 [Understanding Monads. A Guide for the Perplexed][120] - Barry Burd
    
    "With the current explosion of functional programming, the 'monad' functional structure is once again striking fear into the hearts of newcomers."
    

-   2017-11 [Learning Monads by Example][121] - Diego Vicente
    
    "While writing my bachelor thesis, a heuristic search framework in Haskell, I ran into a roadblock I long feared to have: I needed to understand how monads work."
    

## year 2019

-   2019-04 [Monads Tutorial][122] - *Monday Morning Haskell*
    
    "\[...\] here's my crack at a definition: A Monad wraps a value or a computation with a particular **context**."
    

-   2019-08 [Monads as a Programming Pattern][123] - Sam Grayson

-   "This article is written from a programmer’s perspective, where a monad is a *software engineering pattern*."

-   2019-12 [An Overview of the Monad][124] - Ragnhild Aalvik
    
    "In this article I will give a brief overview of what a *monad* is. \[...\] I will use metaphors to explain the concept, and I will *not* give any code examples in this article."
    

## year 2020

-   2020-07 [Yet another lousy monad tutorial][125] - Chankey Pathak
    
    " I like concrete explanations that start with practical examples, without any annoying metaphors, and especially without any Haskell code."
    

## year 2021

-   2021-01 [Simple Introduction to Monads][126] - Christian Neumanns
    
    "A simple step-by-step introduction to monads for developers with a background in non-pure-functional programming languages like C#, Java, Python, etc."
    

-   2021-03 [Merely monadic][127] - (*Haskell community*)
    
    "In Haskell, monadic types - types having an instance for the Monad class - can be thought of as abstract descriptors of computations which are inherently composable \[...\]"
    

-   2021-04 [Monads Explained][128] - Vidisha Jitani
    
    "So, it took me a lot of struggling to understand what exactly *“Monad”* is!"
    

-   2021-12 [Monads in a simple way][129] - Guilherme dos Reis Meira
    
    "Monads were created by mathematicians in 1960 and rediscovered by computer scientists in 1990 as a new way to handle effects."
    

[1]: https://wiki.haskell.org/File:Monad-tutorials-chart.png
[2]: https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.41.9361&rep=rep1&type=pdf
[3]: https://web.archive.org/web/19991018214519/http://www.dcs.gla.ac.uk/~nww/Monad.html
[4]: http://www-fp.dcs.st-and.ac.uk/~kh/papers/io-tutorial/io-tutorial.html
[5]: http://homepages.inf.ed.ac.uk/wadler/papers/marktoberdorf/baastad.pdf
[6]: http://www.engr.mun.ca/~theo/Misc/haskell_and_monads.htm
[7]: http://en.wikibooks.org/wiki/Haskell/YAHT/Monads
[8]: http://www.haskell.org/haskellwiki/All_About_Monads
[9]: http://www.sampou.org/haskell/a-a-monads/html/index.html
[10]: http://www.ccs.neu.edu/home/dherman/research/tutorials/monads-for-schemers.txt
[11]: https://wiki.haskell.org/Monads_as_Containers "Monads as Containers"
[12]: http://ru-lambda.livejournal.com/12467.html
[13]: http://web.archive.org/web/20080515195640/http://sleepingsquirrel.org/monads/monads.html
[14]: http://moonbase.rydia.net/mental/writings/programming/monads-in-ruby/00introduction.html
[15]: http://web.archive.org/web/20081206204420/http://www.loria.fr/~kow/monads/index.html
[16]: http://en.wikibooks.org/w/index.php?title=Haskell/Understanding_monads&oldid=933545
[17]: https://wiki.haskell.org/The_Monadic_Way "The Monadic Way"
[18]: https://wiki.haskell.org/Meet_Bob_The_Monadic_Lover "Meet Bob The Monadic Lover"
[19]: http://sigfpe.blogspot.com/2006/08/you-could-have-invented-monads-and.html
[20]: http://www.reddit.com/r/programming/comments/ox6s/ask_reddit_what_the_hell_are_monads/coxiv
[21]: http://www.grabmueller.de/martin/www/pub/Transformers.en.html
[22]: http://www.haskell.org/pipermail/haskell-cafe/2006-November/019190.html
[23]: http://blog.tmorris.net/maybe-monad-in-java/
[24]: http://koweycode.blogspot.com/2007/01/think-of-monad.html
[25]: http://kawagner.blogspot.com/2007/02/understanding-monads-for-real.html
[26]: http://patryshev.com/monad/m-intro.html
[27]: http://www.randomhacks.net/articles/2007/03/12/monads-in-15-minutes
[28]: http://saxophone.jpberlin.de/MonadTransformer?source=http%3A%2F%2Fwww%2Ehaskell%2Eorg%2Fhaskellwiki%2FCategory%3AMonad&language=English
[29]: https://wiki.haskell.org/Monads_as_computation "Monads as computation"
[30]: http://en.wikibooks.org/wiki/Haskell/Understanding%20monads
[31]: https://wiki.haskell.org/Monad_(sans_metaphors) "Monad (sans metaphors)"
[32]: http://www.reddit.com/r/programming/comments/64th1/monads_in_python_in_production_code_you_can_and/c02u9mb
[33]: http://book.realworldhaskell.org/read/monads.html
[34]: http://spbhug.folding-maps.org/wiki/Monads
[35]: http://spbhug.folding-maps.org/wiki/MonadsEn
[36]: http://stackoverflow.com/a/194207
[37]: http://blog.sigfpe.com/2008/11/from-monoids-to-monads.html
[38]: http://byorgey.wordpress.com/2009/01/12/abstraction-intuition-and-the-monad-tutorial-fallacy/
[39]: http://onclojure.com/2009/03/05/a-monad-tutorial-for-clojure-programmers-part-1/
[40]: http://noordering.wordpress.com/2009/03/31/how-you-shouldnt-use-monad/
[41]: http://www.muitovar.com/monad/moncow.html
[42]: http://brandon.si/code/the-state-monad-a-tutorial-for-the-confused/
[43]: http://data.tmorris.net/talks/what-does-monad-mean/what-does-monad-mean/chunk-html/index.html
[44]: https://wiki.haskell.org/What_a_Monad_is_not "What a Monad is not"
[45]: http://strabismicgobbledygook.wordpress.com/2010/03/06/a-state-monad-tutorial/
[46]: http://just-bottom.blogspot.fi/2010/04/programming-with-effects-story-so-far.html
[47]: https://intoverflow.wordpress.com/2010/07/20/i-come-from-java-and-want-to-know-what-monads-are-in-haskell/
[48]: http://learnyouahaskell.com/a-fistful-of-monads
[49]: http://mvanier.livejournal.com/3917.html
[50]: http://mvanier.livejournal.com/4305.html
[51]: http://mvanier.livejournal.com/4586.html
[52]: http://mvanier.livejournal.com/4647.html
[53]: http://mvanier.livejournal.com/5103.html
[54]: http://mvanier.livejournal.com/5343.html
[55]: http://mvanier.livejournal.com/5406.html
[56]: http://mvanier.livejournal.com/5846.html
[57]: http://stackoverflow.com/a/3870310
[58]: https://www.it.uu.se/edu/course/homepage/avfunpro/ht10/notes/glimming-4up.pdf
[59]: http://bartoszmilewski.wordpress.com/2011/01/09/monads-for-the-curious-programmer-part-1/
[60]: http://bartoszmilewski.wordpress.com/2011/03/14/monads-for-the-curious-programmer-part-2/
[61]: http://bartoszmilewski.wordpress.com/2011/03/17/monads-for-the-curious-programmer-part-3/
[62]: http://bartoszmilewski.wordpress.com/2011/07/11/monads-in-c/
[63]: http://scscript.blogspot.de/2011/03/monads-in-r-sapply-and-foreach.html
[64]: http://blog.jcoglan.com/2011/03/05/translation-from-haskell-to-javascript-of-selected-portions-of-the-best-introduction-to-monads-ive-ever-read/
[65]: http://ertes.de/articles/monads.html
[66]: http://web.archive.org/web/20120117061438/http://unknownparallel.com/monads.php
[67]: http://blog.g23.co/futures-monads-with-the-context-of-asynchrono
[68]: http://the-27th-comrade.appspot.com/blog/ahJzfnRoZS0yN3RoLWNvbXJhZGVyDAsSBUVudHJ5GOFdDA
[69]: http://youtu.be/Mw_Jnn_Y5iA
[70]: http://softwaresimply.blogspot.com/2012/04/less-travelled-monad-tutorial-part-1.html
[71]: http://softwaresimply.blogspot.ca/2012/04/ltmt-part-2-monads.html
[72]: http://cdsmith.wordpress.com/2012/04/18/why-do-monads-matter/
[73]: http://newartisans.com/2012/08/monads-in-pictures/
[74]: http://cswords.com/paper/alamode.pdf
[75]: https://www.kovach.me/posts/2013-01-02-intro-monads.html
[76]: http://blog.leahhanson.us/post/monad-tutorial.html
[77]: http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.html
[78]: http://www.alpheccar.org/content/60.html
[79]: http://www.alpheccar.org/content/61.html
[80]: http://www.alpheccar.org/content/67.html
[81]: http://www.alpheccar.org/content/74.html
[82]: http://www.alpheccar.org/content/76.html
[83]: http://www.alpheccar.org/content/77.html
[84]: http://www.alpheccar.org/content/86.html
[85]: http://www.alpheccar.org/content/87.html
[86]: http://www.alpheccar.org/content/90.html
[87]: http://mergeconflict.com/kleisli-composition-a-la-up-goer-five/
[88]: http://adit.io/posts/2013-06-10-three-useful-monads.html
[89]: http://www.stephendiehl.com/posts/monads.html
[90]: http://www.stephendiehl.com/posts/adjunctions.html
[91]: http://izbicki.me/blog/functors-and-monads-for-analyzing-data
[92]: https://www.fpcomplete.com/school/starting-with-haskell/basics-of-haskell/3-pure-functions-laziness-io
[93]: https://www.fpcomplete.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling
[94]: https://www.fpcomplete.com/school/starting-with-haskell/basics-of-haskell/12-State-Monad
[95]: https://www.cs.hmc.edu/~adavidso/monads.pdf
[96]: https://sean.voisen.org/blog/2013/10/intro-monads-maybe
[97]: http://www.cakesolutions.net/teamblogs/2013/12/29/monad-transformers/
[98]: https://www.fpcomplete.com/school/starting-with-haskell/basics-of-haskell/the-tao-of-monad
[99]: https://nikgrozev.com/2013/12/10/monads-in-15-minutes
[100]: https://homepages.inf.ed.ac.uk/wadler/papers/yow/monads-haskell.pdf
[101]: http://www.mjoldfield.com/atelier/2014/01/monads-algebra.html
[102]: http://www.idryman.org/blog/2014/01/23/yet-another-monad-tutorial/
[103]: http://profectium.blogspot.com/2014/01/using-monads-in-haskell.html
[104]: http://www.mjoldfield.com/atelier/2014/01/monads-list.html
[105]: http://www.cs.nott.ac.uk/~gmh/monads
[106]: http://www.stephendiehl.com/posts/monads.html
[107]: https://www.fpcomplete.com/school/starting-with-haskell/basics-of-haskell/10_Error_Handling
[108]: http://www.johndcook.com/blog/2014/03/03/monads-are-hard-because/
[109]: http://www.haskellforall.com/2014/04/how-continuation-monad-works.html
[110]: https://queue.acm.org/detail.cfm?id=2611829
[111]: http://codon.com/refactoring-ruby-with-monads
[112]: https://fsharpforfunandprofit.com/posts/monadster
[113]: https://blog.reverberate.org/2015/08/monads-demystified.html
[114]: https://codetalk.io/posts/2015-11-28-briefly-on-the-purpose-of-functors-applicatives-and-monads.html
[115]: https://www.cs.cornell.edu/~akhirsch/monads.html
[116]: https://engineering.sharethrough.com/blog/2016/04/18/explaining-monads-part-1
[117]: https://medium.com/@franzejr/a-quick-intro-about-monads-291e50dda062
[118]: https://www.ahnfelt.net/monads-forget-about-bind
[119]: https://lamda.blog/2017/03/27/functors-and-monads
[120]: https://www.infoq.com/articles/Understanding-Monads-guide-for-perplexed
[121]: https://diego.codes/post/learning-monads
[122]: https://mmhaskell.com/monads/tutorial
[123]: https://samgrayson.me/2019-08-06-monads-as-a-programming-pattern
[124]: https://www.bekk.christmas/post/2019/5/an-overview-of-the-monad
[125]: https://tutswiki.com/yet-another-lousy-monad-tutorial/
[126]: https://www.codeproject.com/Articles/5290753/Simple-Introduction-to-Monads
[127]: https://wiki.haskell.org/Merely_monadic "Merely monadic"
[128]: https://towardsdatascience.com/monads-from-the-lens-of-imperative-programmer-af1ab8c8790c
[129]: https://dev.to/kindsloth/monads-in-a-simple-way-7f9
