# Constant Applicative Form


**Constant Applicative Form** is any supercombinator which is not a lambda abstraction, including truly constant expressions such as `12`, `((+) 1 2)`, `[1,2,3]` as well as partially applied functions such as `((+) 4)`. Note that this last example is equivalent under eta abstraction to `\x -> (+) 4 x` which is not a CAF (!?).

A better way of thinking about CAFs is that a CAF is anything that can be *let floated* (see *lifting pattern*) to the top level. So a CAF can not only call other CAFs, but also other *super combinators*. I believe that's already granted, according to the definition of a super combinator. A CAF may reference any super combinator simply by virtue of being one.

A **supercombinator** is either a constant, or a combinator which contains only supercombinators as subexpressions.


**Combinators** derive from combinatory logic and consist of functions which only apply the values passed in to one another in one or another form - i.e. they combine their arguments.

The most famous set of combinators are S, K, I, which taken together are Turing-complete.

**Supercombinators**, in this context, are functions built only of values passed in, combinators, and other supercombinators. Hence any supercombinator can be expanded, through substitution, into a plain old combinator.

Some compilers for functional languages (not GHC!) use combinators and supercombinators as intermediate steps in compilation. As with any similar compiler technology, the reason for doing this is to admit optimization analysis that is more easily performed in such a simplified, minimal language.

Constant Applicative Forms are something else entirely. They're a bit more subtle, and have a few gotchas. The way to think of them is as an aspect of compiler implementation with no separate semantic meaning but with a potentially profound effect on runtime performance. The following may not be a perfect description of a CAF, but it'll try to convey my intuition of what one is, since I haven't seen a really good description anywhere else for me to crib from. The clean "authoritative" description in the GHC Commentary Wiki reads as follows:

    Constant Applicative Forms, or CAFs for short, are top-level values defined in a program. Essentially, they are objects that are not allocated dynamically at run-time but, instead, are part of the static data of the program.


That's a good start. Pure, functional, lazy languages can be thought of in some sense as a graph reduction machine. The first time you demand the value of a node, that forces its evaluation, which in turn can demand the values of subnodes, etc. One a node is evaluated, the resultant value sticks around (although it does not have to stick around -- since this is a pure language we could always keep the subnodes live and recalculate with no semantic effect). A CAF is indeed just a value. But, in the context, a special kind of value -- one which the compiler can determine has a meaning entirely dependent on its subnodes. That is to say:

So why do we care if things are CAFs? Basically because sometimes we really really don't want to recompute something (for example, a memotable!) and so want to make sure it is shared properly. Other times we really do want to recompute something (e.g. a huge boring easy to generate list -- such as the naturals -- which we're just walking over) and not have it stick around in memory forever. A combination of naming things and binding them under lets or writing them inline, etc. typically lets us specify these sorts of things in a natural, intuitive way. 

Occasionally, however, the compiler is smarter or dumber than we expect, and something we think should only be computed once is always recomputed, or something we don't want to hang on to gets lifted out as a CAF. Then, we need to think things through more carefully. See this discussion to get an idea about some of the trickiness involved: *A good way to avoid "sharing"?*
https://stackoverflow.com/questions/6614023/a-good-way-to-avoid-sharing





---

https://stackoverflow.com/questions/8330756/what-are-super-combinators-and-constant-applicative-forms

---

(These Haskell wiki pages you reference are old, and I think unfortunately written. Particularly unfortunate is that they mix up CAFs and supercombinators. Supercombinators are interesting but unrelated to GHC. CAFs are still very much a part of GHC, and can be understood without reference to supercombinators.)

---

[The Implementation of Functional Programming Languages, Simon Peyton Jones].


(CAF) A supercombinator which is not a lambda abstraction. This includes truly constant expressions such as 12, (+ 1 2), [1, 2, 3] as well as partially applied functions such as (+ 4). Note that this last example is equivalent under eta abstraction to \ x . + 4 x which is not a CAF.

Since a CAF is a supercombinator, it contains no free variables. Moreover, since it is not a lambda abstraction it contains no variables at all. It may however contain identifiers which refer to other CAFs, e.g.

c 3 where c = (* 2).

A CAF can always be lifted to the top level of the program. It can either be compiled to a piece of graph which will be shared by all uses or to some shared code which will overwrite itself with some graph the first time it is evaluated. A CAF such as

ints = from 1 where from n = n : from (n+1)

can grow without bound but may only be accessible from within the code of one or more functions. In order for the garbage collector to be able to reclaim such structures, we associate with each function a list of the CAFs to which it refers. When garbage collecting a reference to the function we collect the CAFs on its list.

[The Implementation of Functional Programming Languages, Simon Peyton Jones].

---


https://wiki.haskell.org/Constant_applicative_form
https://wiki.haskell.org/Memoization#Memoising_CAFS
https://wiki.haskell.org/Lifting_pattern
https://wiki.haskell.org/Super_combinator
https://stackoverflow.com/questions/8330756/what-are-super-combinators-and-constant-applicative-forms


---


Memoizing polymorphic functions
http://conal.net/blog/posts/memoizing-polymorphic-functions-part-one/
http://conal.net/blog/posts/memoizing-polymorphic-functions-part-two/

[Memoizing polymorphic functions via unmemoization
http://conal.net/blog/posts/memoizing-polymorphic-functions-via-unmemoization/

speeding up fibonacci with memoizing
http://www.haskell.org/pipermail/haskell-cafe/2007-February/021288.html

Elegant memoization with functional memo tries
http://conal.net/blog/posts/elegant-memoization-with-functional-memo-tries

[Haskell-Cafe about memoization utility function](http://www.haskell.org/pipermail/haskell-cafe/2007-May/024689.html)
[Haskell-Cafe "memoisation"](http://www.haskell.org/pipermail/haskell-cafe/2007-February/021563.html)
[Haskell-Cafe about Memoization and Data.Map](http://www.haskell.org/pipermail/haskell-cafe/2005-October/010287.html)
[http://programming.reddit.com/info/16ofr/comments](http://programming.reddit.com/info/16ofr/comments)
[Monadic Memoization Mixins](http://www.cs.utexas.edu/~wcook/Drafts/2006/MemoMixins.pdf)
[data-memocombinators: Combinators for building memo tables.](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/data-memocombinators)
[MemoTrie: Trie-based memo functions](http://hackage.haskell.org/cgi-bin/hackage-scripts/package/MemoTrie)
[monad-memo: memoization monad transformer](http://hackage.haskell.org/package/monad-memo)
[memoize: uses Template Haskell to derive memoization code](http://hackage.haskell.org/package/memoize)
[array-memoize: memoize finite (and/or discrete) sub-domains of a function using arrays](http://hackage.haskell.org/package/array-memoize)
