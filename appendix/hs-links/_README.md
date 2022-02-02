

https://sookocheff.com/post/fp/introducing-lambda-calculus/
https://sookocheff.com/post/fp/evaluating-lambda-expressions/
https://sookocheff.com/post/fp/typed-lambda-calculus/
https://sookocheff.com/post/bash/bash-string-operators/
https://sookocheff.com/post/fp/alpha-conversion/
https://sookocheff.com/post/fp/beta-reduction/
https://sookocheff.com/post/fp/eta-conversion/
https://sookocheff.com/post/fp/representing-pairs-and-lists-in-lambda-calculus/
https://sookocheff.com/post/fp/recursive-lambda-functions/
https://sookocheff.com/post/fp/simplifying-lambda-syntax/
https://sookocheff.com/post/fp/introducing-lambda-calculus/
https://sookocheff.com/post/fp/domain-range-codomain/
https://sookocheff.com/post/fp/a-functional-learning-plan/
https://sookocheff.com/post/fp/differences-between-imperative-and-functional/
https://sookocheff.com/post/fp/why-functional-programming/
https://sookocheff.com/post/fp/what-is-functional-programming/



https://crypto.stanford.edu/~blynn/lambda/
http://dev.stephendiehl.com/fun/lambda_calculus.html

https://sookocheff.com/post/fp/introducing-lambda-calculus/
https://sookocheff.com/post/fp/evaluating-lambda-expressions/
https://sookocheff.com/post/fp/typed-lambda-calculus/

https://blog.ssanj.net/posts/2018-03-05-functor-applicative-and-monad-instances-for-reader.html
https://blog.ssanj.net/tags/haskell.html


# How to learn Haskell

This is a recommended path for learning Haskell based on experience helping others. A list of recommendations from one of the authors of the [Haskell Book.](http://haskellbook.com)

#### _Don't sweat the stuff you don't understand immediately_. Keep moving!

## Community

Our IRC channel is `#haskell-beginners` on Freenode.

IRC web client [here](http://webchat.freenode.net/).

The haskell [mailing lists](https://wiki.haskell.org/Mailing_lists).

### Community Guidelines

See [the community guidelines](coc.md) to understand the conduct that is expected in the IRC channel. You'll get a warning if you're not obviously trolling, but be aware the channel is exclusively for those learning or teaching Haskell.

# Installing Haskell

## Use Stack to get going with Haskell

Get [Stack](http://haskellstack.org) to get GHC installed and to build your projects.

If you don't know anything about Stack and would like an overview, check out this [comprehensive Stack video tutorial](https://www.youtube.com/watch?v=sRonIB8ZStw).

## Also, DO NOT INSTALL HASKELL PLATFORM

Instead of following the instructions on Haskell.org, get Stack.

### Why not platform?

https://mail.haskell.org/pipermail/haskell-community/2015-September/000014.html

# How should I learn Haskell?

The core recommendation is to read the lectures and complete all exercises/homework for the Spring 13 version of cis194 followed by the FP course. Both are linked below. Everything else can be considered optional and is mentioned so you know where to look.

## Haskell Programming from First Principles.

[@dmvianna](https://github.com/dmvianna) wanted me to let you know that the below are just the _free_ recommended resources. If you're willing to check out a book, we heartily recommend our own [Haskell Book!](http://haskellbook.com) If you can't afford the book for any reasons, please email us using the contact information at [our support page](http://haskellbook.com/support.html).

### Haskell Book subsumes all of the primary resources recommended here

## Yorgey's cis194 course

> _Do this first_ if aren't getting the Haskell Book, this is the best _free_ introduction to Haskell.

Available [online](http://www.seas.upenn.edu/~cis194/spring13/lectures.html).

[Brent Yorgey](https://byorgey.wordpress.com)'s course is the best I've found so
far. This course is valuable as it will not only equip you to write basic
Haskell but also help you to understand parser combinators.

The only reason you shouldn't start with cis194 is if you are not a programmer
or are an inexperienced one. If that's the case, start with
[Thompson's book](http://www.haskellcraft.com/craft3e/Home.html) and transition
to cis194.

---

## Functional Programming course

> This is the course we recommend doing after Yorgey's cis194 course

Available on github [here](https://github.com/bitemyapp/fp-course).

This will reinforce and give you experience directly implementing the
abstractions introduced in cis194, this is practice which is _critical_ to
becoming comfortable with everyday uses of Functor/Applicative/Monad/etc. in
Haskell. Doing cis194 and then the FP course represents the core
recommendation of my guide and is how we teach everyone Haskell.

---

## Supplementary course after cis194 and the FP course

> Provides more material on intermediate topics

cs240h is available online:

- [Spring 14](http://www.scs.stanford.edu/14sp-cs240h/)
- [Winter 16](http://www.scs.stanford.edu/16wi-cs240h/)

This is [Bryan O'Sullivan](https://github.com/bos)'s online course from the
class he teaches at Stanford. If you don't know who he is, take a gander at half
the libraries any Haskell application ends up needing and his name is on it. Of
particular note if you've already done the Yorgey course are the modules on
phantom types, information flow control, language extensions, concurrency,
pipes, and lenses.

---

# Resources for specific topics in Haskell

These resources are not vetted or tested with learners as cis194 and FP course have been, but they're linked in [the topic listing](specific_topics.md) so you have ideas on where to begin. This includes things like intermediate/advanced concepts and subjects like tooling and text editors.

## Dialogues

> Hosted in this repository [here](../../terms/__unsorted/dialogues.md).

These are actually pretty important and helpful. Look here for deep dives on a
variety of topics.
