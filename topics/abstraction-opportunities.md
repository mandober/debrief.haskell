# Abstraction opportunities in Haskell

What things (on what levels) are suspect to abstraction in Haskell?

Abstracting over:
- term level
  - values
  - expressions
  - functions
  - names
- type level
  - type subsets
  - classes
  - methods
- class level
- module level
  - via cabal
- package level
  - via backpack


## Polymorhism

Polymorhism is the fundamental means of abstraction. 

There probably aren't too many things we'd like to abstract over in a dynamic PL since without the type constraints most things seem open and generic enough. For example, any function will gladly accept arguments of any type, even though it may not work with most. If we start with term-level values, Haskell already provides adequate abstractions via type parameters which we can constrain in different ways until we pinpoint the exact subset of types we wish to work with. These constraints are primarily enforced through type classes, which enable us to abstract over arbitrary subsets of types.


## Libraries

Libraries authors are often faced with dillema about which string implementation to rely on. The standard `Prelude.String`, being just an alias for `[Char]`, uses the plain old singly-linked list as its storage. Albeit a very wasteful solution, it is the default option solicited by the 'base' itself, therefore ubiquitous, deeply affectious and undying. The other implementation of strings does a UTF-8 encoding of its backing `ByteString` stoprage and answers to `Tex!` (no, it's a "t", your monitor's busted). Alas, it was all for nothing - library authors don't have the 2-choose-1 luxury, noo - instead they are forced to support both implementations, constantly degrading themselves doing copypasta programming, only to rejoice briefly as they make small adjustements (doing Haskell programming!) due to subtle differences between the two string types. Alas, there are other string types lurking within the standard, e.g. `FilePath`.

Backpack solves such problems by allowing you to **parameterize over a signature** rather than a concrete implementation of a data type! Go ahead and instantiate it when it suits you.

Because you are allowed to instantiate an indefinite library whenever you want, we can eagerly instantiate a posix-indef using String and ship it as posix, keeping backwards compatibility with all packages which are Backpack ignorant.

At the same time, if packages depend directly on posix-indef, they themselves are parametrizable over a string type. Entire library ecosystems can defer the choice of string type to the end user, which on a sufficiently new version of GHC offers an backwards compatible way of adding support for new string types to a library. (I don't want to say, support multiple string types, because this is not necessarily a virtue in-and-of-itself.)

http://blog.ezyang.com/2016/09/the-base-of-a-string-theory-for-haskell/


## Open-world and close-world types

ADTs fulfill a similar role to objects in other languages, but with more restrictions: objects are an open universe, where clients can implement new subclasses that were not known at definition time; *ADTs are a closed universe*, where the definition of an ADT specifies precisely all the cases that are possible.

We often think of restrictions of a bad thing, but in the case of ADTs, the restriction of being a closed universe makes programs easier to understand (a fixed set of cases to understand, as opposed to a potentially infinite set of cases) and allows for new modes of expression (pattern matching).

ADTs make it easy to accurately model your objects; they encourage you to write precise types that *make illegal states unrepresentable*.
