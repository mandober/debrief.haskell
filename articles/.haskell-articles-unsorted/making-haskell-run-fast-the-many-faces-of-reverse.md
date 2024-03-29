# Making Haskell run fast: the many faces of reverse

> A blog about functional programming

Posted on September 13, 2019

These three versions of `reverse` are curiously related. The comparison will exemplify the role of laziness and purity in optimizations of functional programs, and the need for better tools to help performance tuning evolve from arcane art to science.

Extensions and imports for this Literate Haskell file

    {-# LANGUAGE TemplateHaskell #-}
    
    module Reverse where
    
    import Prelude hiding (reverse)
    import Test.Inspection (inspect, (===))

Declarative `reverse`
---------------------

The _declarative_ definition answers the question: _what_ is the reverse of a list?

It depends on the list. If it is empty, then its reverse is also the empty list. If it is not empty, `x : xs`, then the last element of the reverse is the head `x`, and the rest is the reverse of the tail `xs`.

    reverse :: [a] -> [a]
    reverse [] = []
    reverse (x : xs) = reverse xs ++ [x]

Step-by-step example:

        reverse (1 : 2 : 3 : [])
    =   reverse     (2 : 3 : [])                ++ [1]
    =  (reverse         (3 : [])        ++ [2]) ++ [1]
    = ((reverse              [] ++ [3]) ++ [2]) ++ [1]
    =                      (([] ++ [3]) ++ [2]) ++ [1]
    ... (simplifying (++))
    = 3 : 2 : 1 : []

Remarking that this function (in particular, the steps skipped at the end of the example above) takes time quadratic in the length of the input list motivates a more operational point of view.

Imperative `reverse`
--------------------

The question for the _imperative_ definition is: _how_ to construct the reverse of a list?

By answering the _how_, we can be careful to only work for a constant amount of time for each element, so that overall the function works in time linear in the length of the list.

Given a list, we can take its elements off one by one to build up another list next to it, which will be the reverse of the original list.

This solution is also known as the “tail-recursive `reverse`”.

    reverse' :: [a] -> [a]
    reverse' xs = revApp xs []
    
    -- revApp xs ys = reverse xs ++ ys
    revApp :: [a] -> [a] -> [a]
    revApp [] acc = acc
    revApp (x : xs) acc = revApp xs (x : acc)

Step-by-step example:

      revApp (1 : 2 : 3 : [])              []
    = revApp     (2 : 3 : [])         (1 : [])
    = revApp         (3 : [])     (2 : 1 : [])
    = revApp              []  (3 : 2 : 1 : [])
    = (3 : 2 : 1 : [])

Declarative, but faster
-----------------------

Another way to arrive at a linear-time `reverse` is to find the cause behind the slowness of the first version and then fix it.

If `n` is the length of the list, there is one `n` factor because of `reverse` calling itself `n` times. For each of those times, we apply `(++)` once, which is the other `n` factor.

But the cost of `(++)` is entirely up to the representation of lists. If we pick a representation with constant-time concatenation, then the declarative definition will give a linear-time function. One such representation is _difference lists_.

We first build a small library of difference lists.

    type DList a = [a] -> [a]
    
    empty     :: DList a
    singleton :: a -> DList a
    (++.)     :: DList a -> DList a -> DList a
    toList    :: DList a -> [a]

`DList` implementation

    type DList a = [a] -> [a]
    
    empty :: DList a
    empty = id
    
    singleton :: a -> DList a
    singleton y = (y :)
    
    (++.) :: DList a -> DList a -> DList a
    (++.) = (.)
    
    toList :: DList a -> [a]
    toList ys = ys []

Now, take the declarative definition, and replace list operations with those from that difference list library, and finally convert the result to the standard list representation.

    -- reverse, where the result is a difference list
    reversed :: [a] -> DList a
    reversed [] = empty
    reversed (x : xs) = reversed xs ++. singleton x
    
    reverse'' :: [a] -> [a]
    reverse'' = toList . reversed

Correctness
-----------

Here we shall consider an implementation “correct” if it is equivalent to a “reference implementation”, which we’ll arbitrarily elect to be the “declarative” `reverse`.

The body of the function `reverse''` is essentially the same as the “reference” `reverse`, and in that sense, we could say that it is “obviously correct”.

More formally, to prove that `reverse` and `reverse''` are equivalent, we can walk through both definition and relate every `DList` on one side with a list on the other side. Here’s a [proof in Coq](https://gist.github.com/Lysxia/d6c655f89f46bf5f2169c234e8f12dc1).

In contrast, to prove directly that the “imperative” `reverse'` is equivalent to the “declarative” `reverse`, the invariant is more ad hoc to come by, even though in the `DList` version, the same invariant turns out to be there too, it’s just hidden by the `DList` abstraction.

Although their destinations may be the same, it seems worth looking back at the different journeys behind `reverse'` and `reverse''` to hopefully learn more general principles which can guide us to write correct and performant programs (that I have yet to figure out, don’t expect to find any clear answers here).

Performance
-----------

Both `reverse'` and `reverse''` evaluate in time which grows linearly with the length of the input list, that was the whole point of the operation.

But there is more to performance than asymptotic complexity. The “imperative” `reverse'` relies on a tail-recursive auxiliary function `revApp`, so it can easily be compiled to a simple loop where most of the work goes directly into constructing the reversed list.

The situation with the difference-list based `reverse''` is less clear. Even if we admit that `(++.)` has a constant cost, there seems to be a fair amount of overhead compared to “imperative” `reverse'`: it is not tail-recursive, and it builds up a long chain of functions which is applied only at the end. As cheap as function calls may be, it seems quite hopeless to shed all of that weight to catch up to `reverse'`.

And yet, simply by making explicit the second argument of `reversed`, with a bit of rewriting, `reversed` transforms into `revApp` (the meat of “imperative” `reverse'`):

    reversed :: [a] -> DList a
    revApp   :: [a] -> [a] -> [a]  -- same type
    
    -- same definition as revApp
    reversed [] zs = zs
    reversed (y : ys) zs = reversed ys (y : zs)

Detailed equations

      reversed [] zs
    = empty zs
    = zs
    
      reversed (y : ys) zs
    = (reversed ys ++. singleton y) zs
    = reversed ys (singleton y zs)
    = reversed ys (y : zs)

Thanks to that, the glorious GHC compiles both the “declarative” `reverse''` and the “imperative” `reverse'` to identical Core terms.

### Laziness and purity

Haskell is actually in a quite privileged position here: to a certain extent, such an optimization is enabled by laziness and purity, the two main distinguishing features of Haskell.

To see why, take another look at this equation in the definition of `reversed` (in `reverse''`):

    reversed (y : ys) = reversed ys ++. singleton y

`(++.)` is merely function composition `(.)`, so we would like to rewrite that as follows:

    reversed (y : ys) = \zs -> reversed ys (singleton y zs)

But in an eagerly-evaluated language, that transformation delays the evaluation of `reversed ys`, which is valid only if it _terminates with no side effects_. That may be true in this case, but how many actual compilers do infer that information?[1](#fn1)

That reflects what the [haskell.org](https://haskell.org/) site says about laziness:

> Functions don’t evaluate their arguments. This means that programs can compose together very well, with the ability to write control constructs (such as if/else) just by writing normal functions. The purity of Haskell code makes it easy to fuse chains of functions together, allowing for performance benefits.

Laziness and purity make a pretty broad double-edged sword to optimize functional programs. It allows writing performant programs using high-level abstractions, however, the cost model is notoriously hard to grasp, especially beyond asymptotics. We _can_ build fast programs in Haskell, but that alone is not good enough. What does it take to do so _reliably_?

### Inspection testing

Inlining and partial evaluation seem to inherently make the cost model non-compositional, so that we have to know what the code generated by the compiler looks like. But that’s quite tedious, so there should be tools to assist us in spotting patterns of efficient and inefficient code. One such tool (the only one I am aware of in my limited knowledge) is the [_inspection-testing_](https://hackage.haskell.org/package/inspection-testing) library.

For example, here is a test that the fast “declarative” `reverse''` and the “imperative” `reverse'` are compiled to the same Core terms:

    inspect $ 'reverse'' === 'reverse'

When we compile this file (with optimizations), we get the following output confirming our claim:

    posts/2019-09-13-reverse.md:278:1: reverse'' === reverse' passed.
    inspection testing successful
          expected successes: 1

Of course, we can write that test here because we happen to have two functions which should compile to the same Core. It works well for unit-testing metaprograms (programs which generate programs), but it’s ill-suited to test the optimization of application-level code. _inspection-testing_ offers a few other properties which are correlated with “well optimized” in appropriate situations, and there are definitely many more left to discover.

Inspired by
-----------

*   _A novel representation of lists and its application to the function `reverse`_, by John Hughes, 1984. ([PDF](https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/lists.pdf))
    
*   _Why functional programming matters_, by John Hughes, 1990. ([PDF](http://www.cse.chalmers.se/~rjmh/Papers/whyfp.pdf))
    
*   [The many faces of `isOrderedTree`](https://www.youtube.com/watch?v=xcm_H36v_18&feature=youtu.be), talk by Joachim Breitner, MuniHac 2019.
    
*   The [_dlist_ library](http://hackage.haskell.org/package/dlist-0.8.0.7) on Hackage (the README contains many links about differences lists).
    

* * *

1.  In OCaml we can also construct infinite, cyclic lists: `let rec xs = 1 :: xs`. That makes an applicable requirement for this code transformation even more complicated to describe.[↩︎](#fnref1)


[Source](https://blog.poisson.chat/posts/2019-09-13-reverse.html)