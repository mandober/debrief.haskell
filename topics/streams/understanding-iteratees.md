# Why iteratees are hard to understand
http://blog.ezyang.com/2012/01/why-iteratees-are-hard-to-understand/

There are two primary reasons why the low-level implementations of *iteratees*, *enumerators* and *enumeratees* tend to be hard to understand: purely functional implementation and inversion of control.

The users are encouraged to think of
- iteratees as sinks
- enumerators as sources
- enumeratees as transformers

This intuition works well for clients of iteratee libraries but confuses people interested in digging into the internals.

In this article, I'd like to explain the strangeness imposed by the purely functional implementation by comparing it to an imperative implementation. Concepts which are obvious and easy in an imperative setting are less-obvious but only slightly harder in FP.

## Types
