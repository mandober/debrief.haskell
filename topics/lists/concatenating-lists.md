# Concatenated lists

In Haskell, list is the primary representative of compound data types, it is a "default" data structure always ready and willing and thus used everywhere, even when circumstances call for a more appropriate type.

Operationally, Haskell's lists are singly-linked lists, known from the lisp time as the cons lists. Basically, just the two-cell nodes linked together. A node is usually implemented as a pair (a fat pointer) with two fields, commonly named `data` and `next`. The `data` is intended to point to a value, however, it may point to anything at all, even to another cons list (allowing for nested lists); the `next` points to the subsequent node of the current list.

As a data structure, singly linked-list don't offer much speed, however they might just be adequate in some cases.

Haskell issues
- List is in the `Prelude`
- `String` is in the `Prelude`
- `FilePath` is a type alias for `String`
- `String` is type alias for `[Char]`, so `String`s are a known waster of space, along with the other disadvantages that come with lists. Haskell does offer the `Text` datatype, but `String` is the "default" one, available in the Prelude, the same as the list (hand at heart, you can compose your own Prelude, but the standard is the standard…)

Advantages
- apart from prepend (cons) which is O(1), everything else is O(n) or worse
- in the `Prelude`
- lists, even their sublists, can be shared (a plus in pure FP)

Disadvantages
- too many pointers to follow, i.e. too much indirection
- append (snoc) is O(n)
- merging (++) two lists is O(n) in the length of the first
- great care required even for achieving the best bad complexity with many ops
  i.e. it is easy to screw things up beyond the usually achievable timings

Here's a hastly demo:

```hs
as, bs, cs, ds :: [Int]
as = [0..10_000]
bs = [10_000..100_000]
cs = [100_000..1_000_000]
ds = [1_000_000 .. 10_000_000]

rs1, rs2 :: [Int]
rs1 = as ++ bs ++ cs ++ ds
rs2 = (((as ++ bs) ++ cs) ++ ds)

x1 = rs1 !! 9_000_000 -- 8999997 (2.91 secs, 1,224,087,056 bytes)
x2 = rs2 !! 9_000_000 -- 8999997 (0.31 secs, 566,243,712 bytes)
```

Considering that (++) is right-associative, the expression 
`as ++ bs ++ cs ++ ds` is the same as 
`as ++ (bs ++ (cs ++ ds))`; forcing left-associativity with parens, as in 
`(((as ++ bs) ++ cs) ++ ds)`, really affects both time and space consumption.

Two lists cannot be concatenated any other way without changing the unerlying data structure (singly-linked or cons list).

Two lists like [1,2] and [3,4] can only be concatenated into a single list that respects the implicit oprdering i.e. [1,2,3,4], by prepending the left one onto the right. This means destructuring the left list, then prepending an element by element to the right list.

Going the other way, i.e. destructuring the right list in order to append it onto the left is possible, but far worse efficiency-wise; it would entail traversing the entire left list each time an element is about to get appended to its front. These traversals must be done in order to get at the last element (the last cons cell) so it can be set to point to a newly appended element (cons cell) instead of pointing to a marker that signifies the end of list. If lists would just maintain an extra pointer to the last element, appending would also be O(1).

```hs
(++) :: [a] -> [a] -> [a]
[]   ++ ys = ys
x:xs ++ ys = x: (xs ++ ys)

[1, 2] ++ ys =
1: [2] ++ ys = 1: (xs ++ ys)
2: []  ++ ys = 1: 2: (xs ++ ys)
   []  ++ ys = 1: 2: ([] ++ ys)
             = 1: 2: (      ys)
             = 1: 2: (   [3,4])
             = 1 : 2 : [3,4]
             = 1: (2: [3,4])
             = 1: [2,3,4]
             = [1,2,3,4]
```

The way it two lists are ,erged relies on recursion in that the elements of the left list are left on the stack frames until the last element is reached (which can be directly prepended onto the right list). Then the stack is unwound, allowing each element to be prepended to the right list, which may look as if the elements are prepended in their original order reversed, justifying the right-associativity of the cons operator, `:`.
