# Inductive Properties of Infinite Lists

We know how to prove inductive properties over (finite) lists, but the question is now, how many of these properties hold for infinite lists as well?

For example, this relationship between `take`, `drop`, and `++`:   
take n xs ++ drop n xs = xs

is true even if `xs` is infinite. On the other hand, this property in not:    
reverse (reverse xs) = xs

If `xs` is an infinite list the left-hand side will not terminate, i.e. its value is `⟘`.

The special value ⟘ denotes both nonterminating and error-producing computations. Another way to view this value when reasoning about infinite lists is to think of *⟘ as an approximation to an answer*. More specifically, it is the least approximation (thus the name "bottom") that has absolutely no content whatsoever.

In Haskell, every data type, whether built-in or user-defined, has a ⟘ value. For example, every `Int` value can be approximated by ⟘. We can construct a polymorphic version of this value as follows:

```hs
bottom :: a
bottom = bottom
```

In the case of lists, the situation is more interesting, because every tail of a list is also a list, and so could also be approximated by ⟘. For example, the following are increasingly more accurate approximations to the list [1,2,3,4],culminating in the complete list itself:

```hs
⟘
1 : ⟘
1 : 2 : ⟘
1 : 2 : 3 : ⟘
1 : 2 : 3 : 4 : ⟘
1 : 2 : 3 : 4 : []
```

The most interesting is the case of infinite lists, which are mathematically *the limit of an infinite sequence of increasing approximations*. For example, the limit of this infinite sequence:

```hs
⟘
1 : ⟘
1 : 2 : ⟘
1 : 2 : 3 : ⟘
1 : 2 : 3 : 4 : ⟘
1 : 2 : 3 : 4 : 5 : ⟘
1 : 2 : 3 : 4 : 5 : 6 : ⟘
1 : 2 : 3 : 4 : 5 : 6 : 7 : ⟘
```

is the infinite list of natural numbers [1..].

⟘ and any list whose tail is ⟘ is called a *partial list*.

> Every infinite list can be described as the limit of an increasing sequence of partial lists.

Keep in mind that although ⟘ is conceptually a member of every data type in Haskell, and we can generate that value using something like `bottom`, any attempt to test for it, either through pattern-matching or an explicit equality test, will lead to nontermination, i.e. to ⟘ itself. Pragmatically, we can think of this task as checking for an error condition or nontermination, which, in general, is *provably impossible*.

Suppose that we have a property involving lists, called `P`, expressed as an equation in Haskell. If we can prove that this property is true for every partial list in a sequence whose limit is the infinite list `xs`, then the property `P` is also true of `xs`.

We use induction to prove that the property is true of every partial list. But now the base case is different from the one used for finite lists: instead of the empty list, `[]`, we now use the least defined list, ⟘, instead.

More precisely, to prove that a property `P`, expressed as an equation in Haskell, is true of all infinite lists, we proceed in two steps:
1. Prove `P(⟘)` (the base case)
2. Assuming that `P(xs)` is true (which is the induction hypothesis), prove that `P(x:xs)` holds (which is the induction step).

Curiously, why is the phrase "expressed as an equation in Haskell" used to constrain the property of interest? The problem is that the leap from a property being true for every partial list in an approximating sequence to the property being true for the infinite list itself is not justified for all properties. The simplest counter example is the property that states "the list is partial." This is trivially satisfied by every partial list, but is clearly not true of the limit, an infinite list. Fortunately, there is no way to write this property as an equation in Haskell, so that constraint is enough to guarantee that our proof technique is sound.

Consider the first example given earlier: `take n xs ++ drop n xs = xs`.

We can easily prove the base case:

```hs
take :: Int -> [a] -> [a]
take 0 _  = []
take _ [] = []
take n (x:xs)
  | n > 0 = x : take (n - 1) xs
  | otherwise = (x:xs)

drop :: Int -> [a] -> [a]
drop 0 xs = xs
drop _ [] = []
drop n (x:xs)
  | n > 0 = drop (n - 1) xs
  | otherwise = (x:xs)


take n ⟘ ++ drop n ⟘
⇒     ⟘ ++ drop n ⟘
⇒     ⟘
```
