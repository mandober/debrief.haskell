---
downloaded:       2022-01-03
page-url:         https://medium.com/twelve-days-of-monad/day-8-logict-a-list-transformer-a03dabba79cf
page-title:       Day 8: LogicT — A List Transformer | by Ben Clifford | Twelve Monads of Christmas | Medium
article-title:    Day 8: LogicT — A List Transformer
---
# Day 8: LogicT — A List Transformer | by Ben Clifford | Twelve Monads of Christmas | Medium

The list monad lets you perform non-deterministic searches for a solution to a problem. At each step of your program, you can provide a…
[][1]

The list monad lets you perform non-deterministic searches for a solution to a problem. At each step of your program, you can provide a choice of possible values to bind to a variable, and the rest of the program will run several times, trying each possible value and collecting all the results from every run into one thing. That is what `>>=` is for in `[]`.

Here’s a pretty contrived problem: we’re going to search all of the integers to see if there is an integer greater than 100.

First we need a list of all the integers, which we’ll get by combining the positives, zero and the negatives.

positives = \[1..\]  
negatives = map (\* (-1)) \[1..\]  
the\_numbers = positives ++ \[0\] ++ negatives

Now we can run a simple program to find the first ( `head` ) solution:

a\_solution = head $ do  
  n <- the\_numbers  
  guard (n > 100)  
  return n

We’ll go over all of the integers, and for each one, if it matches our search condition, we’ll return it, or otherwise we’ll abort this part of the search with no results.

\> **a\_solution**  
101

That answer is pretty obvious.

Let’s use the same technique to search for an integer less than 0. Can you think of such an integer? Hopefully. Here’s the program, just modified a little bit with our new condition:

a\_solution = head $ do  
  n <- the\_numbers  
  guard **(n < 0)**  
  return n

Alas, it hangs rather than giving an answer:

\> **a\_solution  
**...infinite wait...

This program checks `the_numbers` in order, and we defined it using `++` as first having all the (infinite number of) positive numbers, then 0, then the (infinite number of negatives). We won’t ever consider any negative numbers as possible solutions until we’ve checked all the positive numbers. `++` is unfairly favouring the left hand side over the right hand side.

## Fairer Disjunction

How about we use `interleave` instead of `++` ?

\> **the\_numbers = positives \`interleave\` \[0\] \`interleave\` negatives**  
\> **take 10 the\_numbers**  
\[1,-1,0,-2,2,-3,3,-4,4,-5\]

We’ve lost some of the ordering that we had before: our three lists are now all jumbled up with each other. But that means our search program will find a solution (on the second attempt, even).

## List Monad Transformers

Lots of monads have a transfomer version (sometimes, even, the basic version in terms of the transformer and `Identity`) — is there is sensible one for `[]`? So that we can get (for example) `ListT IO` which would act sometimes like a list and sometimes like `IO`.

Each step in `ListT IO` could run an IO action that returns many values, and the rest of the program will be run for each of those. Similarly, there could be `mplus` that works like `++` but on `ListT IO` actions.

A very simple implementation says a `ListT IO a` action is an embedded `IO [a]` action which runs and returns a list, and have `>>=` and `mplus` that work as in a regular list.

Like the earlier pure example, this works badly in the case of infinite lists: if the `IO` action that produces a list runs forever (producing values as it goes), the next step of the program is not even attempted once! The action needs to finish before it can return even the start of the result list — we’re far too strict in the first step.

A related problem arises if we try the “`interleave` instead of `++`” trick — we would need our three IO actions producing the three parts to have run for infinitely long before we can make the first test. Not only would we not find any negative answers, we wouldn’t find any positive ones either.

## LogicT to the rescue

This, broadly, is what `LogicT` sets out to do. It introduces a new primitive, `msplit`, with roughly this type signature: (very simplified)

msplit :: LogicT m a => LogicT m (Maybe (a, LogicT m a))

This works a little bit like a `case` match on a list: given a pure list, you can case match and get either the empty list or a pair: the first element of the list, and the rest of the list.

Given a `LogicT m` action `a`, running`msplit a` will either give you `Nothing` (like the empty list), or an element and a new action — analogous to the first element and (an action that will provide) the rest of the list. It will run the action for long enough to produce a single element, and then return control back to you.

Now an `interleave` can be defined which takes elements alternately from two `LogicT` actions and returns them interspersed with each other, without needing either of the supplied actions to fully complete — each one only gets to run long enough to produce one answer before the other one gets a chance. Here’s some pseudo-code based on the LogicT paper listed at the bottom:

interleave :: LogicT m a -> LogicT m a -> LogicT m a  
interleave l r = do  
  s <- msplit l  
  case s of  
    Nothing -> r  -- we've reached the end of l so all that  
                  -- remains is r  
    Just (s, rest) -> return s \`mplus\` interleave r rest  
       -- we got a value, so return it, and then interleave  
       -- the rest, starting with r first

There is a pile of other interesting stuff that `msplit` facilitates, covered in section 3 of the paper listed at the bottom — for example a fair version of `>>=` (did you know it wasn’t fair?) and some search pruning operations.

## See Also

Here’s the paper I usually read when I’m trying to understand LogicT. I found it pretty dense reading: [LogicT.pdf][2]

Day 6 of this blog, on conduits, also dealt with infinite lists of stuff and how to stick together programs which want to work with them — but the interface was very different to this.

[1]: https://medium.com/@benclifford?source=post_page-----a03dabba79cf-----------------------------------
[2]: http://okmij.org/ftp/Computation/LogicT.pdf
