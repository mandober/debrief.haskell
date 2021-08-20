# SimulatingDependentTypes - The Haskell Wiki

> The Wayback Machine - https://web.archive.org/web/20060203083622/http://www.haskell.org:80/hawiki/SimulatingDependentTypes

The Wayback Machine - https://web.archive.org/web/20060203083622/http://www.haskell.org:80/hawiki/SimulatingDependentTypes

Dependent Types
---------------

Dependent types allow you to catch complicated invariants in your code and data structures. In some cases, this may be accomplished by simply using better types ([?](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20060203083622/http://www.haskell.org/hawiki/NestedDatatypes)NestedDatatypes) but other times one truly needs the power of dependent types or simply dependent types may be much clearer in what they are accomplishing. Simulating dependent types in Haskell requires some widely available extensions ([?](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20060203083622/http://www.haskell.org/hawiki/MultiParameterTypeClasses)MultiParameterTypeClasses and [FunDeps](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20060203083622/http://www.haskell.org/hawiki/FunDeps)), and in many cases you need to allow the type checker the possibility of getting in an infinite loop (enabled with -fallow-undecidable-instances with GHC.) The following two links show some examples of (ab)using the type system in this way.

"Fun with Functional Dependencies"  
[http://www.cs.chalmers.se/~hallgren/Papers/wm01.html](https://web.archive.org/web/20060203083622/http://www.cs.chalmers.se/~hallgren/Papers/wm01.html)

"Haskell Does it with Class"  
[http://www.informatik.uni-bonn.de/~ralf/talks.html](https://web.archive.org/web/20060203083622/http://www.informatik.uni-bonn.de/~ralf/talks.html) (T24)

"Faking It (Simulating Dependent Types in Haskell)"  
[http://www.cs.nott.ac.uk/~ctm/faking.ps.gz](https://web.archive.org/web/20060203083622/http://www.cs.nott.ac.uk/~ctm/faking.ps.gz)

"implementing Cut Elimination: A Case Study of Simulating Dependent Types in Haskell"  
[http://types.bu.edu/reports/Chen+Zhu+Xi:PADL-2004.html](https://web.archive.org/web/20060203083622/http://types.bu.edu/reports/Chen+Zhu+Xi:PADL-2004.html)

A relevant post on the Haskell mailinglist:  
[http://haskell.org/pipermail/haskell/2003-April/011621.html](https://web.archive.org/web/20060203083622/http://haskell.org/pipermail/haskell/2003-April/011621.html) [http://www.haskell.org/pipermail/haskell/2003-April/011693.html](https://web.archive.org/web/20060203083622/http://www.haskell.org/pipermail/haskell/2003-April/011693.html)

An Example
----------

This is an example (using GHC) illustrating a technique to prove the invariant of AVL trees that all subtrees differ in height by at most one level.

{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}

data Zero = Zero
data Succ n = Succ n

type One    = Succ Zero
type Two    = Succ One
type Three  = Succ Two
type Four   = Succ Three
type Five   = Succ Four

class DiffersByAtMostOne l r

instance DiffersByAtMostOne n n

instance DiffersByAtMostOne (Succ n) n

instance DiffersByAtMostOne n (Succ n)

class Max l r n | l r -> n

instance Max Zero Zero Zero

instance Max Zero (Succ n) (Succ n)

instance Max (Succ n) Zero (Succ n)

instance (Max l r n) => Max (Succ l) (Succ r) (Succ n)

class Height t n | t -> n

instance Height (Leaf k \[http://hcnvfi.d5x5.net/vvbmhqx.htm v\]) Zero

instance (Height (l k v) hl,
          Height (r k v) hr,
          Max hl hr n) => Height (Branch l r k v) (Succ n)

class HeightDiffersByAtMostOne l r

instance (Height l hl,
          Height r hr,
          DiffersByAtMostOne hl hr) => HeightDiffersByAtMostOne l r

data Leaf k v = Leaf v
data (Ord k,
      HeightDiffersByAtMostOne (l k v) (r k v)) => Branch l r k v
  = Branch (l k v) k (r k v)

instance Show v => Show (Leaf k v) where
    show (Leaf v) = "(Leaf "++show v++")"

instance (Show k,
          Show v,
          HeightDiffersByAtMostOne (l k v) (r k v),
          Ord k,
          Show (l k v),
          Show (r k v)) => Show (Branch l r k v) where
    show (Branch l k r) = "(Branch "++show l++" "++show k++" "++show r++")"

ex1 = Branch (Leaf 'a') 2 (Branch (Leaf 'b') 3 (Leaf 'c'))

ex2 = Branch (Branch (Leaf 'a') 1 (Leaf 'b')) 2 (Leaf 'c')

ex3 = Leaf 'a'

-- type error, as it should
-- ex4 = Branch (Branch (Branch (Leaf 'a') 3 (Leaf 'b')) 2 (Leaf 'c')) 1 (Leaf 'd')

\-- [DerekElkins](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20060203083622/http://www.haskell.org/hawiki/DerekElkins) (Darius)

* * *

[CategoryIdiom](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20060203083622/http://www.haskell.org/hawiki/CategoryIdiom)


[Source](https://web.archive.org/web/20060203083622/http://www.haskell.org:80/hawiki/SimulatingDependentTypes)