# MemoisingCafs - The Haskell Wiki

> The Wayback Machine - https://web.archive.org/web/20040104210032/http://www.haskell.org:80/hawiki/MemoisingCafs

The Wayback Machine - https://web.archive.org/web/20040104210032/http://www.haskell.org:80/hawiki/MemoisingCafs

[![HelpContents](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032im_/http://www.haskell.org/moinwiki/img/moin-help.gif)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/HelpContents) [![Search](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032im_/http://www.haskell.org/moinwiki/img/moin-search.gif)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/FindPage?value=MemoisingCafs) [![Diffs](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032im_/http://www.haskell.org/moinwiki/img/moin-diff.gif)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/MemoisingCafs?action=diff) [![Info](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032im_/http://www.haskell.org/moinwiki/img/moin-info.gif)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/MemoisingCafs?action=info) [![Edit](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032im_/http://www.haskell.org/moinwiki/img/moin-edit.gif)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/MemoisingCafs?action=edit) [![Subscribe](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032im_/http://www.haskell.org/moinwiki/img/moin-email-x.gif)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/MemoisingCafs?action=subscribe) [![XML](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032im_/http://www.haskell.org/moinwiki/img/moin-xml.gif)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/MemoisingCafs?action=format&mimetype=text/xml) [![Print](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032im_/http://www.haskell.org/moinwiki/img/moin-print.gif)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/MemoisingCafs?action=print) [![View](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032im_/http://www.haskell.org/moinwiki/img/moin-show.gif)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/MemoisingCafs)

<table><tbody><tr><td><span face="Arial,Helvetica" size="-1">&nbsp;<b>The Haskell Wiki</b>&nbsp;</span></td><td><span face="Arial,Helvetica" size="-1">&nbsp;<a href="chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/FrontPage">FrontPage</a>&nbsp;</span></td><td><span face="Arial,Helvetica" size="-1">&nbsp;<a href="chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/RecentChanges">RecentChanges</a>&nbsp;</span></td><td><span face="Arial,Helvetica" size="-1">&nbsp;<a href="chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/TitleIndex">TitleIndex</a>&nbsp;</span></td><td><span face="Arial,Helvetica" size="-1">&nbsp;<a href="chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/WordIndex">WordIndex</a>&nbsp;</span></td><td><span face="Arial,Helvetica" size="-1">&nbsp;<a href="chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/SiteNavigation">SiteNavigation</a>&nbsp;</span></td><td><span face="Arial,Helvetica" size="-1">&nbsp;<a href="chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/HelpContents">HelpContents</a>&nbsp;</span></td></tr></tbody></table>

* * *

[CAF](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/CAF)s and arrays can be used to memoize functions

eg. if we had a function which typically took a long time to calulate (i.e. not like the example that's coming up) we could memoise this using an array. This would incur some extra time the first time the function was evaluated but afterwards would return the result in constant time.

import Array 
 
isUppercase :: Char -> Bool 
isUppercase x = let ox = ord x 
                in (ox >= ord 'A' && ox <= ord 'Z') 
 
isUC x = uc!x 
 
uc:: Array Char Bool 
uc = array bnds \[ (x,isUppercase x) | x <- range bnds\] 
        where bnds = ((chr 0,chr 255) :: (Char,Char)) 

\-- [ChrisAngus](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/ChrisAngus)

* * *

Memoising constructor functions gives you [HashConsing](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/HashConsing), and you can sometimes use [MemoisingCafs](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/MemoisingCafs) to implement that.

The [MemoisingCafs](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/MemoisingCafs) idiom also supports recursion.

Consider, for example:

wonderous :: Integer -> Integer 
wonderous 1 = 0 
wonderous x 
  | x \`mod\` 2 == 0 = 1 + wonderous (x \`div\` 2) 
  | otherwise      = 1 + wonderous (3\*x+1) 

This function is not at all understood by mathematicians and has a surprisingly complex recursion pattern, so if you need to call it many times with different values, optimising it would not be easy.

However, we can memoise some of the domain using an array [CAF](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/CAF):

wonderous2 :: Integer -> Integer 
wonderous2 x 
  | x <= maxMemo = memoArray ! x 
  | otherwise    = wonderous2' x 
  where 
        maxMemo = 100 
        memoArray = array (1,maxMemo) 
                        \[ (x, wonderous2' x) | x <- \[1..maxMemo\] \] 
   
        wonderous2' 1 = 0 
        wonderous2' x 
          | x \`mod\` 2 == 0 = 1 + wonderous2 (x \`div\` 2) 
          | otherwise      = 1 + wonderous2 (3\*x+1) 

When using this pattern in your own code, note carefully when to call the memoised version (wonderous2 in the above example) and when not to. In general, the partially memoised version (wonderous2' in the above example) should call the memoised version if it needs to perform a recursive call.

Thanks to [LazyEvaluation](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/LazyEvaluation), we can even memoise an infinite domain, though we lose constant time lookup. This data structure is O(log N):

type MemoTable a = \[(Integer, BinTree a)\] 
data BinTree a = Leaf a | Node Integer (BinTree a) (BinTree a) 
  
wonderous3 :: Integer -> Integer 
wonderous3 x 
  = searchMemoTable x memoTable 
  where 
        memoTable :: MemoTable Integer 
        memoTable = buildMemoTable 1 5 
 
        buildMemoTable n i 
            = (nextn, buildMemoTable' n i) : buildMemoTable nextn (i+1) 
            where 
                nextn = n + 2^i 
 
                buildMemoTable' base 0 
                    = Leaf (wonderous3' base) 
                buildMemoTable' base i 
                    = Node (base + midSize) 
                           (buildMemoTable' base (i-1)) 
                           (buildMemoTable' (base + midSize) (i-1)) 
                    where 
                        midSize = 2 ^ (i-1) 
  
        searchMemoTable x ((x',tree):ms) 
            | x < x'    = searchMemoTree x tree 
            | otherwise = searchMemoTable x ms 
 
        searchMemoTree x (Leaf y) = y 
        searchMemoTree x (Node mid l r) 
            | x < mid   = searchMemoTree x l 
            | otherwise = searchMemoTree x r 
  
        wonderous3' 1 = 0 
        wonderous3' x 
          | x \`mod\` 2 == 0 = 1 + wonderous3 (x \`div\` 2) 
          | otherwise      = 1 + wonderous3 (3\*x+1) 

Naturally, these techniques can be combined, say, by using a fast [CAF](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/CAF) data structure for the most common part of the domain and an infinite [CAF](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/CAF) data structure for the rest.

\-- [AndrewBromage](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/AndrewBromage)

* * *

[CategoryIdiom](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032/http://www.haskell.org/hawiki/CategoryIdiom)

* * *

[![PythonPowered](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/web/20040104210032im_/http://www.haskell.org/moinwiki/img/PythonPowered.gif)](https://web.archive.org/web/20040104210032/http://www.python.org/)


[Source](https://web.archive.org/web/20040104210032/http://www.haskell.org/hawiki/MemoisingCafs)