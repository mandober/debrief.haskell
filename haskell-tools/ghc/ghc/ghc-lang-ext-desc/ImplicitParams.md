# ImplicitParams

Lang Pragma: `ImplicitParams`


```hs
{-# LANGUAGE ImplicitParams #-}

import Data.List (sortBy)

sortBy' :: (?cmp :: a -> a -> Ordering) => [a] -> [a]
sortBy' = sortBy ?cmp
sort :: Ord a => [a] -> [a]
sort = let ?cmp = compare in sortBy'

main = putStrLn (show (sort [3,1,2]))
```




https://wiki.haskell.org/Implicit_parameters

https://hackage.haskell.org/package/implicit-params-0.2.1#readme

https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/other-type-extensions.html#implicit-parameters

http://joyoftypes.blogspot.com/2012/02/haskell-supports-first-class-instances.html

http://joyoftypes.blogspot.com/2013/01/using-compiler-bugs-for-fun-and-profit.html

https://ocharles.org.uk/posts/2014-12-11-implicit-params.html

https://blog.csongor.co.uk/global-implicit-parameters/


http://hackage.haskell.org/package/data-default


Explicit call-stacks built via ImplicitParams
T:\code\haskell\ghc\ghc-9.0.1\libraries\base\GHC\Stack\Types.hs
