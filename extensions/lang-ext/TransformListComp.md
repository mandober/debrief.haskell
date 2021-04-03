# TransformListComp

- Pragma: `TransformListComp`
- https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/generalised_list_comprehensions.html#extension-TransformListComp
- enables quasi-SQL syntax in list comprehensions
- introduces 3 new keywords: `group`, `by`, `using`

```hs
import Data.List
import GHC.Exts

employees = [ ("Simon",  "MS",      80)
            , ("Erik",   "MS",      100)
            , ("Phil",   "Ed",      40)
            , ("Gordon", "Ed",      45)
            , ("Paul",   "Yale",    60)
            ]

-- sortWith and groupWith are ordinary functions from GHC.Exts
output = [ (the dept, sum salary)
         | (name, dept, salary) <- employees
         , then group by dept using groupWith
         , then sortWith by (sum salary)
         , then take 5
         ]

-- output = [("Yale", 60), ("Ed", 85), ("MS", 180)]
```
