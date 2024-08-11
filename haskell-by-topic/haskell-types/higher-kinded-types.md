# Higher-Kinded Types

https://serokell.io/blog/kinds-and-hkts-in-haskell

https://diogocastro.com/blog/2018/10/17/haskells-kind-system-a-primer/


Types classify values and kinds classify types.

There is a universe of values populated by values like `True`, `Just 42`, `[1, 2, 3]`, `(\ n -> even n)`, etc. 

There is also a universe of types that classify these values:





- value `Just 42` has type `Maybe Int`, i.e. `Just 42 :: Maybe Int`
- `[1, 2, 3] :: List Int`
- `(\ n -> even n) :: Int -> Bool`

- `(Int -> Bool) -> Int -> String`


And we can imagine a universe of types governing those values, with types such as String, Bool, and [Int].
