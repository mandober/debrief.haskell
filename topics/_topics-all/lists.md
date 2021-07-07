# Lists

## Prelude list

Haskell's builtin list datatype uses reserved keywords (`:`, `[]`) so it cannot be user-re-defined with those symbols, but if it could, it'd look like:

```hs
-- ...like this
data [a] = [] | a : [a]
-- i.e.
data [] a = [] | a : ([] a)
-- i.e.
data [] a = [] | (:) a ([] a)
-- i.e.
data [] a = [] | (:) (a, [] a)
```

Its type name is `[]`, which is also the type of the type ctor (parameterized by the type of list elements, `a`), and also the name of the nullary (nil) data ctor, which constructs the empty list.

Another thing to note is that Haskell's notation for a product type, such as `Cons a (List a)`, uses the simplified syntax, rather then the OCaml's product type syntax that uses explicit pairs with `*` keyword. 

```ocaml
type List 'a = Nil | Cons of 'a * List 'a
```

However, it is also possible to write this in Haskell using the explicit product type syntax i.e. as a pair, `Cons (a, List a)`, since a pair is the canonical product.

```hs
data List a = Nil | Cons  a (List a) ≅
data List a = Nil | Cons (a, List a) ≅
```






## Huges List as function

* To more efficiently concat lists or build strings compose sections made with `<>` or `++` functions.

```hs
s1 :: String -> String
s1 = ("abc" ++) . ("def" ++)  . ("ghi" ++) . ("jkl" ++) .
     ("mno" ++) . ("pqrs" ++) . ("tuv" ++) . ("wxyz" ++)
s2 :: String
s2 = s1 "" -- "abc cde ..."

-- ShowS
showS :: String -> String
showS s = (s ++)
```
