# Type inference and constraints

https://cs3110.github.io/textbook/chapters/interp/inference.html

## Example: give hell to OCaml type checker

```ml hs
let b = true;;
let g = fun x -> x + 1;;
let f = fun x -> if b then g else fun y -> x y;;
let f = fun x -> if b then f else fun y -> x y;;
-- keep repeating the last line to see how,
-- as the types build up, the type checker
-- struggles more and more.
```


## Example: infer the type of exp

```
λf. λx. if b then (λx.f x) else (λy.x y)
```

## Example: infer the type of exp

```
λx. if b then (λx.x+1) else (λy.x y)
                                ↑
                              bound
```
