# Let and where


## where bindings

- where clause
- where clause is a syntactic construct
- where keyword
- where keyword has other uses (module...where, instance...where, GADT)
- where binding
- nested where clause


Unlike the `let`, which is an expression, the `where` is just a syntactic construct.

We can put the keyword `where` after the guards (indented as much as the guard pipes) and then we can define temporary names for reapeated computations. The names we define will be visible across the guards, thereby saving us from having to repeat ourselves.

```hs
tell :: (RealFloat a) => a -> a -> String
tell weight height
    | bmi <= lo  = "dying"
    | bmi <= med = "skinny"
    | bmi <= hi  = "obese"
    | otherwise  = "fat fuck"
    where bmi = weight / height ^ 2
          (lo, med, hi) = (20.0, 25.0, 35.0)
```

The *where bindings* are not shared across function bodies of different patterns (in a piece-wise function definition). If you want several patterns of one function to access some shared name, you have to define it globally.

The *where bindings* (like any binding site) can be used for pattern matching:

```hs
initials :: String -> String -> String
initials fname lname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = fname
          (l:_) = lname

-- Notice that PM a string with (x:xs) gives (Char:String)
-- so to turn x back from Char into a string, do [x]
```

The *where bindings* can also be nested. It's a common idiom to make a function and define some helper function in its *where clause* and then to give the helper function its helper function as well, each with its own where clause.


The `where` keyword has other uses beyond where clause; it is used in:
- module declarations: `module ... where`
- instance declarations: `instance ... where`
- in GADT declarations: `data T a = T a | ... where T ...`


## Let bindings

*Where bindings* are a syntactic construct that let you bind values to variables at the end of a function and the whole function can see them, including the guards.

**Let bindings** let you bind to variables anywhere, but are very local - they don't span across guards.

Another difference is that the where clause is is a syntactic construct (where-statement), while *let* is an expression, just like *if-then-else* is an expression.

Like any other binding site, *let bindings* can be used for pattern matching.




## Let

The keyword `let` is used in 3 ways in Haskell:
- let-expression
- let-statement
- let-statement in list comprehensions


The first form is a **let-expression**    
`let name = body in expr`

that can be used wherever an expression is allowed, e.g.    
`(let x = 2 in x*2) + 3`


The second is a **let-statement**.

This form is only used inside of do-notation, and does not use in.

do statements
   let variable = expression
   statements


The third is similar to number 2 and is used inside of list comprehensions. Again, no in.

> [(x, y) | x <- [1..3], let y = 2*x]
[(1,2),(2,4),(3,6)]

This form binds a variable which is in scope in subsequent generators and in the expression before the |.
