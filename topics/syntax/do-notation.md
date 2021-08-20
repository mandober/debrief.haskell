topics:
  - do-notation
  - simplification rules for the do notation
  - desugaring do-notation
---
# do-notation


```hs
do {      e    } = e                        -- (1)
do {      e; s } = e >>        do { s }     -- (2)
do { x <- e; s } = e >>= \x -> do { s }     -- (3)
```


*do-expression* provides a convenient syntax for writing monadic expressions.

1. The `do` keyword encompassing a single action is translated into that action alone.

2. The `do` keyword encompassing more then one unparameterized action is translated into the first action, followed by `>>`, followed by the `do` keyword encompassing the remaining actions. In a way, the first action is pulled out of the `do` block, but still retaining the connection with it through the `>>` operator. When this rule is applied repeatedly, the entire `do` block is dissolved into a chain of actions that are connected together with the `>>` operators.

3. Parameterized action is translated using (>>=)

On the left of the <- is a normal Haskell pattern. This can be a single variable or something more complicated. A guard expression is not allowed.

This pattern is translated into a let binding that declares a local function with a unique name (we're just using f as an example above). The action on the right of the <- is then chained with this function using (>>=).

What's noteworthy about this translation is that if the pattern match fails, the local function calls the monad's fail implementation. Here's an example using the Maybe monad:

```hs
robust :: [a] -> Maybe a
robust xs = do
    (_:x:_) <- Just xs
    return x
```

> NOTE: the `fail` method was moved from `Monad` into its own `MonadFail` class.


4. `let` bound params

Finally, when we write a `let` expression in a `do` block, we can omit the usual `in` keyword (for complete construct is `let ... in`). Subsequent actions in the block must be lined up with the `let` keyword.


```hs
-- ----------------------------------------------------------------------------
do {      e    } = e                        -- (1)
do {      e; s } = e >>        do { s }     -- (2)
do { x <- e; s } = e >>= \x -> do { s }     -- (3)
-- ----------------------------------------------------------------------------



-- (1)

-- using layout:
do1 = do
    act

-- or without layout:
do1 = do { act }

-- in any case, it gets translated into:
tr1 = act


-- ----------------------------------------------------------------------------
-- (2)

-- using layout:
do2 = do
    act1
    act2
    {- etc. -}
    actN

-- without the layout:
do2 = do { act1; act2; {- ... -} actN }

-- unparameterized action is translated using (>>)
tr2a =
    act1 >>
    do act2
       {- etc. -}
       actN

-- without the layout:
tr2a = act1 >> do { act2; {- ... -} actN }

-- then the subsequent paramless actions are translated one by one
tr2b =
    act1 >>
    act2 >>
    {- etc. -}
    actN

-- without the layout:
tr2b = act1 >> act2 >> {- ... -} actN

-- ----------------------------------------------------------------------------
-- (3)

-- parameterized action is translated using (>>=)
do3 = do
    x <- act1
    y <- act2
    z <- act3

tr3 = act1 >>= \x -> act2 >>= \y -> act3 >>= \z -> atc3


-- before, when fail method was still in the Monad (now in MonadFail)
do4 = do
    x <- act1
    act2
    act3

tr4 =
    let f x = do
        act2
        act3
        f _ = fail "..."
    in act1 >>= f

-- ----------------------------------------------------------------------------
-- (4)

-- let binding
do4 = do
    let val1 = expr1
        val2 = expr2
        val3 = expr3
    act1
    act2
    act3

-- alt layout
do4 =
    do let val1 = expr1
           val2 = expr2
           val3 = expr3
       act1
       act2
       act3

-- translatation
tr4 =
    let val1 = expr1
        val2 = expr2
        val3 = expr3
    in do act1
          act2
          act3

-- translatation, alt layout
tr4 =
    let val1 = expr1
        val2 = expr2
        val3 = expr3
    in do
        act1
        act2
        act3
```

## The do notation

https://en.wikibooks.org/wiki/Haskell/do_notation

The "then" operator, `>>`, works almost identically in the "do" notation as in unsugared code. The following sequence of instructions nearly matches that of an imperative language.

```hs
-- a chain of actions like this
putStr "Hello" >>
putStr " " >>
putStr "world!" >>
putStr "\n"

-- may be rewritten as
do { putStr "Hello"
   ; putStr " "
   ; putStr "world!"
   ; putStr "\n" }
-- with explicit braces and semicolons added, even
-- though they are both optional is this particular layout
```

In Haskell, we can chain any actions as long as all of them are in the same monad. In the context of the IO monad, the actions include things such as writing to a file, opening a network connection, asking for input.




## References

https://en.wikibooks.org/wiki/Haskell/do_notation
https://wiki.haskell.org/Do_notation_considered_harmful
http://book.realworldhaskell.org/read/monads.html#monads.state
https://stackoverflow.com/questions/8019670/desugaring-do-notation-for-monads
http://www.haskellforall.com/2014/10/how-to-desugar-haskell-code.html
