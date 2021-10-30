# Recursion

* In Haskell, repeated caclulation is realized through recursion, not throught the use of iterative looping constructs (like `for`, `while`, `do while`).

* However, recursion is more elegant and a more natural way to express repetition that is close to mathematical induction, which is used for proofs of program correctness when doing equational reasoning.

* The two kinds of repeatition are equivalent up to stack exhaustion, which may happend during recursion. In fact, recursion could be considered a syntactic sugar considering that the compiler will unfold it into a iterative loop.

* Performance wise, GHC compiles recursive datatypes and functions into very efficient loops; therefore, the surface syntax offers elegance while keeping performance in check.

* Entering a function in Haskell does not create a new stack frame, the logic of the function is entered with the arguments on the stack and yields result to a register. In case where a function returns an invocation of itself invoked in the *tail position*, the resulting logic is compiled identically to a `while` loop in other languages, via a `jmp` instruction instead of a `call`.


## Recursive functions

List is the prime example of a structure where the recursion can be clearly observed. List itself is a recursive data type and there are a couple of similar ways to describe it: 
* a list is either empty or it has an element appended to (the rest of) a list.
* a list consists of:
  - the *nil* data constructor that marks the end of a list and alone marks the empty list
  - the *cons* data constructor that represents appending an element to a list




```hs
sum :: [Int] -> Int
sum ys = go ys 0
  where
    go (x:xs) i = go xs (i+x)
    go [] i = i
```


Mutual recursion:

```hs
even 0 = True
even n = odd (n-1)

odd 0 = False
odd n = even (n-1)
```
