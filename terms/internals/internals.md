# GHC Internals

https://notepad.mmakowski.com/Advanced%20Haskell


## Lists

- imperative: destructive update (ephemeral data structures)
- functional: creation of a new value (persistent data structures)

[1,2,3,4] is syntactic sugar for 1:(2:(3:(4:[])))

When doing `let y = 0:xs`, the `xs` is reused; it is safe to **share** it, because it is immutable. The same happens when we `drop 3 ys`, it is **pointer**. However, when we `take 2 ys`, we have to copy the cons cells.

### In-memory representation

Words of memory:

The first word identifies the constructor. The other words contain the payload (pointers to constructor args):

+---+----------+----------+
|(:)|hd ptr -> |tl ptr -> |
+---+----------+----------+


Thunks are represented similarly, as heap objects:

+---+----------+----------+
|fn |arg 1 ->  |arg 2 ->  |
+---+----------+----------+

The first field indicates whether it is a thunk or constructor. If thunk, it can be replaced by evaluated value (one of very few cases where in Haskell something gets changed destructively). The benefit is that if a shared thunk is evaluated, the value will be available to all places pointing to it.
