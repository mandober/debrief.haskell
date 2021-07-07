# Handles

## Handle-IO

- The handle-based I/O is I/O done like in C
- idiomatic C code can also be written in Haskell with a handle-based IO
- We can now differentiate EOF from other IO errors, which wasn't possible with the Lazy IO. We can even recover from some errors (like file locking errors). The code is much more explicit, with error handling and ensuring that the handle is always closed, but is still quite simple. Only one line in that code deals with the counting of the whitespace characters; the others are the boilerplate.
