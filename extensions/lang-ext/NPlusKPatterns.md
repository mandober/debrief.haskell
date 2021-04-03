# NPlusKPatterns

- Pragma: `NPlusKPatterns`
- status: **obsolete**
- Enables `n + k` pattern on the LHS of equations, which are useful for recursive case pattern matching. The important thing is that the rec arg is decreased - and that can be done on the left side (with this pragma) or on the right side (using normal subtraction).

This pragma is obsolete and frowned upon even by the compiler.

```hs
add m  0      = m
add m (n + 1) = succ (m + n)

-- instead of

add m 0 = m
add m n = succ (m + (n - 1))
```
