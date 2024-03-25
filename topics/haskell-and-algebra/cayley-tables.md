# Cayley tables

* In a Cayley table, the first row is always the same as the *header row*, and the first column is always the same as *header column*.
* In a Cayley table, each row and column must contain *distinct elements*.

Rows and columns are 0-indexed. After the header row comes the row 0, then row 1, etc. After the header column, comes the column 0, then 1, etc. Cayley table has `n` rows and `n` colums, `n×n`, it is always a square table; i.e. max row is the same as max column and both are `n`. So we can talk about row `i` and column `j`. Row iterator variable `i` goes from 0 to `n - 1`, and column iterator `j` from 0 to `n - 1`.

- row iterator: i = 0..n-1
- col iterator: j = 0..n-1


**Standard solution**: For each row `n`: right-shift the first row (n = 0), which is equal to the header row, by `n`:
- row 0 = header row
- row 1 = right shift by 1 >> row1
- row 2 = right shift by 2 >> row2
- row 3 = right shift by 3 >> row3
- row n = right shift by n >> rowN

In a standard solution, all secondary diagonals contain the same element.


* Ex. if the header row is `01234`, then the row `4` is `4 >> 01234 = 40123`.
* Ex. if the header row is `01234`, then the row `6` is `6 >> 01234 = 12340`, i.e. same as row 1.

```
r|01234|01234|01234|
─|─────|─────|─────|
0|01234|     |     | 0
1| 1234|0    |     |
2|  234|01   |     |
3|   34|012  |     |
4|    4|0123 |     |
5|     |01234|     | 5
⁶|     | 1234|0    |
⁷|     |  234|01   |
⁸|     |   34|012  |
⁹|     |    4|0123 |
ᵃ|     |     |01234| 10
```

### mod 2

- n = 2
- row0,rowN-1
- col0,colN-1
- i,j = 0..n, i,j = 0..2
- first cell: A[i][j] = A[0][0]
- last cell: A[n][n] = A[n][n]

- (mod 2) has 1 (standard) solution
- Number of diagonals (main-like and aux-like): 3

┌─────┬───┬───┐
│mod 2│ ⁰ │ ¹ │
├─────┼───┼───┤
│  ⁰  │ 0 │ 1 │
├─────┼───┼───┤
│  ¹  │ 1 │ 0 │
└─────┴───┴───┘

### mod 3

- n = 3
- (mod 3) has 1 (standard) solution
- Number of diagonals (main-like and aux-like): 3

┌─────┬───┬───┬───┐
│mod 3│ ⁰ │ ¹ │ ² │
├─────┼───┼───┼───┤
│  ⁰  │ 0 │ 1 │ 2 │
├─────┼───┼───┼───┤
│  ¹  │ 1 │ 2 │ 0 │
├─────┼───┼───┼───┤
│  ²  │ 2 │ 0 │ 1 │
└─────┴───┴───┴───┘


### mod 4

Number of solutions: 2

- The main diagonal `Δ` is from top-left cell to bot-right cell.
- The aux diagonal  `δ` is from top-right cell to bot-left cell.

main diagonals `↘`:
- ↘₀ : from  A[0][n]   to  A[0][n] (top-right cell)
- ↘₁ : from  A[0][n-1] to  A[1][n]
- ↘₂ : from  A[0][n-2] to  A[1][n]

When n = 4 (so i,j=0..3)
- ↘₀ : from  A[0][n]   to  A[0][n] (top-right cell)
- ↘₁ : from  A[0][n-1] to  A[1][n]
- ↘₂ : from  A[0][n-2] to  A[1][n]
- ↘₃ : from  A[0][n-2] to  A[1][n]
- ↘₄ : from  A[0][n-2] to  A[1][n]
- ↘₅ : from  A[0][n-2] to  A[1][n]
- ↘₆ : from  A[0][n-2] to  A[1][n]
- ↘₇ : from  A[0][n-2] to  A[1][n]





- aux diagonals  `↙` start from the top-left corner to bot-right.

1. solution#1 (standard solution)
- main diagonal,       Δ = `0202`
- main diagonals,      ↘ = (3, 20, 131, 0202, 131, 20, 3)
- secondary diagonal,  δ = `3333`
- secondary diagonals, ↙ = (0, 11, 222, 3333, 000, 11, 2)

1. Standard solution (Δ = 0202)
┌───────┬───┬───┬───┬───┐
│mod4 1 │ ⁰ │ ¹ │ ² │ ³ │
├───────┏━━━━━━━━━━━━━━━┥
│     ⁰ ┃ 0 │ 1 │ 2 │ 3 │
├───────┃───┼───┼───┼───┤
│     ¹ ┃ 1 │ 2 │ 3 │ 0 │
├───────┃───┼───┼───┼───┤
│     ² ┃ 2 │ 3 │ 0 │ 1 │
├───────┃───┼───┼───┼───┤
│     ³ ┃ 3 │ 0 │ 1 │ 2 │
└───────┸───┴───┴───┴───┘


2. solution#2
- main diagonal, `Δ = 0000`
- secondary diagonals alternate between
  - same elem
  - a diagonal made up of 2's and 0's, here only `202`
  - (0, 11, 202, 3333, 202, 11, 0)



2. Solution with 0 on the main diagonal (Δ = 0000)
┌───────┬───┬───┬───┬───┐
│mod4 2 │ ⁰ │ ¹ │ ² │ ³ │
├───────┏━━━━━━━━━━━━━━━┥
│     ⁰ ┃ 0 │ 1 │ 2 │ 3 │
├───────┃───┼───┼───┼───┤
│     ¹ ┃ 1 │ 0 │ 3 │ 2 │
├───────┃───┼───┼───┼───┤
│     ² ┃ 2 │ 3 │ 0 │ 1 │
├───────┃───┼───┼───┼───┤
│     ³ ┃ 3 │ 2 │ 1 │ 0 │
└───────┸───┴───┴───┴───┘

3. Solution
┌───────┬───┬───┬───┬───┐
│mod4 2 │ ⁰ │ ¹ │ ² │ ³ │
├───────┏━━━━━━━━━━━━━━━┥
│     ⁰ ┃ 0 │ 1 │ 2 │ 3 │
├───────┃───┼───┼───┼───┤
│     ¹ ┃ 1 │   │   │   │
├───────┃───┼───┼───┼───┤
│     ² ┃ 2 │   │   │   │
├───────┃───┼───┼───┼───┤
│     ³ ┃ 3 │   │   │   │
└───────┸───┴───┴───┴───┘









┌───────┬───┬───┬───┬───┐
│mod4 2 │ ⁰ │ ¹ │ ² │ ³ │
├───────┏━━━━━━━━━━━━━━━┥
│     ⁰ ┃ 0 │ 1 │ 2 │ 3 │
├───────┃───┼───┼───┼───┤
│     ¹ ┃ 1 │   │   │   │
├───────┃───┼───┼───┼───┤
│     ² ┃ 2 │   │   │   │
├───────┃───┼───┼───┼───┤
│     ³ ┃ 3 │   │   │   │
└───────┸───┴───┴───┴───┘
