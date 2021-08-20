# NegativeLiterals

- Pragma: `NegativeLiterals`


Enabling this pragma, the spacing becomes important: `-4` is interpreted as a negative literal, and `x - 4` as the minus operator in subtraction; and so is the weird `x- 4`. So, `5 + -3` is possible to write, as well as sections with the minus operation, `(- 3)` (note the spacing).

```hs
5 - 3              -- 2
5 + -3             -- 2
(\x -> x - 3)   5  -- 2
(\x -> (-3) x)  5  -- error
(\x -> (- 3) x) 5  -- error

((+ 3) . (+ 1)) 5  -- 9
((- 3) . (- 1)) 5  -- error
((3 -) . (5 -)) 6  -- 4

(( -  1) . (5 -)) 6 -- error
(((-) 1) . (5 -)) 6 -- 2
```
