# System.CPUTime

```hs
import System.CPUTime

-- | The 'cpuTimePrecision' constant is the smallest measurable difference in CPU time that the implementation can record, and is given as an Integer number of picoseconds.
cpuTimePrecision :: Integer
-- 1000

-- | Computation 'getCPUTime' returns the number of picoseconds CPU time used by the current program. The precision of the result is implementation-dependent
getCPUTime :: IO Integer
-- 2_009_842_100_000
-- 2128022600000
-- 2456017200000
-- 2798671300000
-- …
```
