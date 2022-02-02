# Index of Data Structures


* Standard library

Haskell Data Structures
* Base
  - pair: `data (,) a b = (a,b)`
  - list: `data [] a = [] | a : [a]`
  - Maybe: `data Maybe a = Nothing | Just a`
  - Either: `data Either a b = Left a | Right b`
* Nested
  * List-based:
    - key-value store: `Ord k => [(k, v)]`
    - SetList: `Ord k => [k]`
  * functions-as-data:
    - pair: `type Pair a b = forall c. a -> b -> c`
    - difference list: `type DList a = [a] -> [a]`
* Next
* Advanced
