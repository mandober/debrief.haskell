# Composition of functions

* Haskell's composition operator is dot, (.), because composition, being used very often, is represented by an easily accessibly keyboard key.

* Many types of functions, regarding their arity, may be composed.

* Composing sections is somewhat related to point-free form

```hs
(+ 3) 5             -- 8
(*2) (+ 3) 5        -- ERR
(*2) ((+ 3) 5)      -- 16
((*2) . (+ 3)) 5    -- 16

[(>= 1), ((== 0) . (`mod` 3)), odd]
```

Predicates
- `all`
- `any`
- `and`
- `or`

```hs
all (== True) xs === and xs

any (== True) xs === or xs
```


Two lists when one holds the values and the other holds the keys,
i.e. a way how to select/drop/keep values from the first
A list that indexes another, value list

* An example, is list of T/F or 0/1, as the ks list
that selects which eleemnts to keep from the vs list
[0 ,  1,  0,  1,  0,  1]
[37, 53, 42, 57, 23, 49] ~~>
[37, 53,     57,     49] ~~> [37, 53, 57, 49]


[(>3), (== 0) . (`mod` 3), (2<), (==57), (/=23), (>=7)]
[37  , 53                , 42  , 57    , 23    ,    49] ~~>

Composition of sections

The fn to check if an element is divisible by 3 is: divBy3 x = x `mod` 3 == 0
but to place such function as an element in the list of similar functions,
it has to be converted in a section (or its lambda form). Figuring out the
lambda form is straing-forward, but not so much about how to sectionize it.
Namely, the section form cannot be: (`mod` 3 == 0)
You have to observe there are 2 functions at play here: mod and (==)
which should hint at composition, but (`mod` 3) . (== 0) is horribly wrong.
It tries to get the mod of a Bool. The hint related to composition is reversal,
as in "g after f". Therefore, it is zero check AFTER getting the x mod 3:
    g = (`mod` 3)
    f = (== 0)
    x is an element of the list
    f (g x) = (f . g) x === (== 0) . (`mod` 3)

How to repr using sections: 0 <= r < 5
(0 <= r) and (r < 5)
(0<=) and (<5)
The problem is, each gives a Bool, so cannot compose them.
(0<=) && (<5)
