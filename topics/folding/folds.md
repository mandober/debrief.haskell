# Folds

Many data structures admit folding, but lists represent a canonical, examplary struture commonly used when discussing it. So the description of folding below is for any foldable data structure, even when lists specifically are mentioned.

## Family of folding functions

- fold
- foldMap
- foldMap'

- foldl
- foldl'
- foldl1
- foldlM

- foldr
- foldr'
- foldr1
- foldrM


There isn't a single fold function, but a *family of fold functions* that are classified according to various criteria.

These criteria include factors such as *strictness* (and levels of strictness) of various parts of the code involved (things described with phrases like: "lazy reducer", "strict reducer in the first argument", "binary operator that is lazy in both args", etc.). Fold members that take this into account include `foldl'` and `foldr'`.

Another defining factor is whether the structure is a monoid, giving rise to fold members such as `foldl1`


One of those classification factors is the direction in which the return value is assembled, which divides the folding functions by chirality into the left and right team.

## Overview of `foldl` and `foldr`

Order of args in reducers
- reducer (foldl) z x
- reducer (foldr) x z

```hs
                      FOLD
LEFT                                       RIGHT
    ↘                                     ↙
      z ⊕  [ 0 ⊕ 1 ⊕ 2 ⊕ 3 ⊕ 4 ]  ⊕ z
```

Reducer of `foldl` takes accumulator first, element second, `z x`.    
Reducer of `foldt` takes element first, accumulator second, `x z`.




```hs
          ┌ Acc
          │    ┌ Elem
          │    │
foldl :: (b -> a -> b) -> b -> [a] -> b
         └────┬──────┘   └┬┘  └─┬─┘
           reducer       acc   list
         ┌────┴──────┐   ┌┴┐  ┌─┴─┐
foldr :: (a -> b -> b) -> b -> [a] -> b
          │    │
          │    └ Acc
          └ Elem
```


The left team is led by the `foldl`, which is lazy in the accumulator and performs a left-associative fold of the structure.

>foldl (⊕) z [x₀, x₁, …, xₙ] ≡ (… ((z ⊕ x₀) ⊕ x₁) ⊕ …) ⊕ xₙ

(… ((z ⊕ x₀) ⊕ x₁) ⊕ …) ⊕ xₙ

(… ((0 ⊕ 1) ⊕ 2) ⊕ …) ⊕ 9






```hs

foldl (⊕) z [1,2,3,4] == (((z ⊕ 1) ⊕ 2) ⊕ 3) ⊕ 4

foldr (⊕) z [1,2,3,4] == 

0 ⊕ 1 ⊕ 2 ⊕ 3 ⊕ 4 ⊕ z



foldl (⊕) z []             ==      z
foldl (⊕) z [x₀]           ==      z ⊕ x₀
foldl (⊕) z [x₀, x₁]       ==      z ⊕ x₀ ⊕ x₁
foldl (⊕) z [x₀, x₁, x₂]   ==      z ⊕ x₀ ⊕ x₁ ⊕ x₂
foldl (⊕) z [x₀, x₁, …, xₙ] == (… ((z ⊕ x₀) ⊕ x₁) ⊕ …) ⊕ xₙ


foldr (⊕) z []             ==                    z
foldr (⊕) z [x₀]           ==               x₀ ⊕ z
foldr (⊕) z [x₀, x₁]       ==          x₀ ⊕ x₁ ⊕ z
foldr (⊕) z [x₀, x₁, x₂]   ==     x₀ ⊕ x₁ ⊕ x₂ ⊕ z
foldr (⊕) z [x₀, x₁, …, xₙ] == x₀ ⊕ (x₁ ⊕ … (xₙ ⊕ z) …)

```

The right team is led by the `foldr`, which is also lazy in the accumulator, but performs a right-associative fold of the structure.

>foldr (⊕) z [x₀, x₁, …, xₙ] ≡ x₀ ⊕ (x₁ ⊕ … (xₙ ⊕ z) …)







Folds are actually a family of functions, divided by various criteria, of which the main one is the direction - not of traversal, which is always from head to tail - but of the associativity of results.

Generally, but in Haskell specifically, 


the two canonical folding operations are `foldl` and `foldr`, differing in the way each forms the result. Either requires 3 arguments:

the order of these arguments is 

1. a binary function, `f`, aka "reducer"
2. a constant, `z`, an accumulator
3. a foldable data structure, `t a` (list locally, foldables generally)


a binary "reducer" function `f` first, initial value `z` second, data structure `t a` third (where `t a` represents a foldable structure, `Foldable t => t a`).


The second argument to a fold, `z`, has several names depending on its current purpose and use.
- It is called a *default value* because in case an empty list is supplied, it is the returned result of a fold.
- It is called an *initial value* because, initially, it is fed into the reducer function along with the first element of the list.
- It is called an *accumulator* because, after it is initially given to the reducer (along with the first list element), the result that the reducer produces is stored in this parameter; it is passed to the reducer again and again, accumulating some value, as each element of the list is processed. When all list elements have beed processed, the return of the overall fold function is what is stored in the accumulator. Again, if the list is empty, the accumulator is immediately returned; if the list is non-empty, then this parameter accumulates some value, until it is finally returned as the results of the overall folding operation.
- This parameter is often a neutral or identity element with regard to the structure.

For example, for lists, this arg is the empty list `[]` (implying that a fold with produce a new list based on the original one); for a fold that reduces a list into a single summary value, the initial argument can be 0 in case of summation, or 1 in case of multiplication; when folding a list using a logical connective, the initial value could be `True` (identity of conjunction) or `False` (identity of disjunction).

A function `f`, called reducer, is the first argument to a fold. It is a binary function that, initially, takes the initial value `z` and the first element of the list `x₀` (i.e. a list's head element). Howevere, the two main folding operators take these two argument in a different order:
- `foldl` takes the 








```hs
foldl :: (b -> a -> b) -> b -> [a] -> b

foldr :: (a -> b -> b) -> b -> [a] -> b
```


Folds are actually a family of functions, divided by various criteria, of which the main one is the direction - not of traversal, which is always from head to tail - but of the associativity of results.


```
foldl                              foldr
z x₀                                 x₀ z

```


`foldl` is left-associative:
- the neutral element (initial value) `z` is a left arg to a reducer

`foldl` is right-associative:
- the neutral element (initial value) `z` is a right arg to a reducer




In the case of lists, `foldl`, when applied to a binary operator, a starting value (typically the left-identity of the operator), and a list, reduces the list using the binary operator, from left to right:

>foldl f z [x₀, x₁, …, xₙ] == (… ((z `f` x₀) `f` x₁) `f` …) `f` xₙ

To produce the outermost application of the operator *the entire input list must be traversed*.

Like all left-associative folds, `foldl` *diverges on infinite lists*.
