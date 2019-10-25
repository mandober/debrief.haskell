# List

<!-- TOC -->

- [String is a list of characters](#string-is-a-list-of-characters)
- [List ctors](#list-ctors)
- [List function types](#list-function-types)
- [List operations](#list-operations)
- [Syntactic sugar and destructuring](#syntactic-sugar-and-destructuring)
- [Destructuring](#destructuring)
- [Implementing custom list type](#implementing-custom-list-type)
- [Older derive](#older-derive)

<!-- /TOC -->

- list is type that allows storing multiple homogeneous values (called elements) within a single value
- lists are not fixed, can be shrinked, extended, indexed
- type of list depends only on the type of elements, `[a]`
- list can contain concrete, `[Bool]`, or polymorphic types, `[a]`
- list can be nested but watch out for types, e.g. `[Int]` vs `[[Int]]`
- Lists: `[]( [], (:) )`
- list has a *unary type ctor*, `[] a`
- list has *2 data ctors*:
  - nullary "empty list" data ctor, `[]`
  - binary "append list" data ctor, `:`, i.e. `a : [a]` i.e. `a : []`



## String is a list of characters

```hs
type String = [Char]

[["a","b","c"],["d","e"]] :: [[[Char]]]
```


## List ctors

```hs
-- TYPE CTOR:
> :i []
data [] a = [] | a : [a]        -- Defined in GHC.Types

> :t []
[] :: [t]

> :k []
[] :: * -> *


-- DATA CTOR:
> :i (:)
data [] a = ... | a : [a]      -- Defined in GHC.Types
infixr 5 :

> :t (:)
(:) :: a -> [a] -> [a]

> :k (:)
-- Error not a type ctor but data ctor
```


## List function types 

```hs
-- data ctor []
[] :: [t]

-- data ctor (:)
(:) :: a -> [a] -> [a]

-- number of elements
length :: Foldable t => t a -> Int

-- flatten nested list
concat :: Foldable t => t [a] -> [a]

-- merge lists
(++) :: [a] -> [a] -> [a]

-- indexing
(!!) :: [a] -> Int -> a
```


## List operations

```hs
l1 = [1,2,3]
length l1           -- 3

l2 = [4,5,6]
l3 = l1 ++ l2       -- [1,2,3,4,5,6]

l4 = [l1,l2]        -- [[1,2,3],[4,5,6]]
concat l4           -- [1,2,3,4,5,6]

-- watch out for type
-- this won't work:
l9 = [1, l1, l2]

-- cuz l1 and l2 :: [Int] and 1 :: Int
-- elements must have the same type i.e. [a]

-- this is ok:
l9 = [[1], l1, l2]  -- [[1],[1,2,3],[4,5,6]]

-- indexing
[4,5,6] !! 0 -- 4
```



## Syntactic sugar and destructuring

```hs
-- sugar:
[1,2,3]

-- desugared:
1 : (2 : (3 : []))

-- result is the same in both cases:
[1,2,3] :: Num a => [a]
```


## Destructuring

```hs
-- desugared formed can be good for destructuring:
> a : (b : (c : [])) = 1 : (2 : (3 : []))
a :: Num t => t     -- 1
b :: Num t => t     -- 2
c :: Num t => t     -- 3

-- this works differently:
-- note the last element, it is not `t`, but `[t]`
(x : y : z) = 1 : (2 : (3 : []))
x :: Num t => t     -- 1
y :: Num t => t     -- 2
z :: Num t => [t]   -- [3]

-- there are actually 4 elements in the list: 1,2,3,[]
(x : y : z : w) = 1 : (2 : (3 : []))
x :: Num t => t     -- 1
y :: Num t => t     -- 2
z :: Num t => t     -- 3
w :: Num t => [t]   -- []
```


## Implementing custom list type

- list is a builtin type represented by using the built-in syntax: `[]` and `:`
- these symbols cannot be (re)used for something else
- builtin types are defined in GHC.Types, but they're not explicitly exported
- they are just for symbol tables and such; they are impl by the compiler


If the list had been defined in std, it would've been defined possibly like this with special symbols as infix operators:

```hs
module List(
  []( [], (:) )
) where

data [] a = [] | a : [a]

-- desugared form:
data [] a = [] | a : (a : [])
```

Considering the data declaration, we can see that a list is:
- either empty or
- a list conatining elements of some type

The sig of its data ctors:

```hs
> :t []
[] :: [a]

> :t (:)
(:) :: a -> [a] -> [a]
```

The *empty data ctor*, `[]`, is a nullary ctor having the some type `a` i.e. the type of (future) elements of the list.

The *append data ctor*, `:`, is binary: it takes an element of type `a` and a list `[a]`, containing the same type of elements, and it appends the element to the list.

We can translate the ctors from their infix symbolic form into prefix functions. The builtin list has the same name for type ctor and the empty data ctors but we'll name them all differently:
- type name i.e. type ctor: `List`
- empty list nullary data ctor: `Nil`
- append list binary data ctor: `Cons`

We also have to pay attention to the last part, `[a]` - it references the *type ctor* (not the type ctor of the same name/symbol), because list is a *recursive type*.


**The translation process**:

```hs
-- original data declaration
data [] a = [] | a : [a]

-- desugared form:
data [] a = [] | a : (a : [])


-- customize the empty data ctor
data [] a = Nil | a : (a : [])

-- customize the type ctor and maintain recursion
data List a = Nil | a : (a : List)

-- customize the append data ctor
data List a = Nil | a Cons (a Cons List)

-- Correct infix ops to prefix position functions
data List a = Nil | Cons a (Cons List a)

-- we don't use both ctors to append an element so drop Cons
data List a = Nil | Cons a (List a)

-- great success!
```

Use it:

```hs
data List a = Nil | Cons a (List a) deriving Show

l0 = Nil
l1 = Cons 1 l0
l2 = Cons 2 l1
l3 = Cons 3 l2
l4 = Cons 4 l3

l5 -- Cons 3 (Cons 2 (Cons 1 Nil))
```

















## Older derive

```hs
data L a = L | a : (a : L)
data L a = L | : a (: a L)
data L a = L | : a (: L a)
data L a = L | S a (S L a)
data L a = L | S a (L a) deriving Show

S 3 L  -- ~~> 3 : []
it :: Num a => L a

S 4 (S 3 L) -- ~~> 4 : (3 : [])
it :: Num a => L a
```
