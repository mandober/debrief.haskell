# Types

- every (sub)expression has a type
- main :: IO ()
- i.e. main func always has the type `IO ()`
- types are (usually) inferred, type annotations are optional
- Basic types: Int Integer Float Double Char, [Char] = String


Every expression (and subexpression) in Haskell has a type that the compiler usually infers, so type annotations are optional.

Basic types:
- `Int` bound integer type, usually 32-bit
- `Integer` unbound integer type
- `Float` single-precision float 32-bit i.e. `binary32`
- `Double` double-precision float 64-bit i.e. `binary64`
- `Char`
- `String` is a synonym for `[Char]`

Composite types:
* Tuples
  - heterogenous data type: depends on the number of elements and their type
  - e.g. `(Int, Float)`, `(Char, Int, Double)`
* Lists, 
  - homogenous data type
  - e.g. `[Int]`, `[[Float]]`, `[[(Char, Float)]]`
* Enumeration types:
  - simple form of algebraic data type:
  - e.g. `data Bool = True | False; deriving (Show, Read, Eq, Ord)`
* Algebraic types:
  - data constructors can have arguments
* Recursive data types



```hs
-- Enumeration type
data Bool = True | False
  deriving (Show, Read, Eq, Ord)
-- here, `Bool` is the name of the new type
-- and `True` and `False` are type constructors


-- Algebraic type
data Shape = Circle Float -- radius
  | Rectangle Float Float -- length, width
deriving (Show, Read, Eq, Ord)
-- here, `Shape` is the name of the new type
-- and `Circle` and `Rectangle` are type constructors

-- Recursive data types:

-- list
data IntList
  = Cons Int IntList
  | EmptyList

-- tree
data IntTree
  = Node Int IntTree IntTree
  | EmptyTree
```

Compare with C:

```c
// list
typedef struct listNode *link;
struct listNode {
  int item;
  link next;
}

// tree
typedef struct treeNode *link;
struct treeNode {
  int item;
  link leftTree, rightTree;
};
```



## Type annotations


```hs
-- accepts Int, returns Int
square :: Int -> Int
square x = x * x

-- accepts two Double args (2 unary fns), returns Double
average :: Double -> Double -> Double
average x y = (x+y)/2
```

## Implementation of lists in Haskell

in C, we would use pointers:

```c
// typedef … token;

typedef struct node *link;
struct node {
  token value;
  link next;
};
```

**Data constructors** are used to build values of non-basic types.

*Lists* have two data constructors, already predefined in `Prelude` module:
- `:` right associative infix constructor which takes an item and a list
- `[]` is the empty list

```hs
-- [0,1,2] is
0:1:2:[]
-- right associative: 0:(1:(2:[]))

-- get list length recursively


-- `length` fn
--    type annot.: accepts a list of type a (any type) and returns an Int
--    definition : xs is fn param. it gets pattern-mathed against:
--    a) the empty list: in which case return 0
--    b) expr y:ys which will take list's head as `y` and list's tail as `ys`.
--       it returns counts the head element as 1 (adds 1 to the running count
--       of list's length), and it will recursively call itself with the tail
--       of the list (all elements but first) i.e. `ys`.

-- fn def:
length :: [a] -> Int
length xs = case xs of
  [] -> 0
  (y:ys) -> 1 + length ys

-- alt fn def:
length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs
```
