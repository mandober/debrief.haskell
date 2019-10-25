# Declarations

In GHCi you may enter any *top-level* Haskell declaration:
  - `data`      Defines a new type
  - `newtype`   Between new type def and type alias
  - `type`      Type aliases
  - `class`     Defining the new type class (trait bound)
  - `instance`  Customizing class' instance
  - `deriving`  Auto-deriving type classes: Show, Num, Bounded

Deriving is possible for these 7 type classes:
  1. Show
  2. Num
  3. Bounded
  4. Ord
  5. Eq
  6. Enum
  7. Read



<!-- #region Data declaration, Bool, Examples -->

## Data declaration
- data
- datatype
- type signature
- data declaration:
  - type constructor, `Bool` (use :info to query)
  - type name, `Bool`
  - data or value constructors: `True`, `False` (use :type to query)


*Data declaration* is a statements that contains the definition of a datatype, e.g. `data Bool = False | True`


A **datatype** is a classification of data (values). Types in Haskell determine what values are members of it or inhabit it. Unlike in other languages, datatypes in Haskell by default do not delimit the operations that can be performed on that data.

**Type constructor** is the (capitalized) *type name*. At the type level of code, *type names* or *type constructors* are what you use.

**Data declarations** are the statements in which the datatypes are defined.

**Data constructors** are the values that inhabit the type they are defined in. They are the values that show up in your code, at the term level instead of the type level (by the term level we mean they're the values as they appear in your code or the values that your code evaluates to).



## Type (data) declarations

```hs
data Bool = False | True

data Shirt = S | M | L

data Shape = Circle Float Float Float
             | Rect Float Float Float Float
    deriving (Show, Eq)

data Customer = Customer String Int deriving Show

data Foo = Foo { a :: Int, b :: Int, c :: String } deriving Show

data Employee = Employee {
    name :: String,
    xp   :: Int
} deriving Show
```



## Boolean datatype

- `Bool` datatype provides for truth values
- there are only two truth values, so easy to list all its *data constructors*
- The type being defined here is Bool, and it has exactly two values: True and False.
- Type `Bool` is an example of a **nullary type constructor** and
- `True` and `False` are **nullary value constructors**.


```hs
-- the definition of Bool
data Bool = False | True -- [5]
--   [1]    [2]  [4] [3]
```
1. *Type constructor* for datatype `Bool` (type name as found in type sigs)
2. *Data constructor* for the value `False`.
3. *Data constructor* for the value `True`.
4. Pipe indicates logical disjunction; a `Bool` value is `True` OR `False`
5. The entire statement is called **data declaration**


*Data declarations* do not always follow precisely the same pattern; there are datatypes that use logical conjunction, and some *type constructors* and *data constructors* may have arguments. The thing they have in common is the keyword `data` followed by the *type constructor* (type name as it will appear in type signatures), the equals sign to denote a definition, and then *data constructors* (names of values that inhabit your term-level code).


Data declaration: `data` TypeCtor = [[DataCtors]...]

Type Signature: identifier/operator :: [TypeCtor1] -> [TypeCtor2]...

Type Signature: ident/op :: ([TypeClass], ...) => [TypeCtor1] -> [TypeCtor2]...


An illustration of how these different parts fit together: if we query the type information for a function `not` we see that it takes a `Bool` value and returns another `Bool` value, so the *type signature* makes reference to the *type constructor* (*type name*):

> :t not
not :: Bool -> Bool

But when we use the `not` function, we use the data constructors (values):
> not True
False

And our expression evaluates to another data constructor (value) - in this case the other data constructor for the same datatype.

>:t (+)
(+) :: Num a => a -> a -> a


### Example 1

```hs
data Mood = Good | Neutral | Bad deriving Show

moodUp :: Mood -> Mood
moodUp Good = "Good for you!"
moodUp x = "Get better"
```

In ghci a func that spans multi lines, must be entered on 1 line with semi-colon separators:
> :{
>| data Mood = Good | Neutral | Bad deriving Show
>| moodUp :: Mood -> Mood
>| moodUp Good = "Good for you!"
>| moodUp x = "Get better"
>| :}

> :t Bad
Bad :: Mood


### Example 2

```hs
data T = A | B | C deriving (Eq, Ord, Show, Enum)

```

> data T = A | B | C deriving (Eq, Ord, Show, Enum)
> [A ..]
[A,B,C]
> :i T
data T = A | B | C -- Defined at <interactive>:2:6
instance Enum T -- Defined at <interactive>:2:45
instance Eq T -- Defined at <interactive>:2:30
instance Ord T -- Defined at <interactive>:2:34
instance Show T -- Defined at <interactive>:2:39


<!-- #endregion -->
