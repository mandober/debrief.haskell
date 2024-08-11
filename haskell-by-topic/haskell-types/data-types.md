# Haskell Data Types

## Terms

- Algebraic data types
  - zero (empty type)
  - unit (unit type)
  - product
  - sum (coproduct)
  - exponential type (function type)
- type aliases
- canonical data type representations

## Algebraic Data Types

Haskell offers many builtin data types, which may be classified algebraically into zero, unit, product, sum and exponential types, collectively called Algebraic Data Types (ADT).

Algebraic Data Types
- zero (empty type)
- one (unit type)
- product
- sum (coproduct)
- exponential type (function type)

## Data type declarations

A custom ADT can be constructed using the declarations:
- data
- newtype
- type

- The `data` keyword is used to construct a bona fide new type. 
- The `newtype` keyword is a special, optimized constructions of a new type but only if it is a single-field product type.
- The `type` keyword is used to make superficial type aliases.

## Canonical representations of data types

{ 0, 1, ×, +, ^ } ≅ { `Void`, `()`, `(,)`, `Either`, `(->)` }

Name        | Algebra | Haskell    | Inhab | Example value
------------|---------|------------|-------|----------------------
zero        | 0       | Void       | 0     | N/A
unit, one   | 1       | ()         | 1     | ()
product     | a ⨯ b   | (a, b)     | a + b | (String, Int)
sum         | a + b   | Either a b | a × b | Either String Int
exponential | b ^ a   | a -> b     | b ^ a | String -> Int


The canonical representation of data types
- (0) zero: `Void`, with no inhabitants
- (1) unit: `()` with a single inhabitant, `()`, i.e. `() :: ()`
- (⨯) product: pair, `(a, b)`, with `a ⨯ b` inhabitants
- (+) coproduct: `Either a b`, with `a + b` inhabitants
- (^) exponential: function type, `a -> b`, , with `bᵃ` inhabitants

This means that any type can be represented using its canonical form:

```
     Bool ≅      Either () ()           2 = 1 + 1
a -> Bool ≅ a -> Either () ()           2 = 1 + 1
Maybe a ≅ Either () a                   M a = 1 + a
Either a b -> c ≅ (a -> c, b -> c)      cᵃᐩᵇ = cᵃ × cᵇ
((a, b) -> c) ≅ (b -> (a -> c)) ≅ (a -> (b -> c))
cᵃᐩᵇ = cᵃ × cᵇ
cᵃᵇ = (cᵃ)ᵇ = (cᵇ)ᵃ
```

The *canonical representation of products* is a pair, so `a + b ≅ (a, b)`, but, more generally, a product type may have any nunmber of *components or fields*, e.g. a + b + c, which may be encoded with nested pairs ((a, b), c), or using *tuples*, (a, b, c).

Nested pairs may be left- or right-associative, depending on the parenthesis
- left-associative: `(((a, b), c), d)`
- right-associative: `(a, (b, (c, d)))`
- as a tuple: `(a, b, c, d)`


## Equivalences of data type constructions

A product data type declared with the `data` keyword has a type ctor on the left side and begins with the data cotr on the right side which is followed by a number of *fields*.

```hs
data Scheme = Scheme [String] Int
```

Since the canonical representation of products is a pair we can write any product type as a pair, e.g.

```hs
data Scheme = Scheme ([String], Int)
```

Now, we have just one field (instead of two fields) which allows us to use the `newtype` keyword:

```hs
newtype Scheme = Scheme ([String], Int)
```

Data types declared with the `data` keyword are "less efficient" then the equivalent data type declared with the `newtype` keyword (if such change is allowed).

>Only single-field product types may be declared with the newtype keyword.

### Extracting the fields value

Declaring a data type using the `data` keyword allows naming the fields. The name of the fields then become the **accessor functions**. In fact, such data type ios called a **record**.

```hs
-- as data record
data Scheme = Scheme { getList :: [String], getVal :: Int }
-- the type of the data ctor (function) Scheme
Scheme :: [String] -> Int -> Scheme
-- the type of the getList accessor
getList :: Scheme -> [String] 
-- the type of the getVal accessor
getVal :: Scheme -> Int
```

Having named fields allow us to get the field values just by calling the suitable accessor function, e.g. in this example, if we have a value `sch` known to be a `Scheme` (i.e. known to have the type `Scheme`), we can get the value of its first field by `getList sch`, and `getVal sch` for the value of the second field.

```hs
-- construct new value #1
sch1 = Scheme ["abc", "def"] 2
x1 = getList sch1 -- ["abc", "def"]
x2 = getVal sch1  -- 2

-- construct new value #2
sch2 = Scheme { getList = ["abc", "def"], getVal = 2 }
```

>Accessor functions are conveninet but partial.



On the other hand, if we didn't declared the accessor functions, we can define them outside the data declaration ourselves. In this case, we'd extract the fields values the old-fashioned way, via *pattern matching*:

```hs
data Scheme = Scheme [String] Int

getList :: Scheme -> [String]
getList xs _ = xs

getVal :: Scheme -> Int
getVal _ n = n

sch2 = Scheme ["abc", "def"] 2
x1 = getList sch2
x2 = getVal sch2

-- A more elaborate extaction
foo :: Scheme -> …
foo (x:xs) n = …
```

The same is true for single-field products constructed with the newtype keyword. We can optionally also name the field, obtaining the accessor function that extracts its value.

```hs
newtype Scheme = Scheme { getBoth :: ([String], Int) }
-- type of accessor
getBoth :: Scheme -> ([String], Int)
```

Otherwise we'd have to define the extractor functions ourselves using pattern matching.

```hs
newtype Scheme = Scheme ([String], Int)

getList :: Scheme -> [String]
getList p = fst p
getVal :: Scheme -> Int
getVal p = snd p
-- or
getList :: Scheme -> [String]
getList (xs, _) = xs
getVal :: Scheme -> Int
getList (_, n) = n

-- A more elaborate extaction requires more parenthesis
foo :: Scheme -> …
foo ((x:xs), n) = …
```

### GADT

```hs
-- #1.0 individual fields
data Scheme where
  Scheme :: [String] -> Int -> Scheme

-- #1.1 individual named fields
data Scheme where
  Scheme :: { getList :: [String] } -> { getList :: Int } -> Scheme

-- #2.0 fields packed in a pair
data Scheme where
  Scheme :: ([String], Int) -> Scheme

-- #2.1 fields packed in a pair and named
data Scheme where
  Scheme :: { both :: ([String], Int) } -> Scheme
```


### All the ways to define this data type

All the ways to define the data type `Scheme` with two fields: first a list of strings, second an integer.

```hs
-- ADT data, #1, two fields
data Scheme = Scheme [String] Int
-- ADT data, #2, named fields
data Scheme = Scheme { getList :: [String], getList :: Int }
-- ADT data, #3, fields as pair
data Scheme = Scheme ([String], Int)
-- ADT data, #4, fields as pair, named
data Scheme = Scheme { getBoth :: ([String], Int) }

-- newtype, #1
newtype Scheme = Scheme ([String], Int)
-- newtype, #2 named
newtype Scheme = Scheme { getBoth :: ([String], Int) }

-- GADT data, #1 individual fields
data Scheme where
  Scheme :: [String] -> Int -> Scheme
-- GADT data, #2 individual named fields
data Scheme where
  Scheme :: { getList :: [String] } -> { getList :: Int } -> Scheme
-- GADT data, #3 fields packed in a pair
data Scheme where
  Scheme :: ([String], Int) -> Scheme
-- GADT data, #4 fields packed in a pair and named
data Scheme where
  Scheme :: { both :: ([String], Int) } -> Scheme
```

The most interesting thing is how the two fields are separated in these defintions of which these examples are sufficient to show different delimiters:

```hs
data Scheme =     Scheme     [String]   Int             -- space
data Scheme where Scheme ::  [String] → Int  → Scheme   -- arrow
data Scheme =     Scheme    ([String],  Int)            -- comma
data Scheme where Scheme :: ([String],  Int) → Scheme   -- comma (arrow)
```

So, a product type may be defined either as an ADT or GADT. As an ADT, the product is defined using the `data` keyword in which case its fields as delimited with spaces. Choosing to collect the fields as components of a tuple allows for a more efficient representation using the `newtype` keyword. Having fileds packed as a tuple, yet sticking with the `data` keyword is weird.

As GADTs, products are defined using the `data` keyword exclusively. In a GADT, each data ctor is annotated with its full type (which is equivalent type to the implicit type of a ADT data ctor). Naming fields is also possible, as is packing the fields in a tuple. Collecting the fields in a tuple makes for less convenient pattern matching (more parenthesis are required).



## Parameterized data type

A data type may be *parameterized* by a number of *type variables* that first need to be *declared* on the left side, before they are *used* on the right side.

```hs
data Scheme a b c = Scheme a b c
```
