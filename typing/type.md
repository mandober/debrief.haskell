# Types

## Algebraic structure
Algebraic structure is a set (called carrier or underlying set) endowed with a set of operations where the operations combine elements of the carrier set with respect to the prescribed axioms.

An example of algebraic structure is a *group*, which consists of a set together with a binary operation, respecting the axioms:
1. Identity: there must be a unique element that, when combined with any other element, leaves the other element unchanged, $$a\star Id = Id\star a = a$$
2. Totality (or closure): combining any two elements produces the element that is also a member of the same set.
3. Associativity: combination order is irrelevant, $$a\star b = b\star a$$
4. Invertibility: combining an element with its inverse must produce the identity element, $$a\star a^{-1} = Id$$

An example of a group is the set of integers together with addition and the axioms of identity, totality, inverse and associativity, 
$$G = \{\mathbb{Z}, +, 0\}$$

A monoid is a group without invertibility. For example: 
$$M = \{\mathbb{N},\cdot , 1\}$$     
Inverses are in the different set, $$\mathbb{Q}$$: $$n\cdot n^{-1} = n \cdot 1/n = Id$$

A semigroup is a monoid lacking identity. 
For example: $$S=\{\mathbb{Z^{+}}, +\}$$


## Algebraic structures in PL
In PL, an algebraic structure consists of a type as the carrier set, whose elements are the values of that type, together with the set of operations (functions) that work with that type and respect a certain set of axioms.

Algebraically, a type is a triple: `T = (Va, Op, Ax)`
- `Va` set of values of the carrier type
- `Op` set of operators (functions) with their signatures
- `Ax` set of axioms describing the behavior of operators over elements


## Primitive and algebraic types
The fundamental machine primitives are the basic primitives in Haskell:
- Int, Integer
- Float, Double
- Char

`Int` is a machine primitive and its bit-width depends on the machine architecture. On a x86_64 architecture, its range is: `-2^63 .. 2^63 - 1`.

`Integer` may be also considered a primitive even though it is implemented as an arbitrary-precision arithmetic library, namely GMP (The GNU MP Bignum Library).

`Float` and `Double` are the standard IEEE binary32 and binary64 floating point numbers that have dedicated FPUs in CPU.

`Char` may also be counted among the basic primitives.

The big difference between these 5 basic primitives and other types in Haskell is that these types are **unlifted**, i.e. they do not have the bottom value. No pointers are involved with this types, that is these are the values that live on the stack. These types are marked with a `#` suffix. Their kind is also marked as `#`, not `*`.

All the other types are **lifted** - the bottom value is a member of each type, even of a presumably empty type, such as `data Void`.


However, since we're dealing almost always with lifted types, all Haskell's types may be regarded as algebraic types (structures). Primitive basic types such as `Int` may be regarded as algebraic sum type, as if it were defined like this:

```hs
data Int = -9223372036854775808 | -9223372036854775807 |...| -2| -1 | 0 | 1 | 2 |...| 9223372036854775806 | 9223372036854775807
```


## Algebraic structures in Haskell

The Boolean type is an algebraic sum type, defined exactly as (hand-waving):

```hs
data Bool = False | True
```

This means `True` or `False` are no special values denoting a higher concept such as truth - they are just arbitrarily named data constructors, might as well be called: `data Foo = Down | Up`. Just when should some value be considered `True` has to be manually and explicitly defined, for every value.




<!-- #region Datatypes -->

# Datatypes

- multiple type constraints: `(^) :: (Num a, Integral b) => a -> b -> a`
- -> has right precedence: a -> b -> c is actually a -> (b -> c)
- `() :: ()` unit type that has only one value: `()`
- `IO` is a monad that manages side-effects (print to stdout)
- shared type sig: `(+), (-), (*) :: a -> a -> a`
- use `data` keyword to define a type
- use `class` keyword to define a type class
- use `instance` keyword to implement class' functions



* Concrete datatypes start with capital letter, e.g. `Int`, `Bool`
* Generic types i.e. type params are usually a single letterlowercase, `a`
* Type classes group concrete types by a set of shared properties, `Eq`, `Ord`


**Bool**
- is a boolean type. It can have only two values: True and False.

**Ordering**
- type that can be GT | LT | EQ
- output of `compare` function, e.g. `5 compare 3` -- GT


**Int**
- Int is a bounded integer
- bounded means it has a minimum and a maximum value
- Int is more efficient, but Integer is bigger

**Integer**
- Integer is unbounded integer
- used to represent very big numbers (unbounded), probusing BigInt
- Int is more efficient, but Integer is bigger

**Float**
- single precision floats

**Double**
- double precision floats


**Char**
- character
- denoted by single quotes
- To prevent problems with locales and languages, a Char value contains one Unicode character.


**String**
- type alias for `[Char]`
- double quotes

**tuple**
- type constructor, `(a, b)`
- hetergenuous
- depends on length and component types
- so theoretically there's an infinite number of tuple types

**unit**
- empty tuple `()` is a unit type
- can only have a single value: `()`

**list**
- `[a]`
- heterogenuous

**Functions**
- type of (unapplied) function: `(• -> •)`
- `->` is a function type ctor (there are no data ctors)
- function type's kind: `*`




<!-- #endregion -->
