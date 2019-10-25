# Classes

Type Classes
- type class instances are types that share common properties and ops
- `=>` type class constraint
- `(+) :: Num a => a -> a -> a`
- plus works for any type, `a`, if it's an instance of the `Num` type class
- multiple type constraints: `(^) :: (Num a, Integral b) => a -> b -> a`
- -> used in type annotations for functions: a -> b -> c
- means func takes value of type `a` and `b` and returns value of type `c`
- -> has right precedence: a -> b -> c is actually a -> (b -> c)
- `() :: ()` unit type that has only one value: `()`
- `IO` is a monad that manages side-effects (print to stdout)
- shared type sig: `(+), (-), (*) :: a -> a -> a`
- use `data` keyword to define a type
- use `class` keyword to define a type class
- use `instance` keyword to implement class


## Class constraints

Places to put class constraints:

* In `class` declarations:
  - makes the current class a subclass of another class

```hs
-- class declaration
class Num a where

-- class declaration with Eq constraint for `a`
class (Eq a) => Num a where

-- the type `a` now first has to
-- be in Eq before being in `Num`

-- Num is a subclass of Eq
-- Eq is a superclass of Num
```

Subclass
- type `a` now first has to be a member of `Eq`, before it can be in `Num`
- `Num` is a *subclass* of `Eq`
- `Eq` is a *superclass* of `Num`
- This also means that if we define default impl for some functions of the Num class, we can use functionality that `Eq` superclass provides, i.e. (==)



<!-- #region Datatypes -->
# Datatypes

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
- list of characters is String

**String**
- type alias for `[Char]`
- double quotes

*tuple*
- type constructor, `(a, b)`
- hetergenuous
- depends on length and component types
- so theoretically there's an infinite number of tuple types

*unit*
- empty tuple `()` is a unit type
- can only have a single value: `()`

*list*
- `[a]`
- heterogenuous

<!-- #endregion -->

<!-- #region Classes -->

## Classes

**Eq**
- used for types that support equality testing
- functions its members implement are == and /=
- Member types: (), Bool, Char, Ordering, Int, Integer, Float, Double

**Ord**
- is for types that have an ordering, e.g. `(>) :: (Ord a) => a -> a -> Bool`
- All the types we covered so far except for functions are part of Ord
- Ord covers all the standard comparing functions such as >, <, >= and <=
- `compare` func takes two same type Ord members and returns an `Ordering`
- `Ordering` is a type that can be GT, LT or EQ
- To be a member of Ord, a type must first be in `Eq`

**Show**
- Members of Show can be presented as strings
- All types covered so far except for functions are a part of Show
- The most used function that deals with the Show typeclass is `show`
- It takes a value whose type is a Show and presents it as a string

**Read**
- is sort of the opposite typeclass of Show
- `read` function takes a string and returns a type which is a member of Read
- All types covered so far except for functions are a part of Read

**Enum**
- Enum members are sequentially ordered types, they can be enumerated
- tuple, list,
- we can use its types in list ranges
- They have defined successors and predecesors, `succ` and `pred` functions
- Member types: (), Bool, Char, Ordering, Int, Integer, Float, Double

**Bounded**
- members have an upper and a lower bound, `minBound` and `maxBound` funcs
- minBound and maxBound have a type of `(Bounded a) => a`
- In a sense they are polymorphic constants
- members: Bool, Char, Int
- tuples are part of Bounded if the components are also in it

**Num**
- Int, Integer, Double, Float
- Its members have the property of being able to act like numbers
- To be in `Num`, a type must already be in `Show` and `Eq`

**Integral**
- includes Int and Integer
- useful function for dealing with numbers is fromIntegral:
- `fromIntegral :: (Num b, Integral a) => a -> b`

**Floating**
- includes Float and Double

<!-- #endregion -->

<!-- #region Class declaration -->

## Class declaration

Type classes allow us to declare which types are instances of which class, and to provide definitions of the overloaded operations associated with a class. 

For example, to define a type class containing an equality operator: 

```hs
class Eq a where 
  (==) :: a -> a -> Bool
```
Here Eq is the name of the class being defined, and == is the single operation in the class. This declaration may be read "a type a is an instance of the class Eq if there is an (overloaded) operation ==, of the appropriate type, defined on it." (Note that == is only defined on pairs of objects of the same type.)

The constraint that a type a must be an instance of the class Eq is written Eq a. Thus Eq a is not a type expression, but rather it expresses a constraint on a type, and is called a context. Contexts are placed at the front of type expressions. For example, the effect of the above class declaration is to assign the following type to ==: 

```hs
(==) :: (Eq a) => a -> a -> Bool
```

This should be read, "For every type a that is an instance of the class Eq, == has type a->a->Bool". This is the type that would be used for == in the elem example, and indeed the constraint imposed by the context propagates to the principal type for elem: 

```hs
elem :: (Eq a) => a -> [a] -> Bool
```

This is read, "For every type a that is an instance of the class Eq, elem has type a->[a]->Bool". This is just what we want---it expresses the fact that elem is not defined on all types, just those for which we know how to compare elements for equality.

But how do we specify which types are instances of the class Eq, and the actual behavior of == on each of those types? This is done with an **instance declaration**.

<!-- #endregion -->

<!-- #region Instance declaration -->

## Instance declaration

**Instance declaration** allow us to specify the types that are members of the class and then to define the actual behavior of overloaded operations associated with that class.

```hs
instance Eq Integer where 
  x == y =  x `integerEq` y
```
The function `integerEq` happens to be the primitive function that compares integers for equality, but in general any valid expression is allowed on the right-hand side, just as for any other function definition.

The overall declaration is essentially saying: "The type Integer is an instance of the class Eq, and here is the definition of the method corresponding to the `==` operation".
```hs
instance Eq Float where
  (==) = floatEq
```
Similarly, we have add another type, Float, to the class by also defining its behaviour wrt `==`.

Let's examine Eq class:

```hs
class  Eq a  where
  (==), (/=)            :: a -> a -> Bool
  x /= y                =  not (x == y)
```

The type sigs are given for both operations (they're the same), but `/=` also has a method body defined - this is *default implementation*.

If a method for a particular operation is omitted in an instance declaration, then the default one defined in the class declaration (if any) is used.

<!-- #endregion -->


<!-- #region Class extension -->
## Class extension

When defining a class `Ord` we may find it useful if Ord would integrate the methods available on `Eq` class alongside defining its own set of comparison operations and minimum and maximum functions. In such case we'd say that Eq is superclass of Ord. This is implemented similarly to type constraints only this time a class is constrained with another:

```hs
class (Eq a) => Ord a where
  (<), (<=), (>=), (>) :: a -> a -> Bool
  max, min             :: a -> a -> a
```

We say that Eq is a **superclass** of Ord or, conversely, that Ord is a **subclass** of Eq. 

> Any type which is an instance of Ord must also be an instance of Eq.

One benefit of such class inclusions is shorter *contexts*: a type expression for a function that uses operations from both the Eq and Ord classes can use the context `(Ord a)`, rather than `(Eq a, Ord a)`, since Ord "implies" Eq.

More importantly, methods for subclass operations can assume the existence of methods for superclass operations.

Haskell also permits *multiple inheritance*, since classes may have more than one superclass.

Class methods are treated as *top level declarations* in Haskell. They share the same namespace as ordinary variables; a name cannot be used to denote both a class method and a variable or methods in different classes.

Contexts are also allowed in data declarations, although very discouraged.

Class methods may have additional class constraints on any type variable apart from those defined in the class declaration context. For example, in this class:
```hs
class C a where
  m :: Show b => a -> b
```
the method `m` requires that type `b` is in class `Show`. However, the method `m` could not place any additional class constraints on type `a`. These would instead have to be part of the context in the class declaration.




<!-- #endregion -->
