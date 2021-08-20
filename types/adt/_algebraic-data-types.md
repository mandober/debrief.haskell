# Algebraic Data Types

- algebraic data type
- algebraic type constructor
- sum (algebraic) type
- product (algebraic) type
- type declaration
- data keyword
  - newtype keyword
  - type keyword
- type name
- type constructor
- type parameters
- type kind (explicit)
- type quantifier (explicit)
- data constructors
- fields
- accessor (function)
- records
- GADTs
- algebra of types (relation to type theory)




* Besides primitives, new values of custom type may be constructed using *algebraic data type* (ADT) construction.

* Haskell supports only sum and product ADTs (no dependent types).

* Both sum and product ADTs are declared using the `data` keyword (the `type` and `newtype` keywords are the performance-related versions).

* Custom datatypes are introduced using the `data` keyword, followed by the type name (arbitrary but must begin in uppercase), possibly followed by a set of type parameters, then an equal sign, followed by a set of data constructors.

## Type constructor

## Data constructor


* The type name is a type constructor and the type's constructor. Interplay of Different schemes is at work as to whether and in what number the type params follow a reference to a type's name.

* To query the kind of the type ctor, state the type's name alone. Since type ctors also support currying in relation to their type params, attaching a type param shows the kind of a partially applied type ctor.


```hs
-- declaration
data Or p q = Lhs p | Rhs q

-- query kind
:k Either
Either :: (* -> * -> *)
:k Either 


```



Sometimes a sole type's name stands for something 

the number of declared type parameters that follow the type when it is mentioned reference to the type, according to different language constructs.


* The possible constructors are either sum types or of product types. All datatypes in Haskell can expressed as sums of products. A sum type is a set of options that is delimited by a pipe.


## Accessor

* Accessor is a function attached to a data declaration that retrieves the data from the field it names.
* It acts as a name for a field, and for a function that gets the field's value
* Accessor is just a convenince
* Accessor function is just function application, the same as field access
* Accessor is often experienced as its signature, which has an implicit parameter (i.e. type itself) to the one stated, but actually accessor means the compiler generates the approapriate function implementation (i.e. it is generated function definition, not just a floating function declaration).
* The relation between the accessor and the enclosing data ctor: the ctor constructs a value by wrapping, the accessor deconstructs it by unwrapping.


```hs
-- ADT with an accessor
newtype Parser a = Parser { parse :: String -> [(a, String)] }

-- means the same
newtype Parser a = Parser ( String -> [(a, String)] )
-- plus
parse :: Parser a -> String -> [(a, String)]
parse (Parser p) s = p s
```

## Algebra of types

https://stackoverflow.com/questions/9190352/abusing-the-algebra-of-algebraic-data-types-why-does-this-work

- Void: 0
- Unit: 1 or x
- Product: ×
- Sum: +
- Exponential: ^ (a -> b <=> bᵃ)

data List a = Nil | Cons a (List a)
L = 1 + a × L
L = 1 + La
L - La = 1
L (1 - a) = 1
L = 1 / (1 - a)
L = 1 + a + a² + a³ + ...
power series expansion of 1 / (1 - X) (used in a totally unjustified way) to derive an interesting result, namely, that an L type is either Nil, or it contains 1 element, or 2, or 3, etc.


data Tree a = Nil | Branch a (Tree a) (Tree a)
T = 1 + X • T²
X • T² - T + 1 = 0
T = (1 - √(1 - 4 • X)) / (2 • X)
T = 1 + X + 2X² + 5X³ + 14X⁴ + ...
so there is:
- 1 empty binary tree
- 1 binary tree with 1 element (at the root)
- 2 binary trees with 2 elements (at the root, left/right branch)
- 5 binary trees with 3 elements,
- etc.










## Refs

https://en.wikipedia.org/wiki/Algebraic_data_type
