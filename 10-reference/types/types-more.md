# Haskell Types

From an algebraic point of view, a type is a triple: `T = (Va, Op, Ax)`
- `Va` set of type values, it is the carrier set of the type
- `Op` set of type operators, including their signatures
- `Ax` set of axioms describing behavior and interaction of operators


Among the *basic types* of Haskell, we could distinguish a group of *builtin types* that are implemented in the compiler, with respect to the low-level native machine types. These are: `Int`, `Float` and `Double`, and `Char`.

`Int` is a machine-dependent integer that is 32-bit on x86, 64-bit on x86_64 architecture.

`Integer` is the arbitrary-precision integer implementation, realized by the [GNU MP Bignum Library](https://gmplib.org/).

`Float` and `Double` are the IEEE standard floating point numbers, `binary32` and `binary64`.

`Char` represents the unicode code points.

`String` is just a type alias for `[Char]`.

`Bool` is also a basic type but it's not special; we could implement it easily if it weren't already defined.


Explicit types (the ones above) are denoted in PascalCase.


*The compound types* types are **function**, **list** and **tuple**.

**Functions** are the main language primitive (building block). A function type does have a type ctor, `->`, but it lacks data ctors. The type of a function is denoted by its *signature*, which is a prominent language item that precedes a definition of a function; although it's not strictly needed, it is considered a good style, so it frequentlly makes an appereance.

**Type parameter** or *type variable* is a similar concept to the usual notion of an (algebraic) variable that ranges over different values, only a type variable ranges over the (available/defined) types. It is denoted in a camelCase, although the convention dictates it is denoted by a single letter. The first type paramater is usually denoted with an `a`,the second witha `b`, and so on (there are rarly more then a few of type params). If a type (signature) contains type params `a` and `b`, that means `a` stands for any concrete type as well as `b`; it only states the fact the `b` may be a different type then `a`. When the intention is to enforce that a type param stands for the same type (once it is determined which), then the same type param (e.g. `a`) is used everywhere. For example a function with the sig `f :: a -> a` is strict in enforcing that the input and output type are the same, while the function `g :: a -> b` allows a different type of output (from the input type). For instance, both signatures as valid to denote the type for a function that takes an integer and returns an integer, but the latter may also represent the type of func that takes a string and returns its length (as int). We could say that the latter is *more general* then the former, or that the former is *more specialized* then the latter.


## Algebraic types

All types in Haskell are algebraic: they are either *sum types* or *product types* (in vanilla GHC in 2019).

**Product types** are analogous to C's structures. In C, a *structure* is a language construct used to define a custom aggregate type, that is, a grouping of values, of possibly different types, into a single type. A structure consists of fields (at least one), with each field of fixed concrete type (no generic types in C), due to contain a particular value of that type when initialized. Structures are used to represent records of related data. If a struct `S` contains a field of type `Bool` (with `True` and `False` values) and another of type `RGB` (with values being `R`, `G` and `B`) then the `S` type has a total of 2*3 = 6 distinct values. Hence the name "product"; they represent "AND" operation with types. The size of a struct is the sum of all its member types.

The sum types are analog to unions in C: they represent *enumeration* of values (of different types). They are defined similarly the same as structs, but unions may have only a single field active at one time. They represent "OR" operation with types. The size of a union type is equal to the size of its largest member.




```c
union Tag {
  int Just;
  null Nothing;
};


// declaration of the new type called `Tag` that is a struct
struct Tag {
  int number;
  char letter;
};
```

```hs
data Tag = Int | Char

data Tag =
  { number = Int
  , letter = Char
  }
```



* Primitives
  * Basic types
    * Machine types
      - Int
      - Float
      - Double
    * Integer
    * Char
  * Bool
  * Compound types
    - function, `->`
    - list, `[]`, `:`
    - tuple, `(,)`
