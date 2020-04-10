# Primitives

Data types that are built-in syntax, defined in *GHC.Types*


*GHC.Types* exports:
* Types defined in GHC.Types but not explicitly exported
  - List: []( [], (:) )
  - Type equality: (~)( Eq# )
- Bool(..)
- Char(..)
- Int(..)
- Word(..)
- Float(..)
- Double(..)
- Ordering(..)
- IO(..)
- isTrue#
- SPEC(..)
- Nat
- Symbol
- Any
- type (~~)
- Coercible,
- TYPE
- RuntimeRep(..)
- Type
- Constraint
- VecCount(..), VecElem(..)
* Runtime type representations
  - Module(..)
  - TrName(..)
  - TyCon(..)
  - TypeLitSort(..)
  - KindRep(..)
  - KindBndr



https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-prim-0.5.3/src/GHC-Types.html#Bool


## GHC.Types source file contents

**Kinds**   
The kind of constraints, like `Show a`

    data Constraint

The kind of types with values, e.g. `Int :: Type`

    type Type = TYPE 'LiftedRep

**Nat and Symbol**   
Kind of type-level natural numbers.

    data Nat

Kind of type-level symbols.

    data Symbol

**Any**   
The type constructor `Any` is type to which you can unsafely coerce any lifted type, and back. More concretely, for a lifted type `t` and value `x :: t`, then 

    unsafeCoerce (unsafeCoerce x :: Any) :: t

is equivalent to `x`.

    type family Any :: k where { }



**Lists**    
Lists are built-in syntax, and hence not explicitly exported.
The builtin list type, usually written in its non-prefix form `[a]`:

    data [] a = [] | a : [a]

Unless the *OverloadedLists* extension is enabled, list literals are syntactic sugar for repeated applications of (:) and `[]`:

    1:2:3:4:[] == [1,2,3,4]

Similarly, unless the *OverloadedStrings* extension is enabled, string literals are syntactic sugar for a lists of characters:

    ['h','e','l','l','o'] == "hello"

**Ordering**   

    data Ordering = LT | EQ | GT


## Int, Char, Word, Float, Double

The character type 'Char' is an enumeration whose values represent Unicode (or equivalently ISO\/IEC 10646) code points. This set extends the ISO 8859-1 (Latin-1) character set (the first 256 characters), which is itself an extension of the ASCII character set (the first 128 characters). A character literal in Haskell has type `Char`.

To convert a 'Char' to or from the corresponding 'Int' value defined by Unicode, use 'Prelude.toEnum' and 'Prelude.fromEnum' from the 'Prelude.Enum' class respectively (or equivalently 'Data.Char.ord' and 'Data.Char.chr').

    data {-# CTYPE "HsChar" #-} Char = C# Char#

A fixed-precision integer type with at least the range [-2^29 .. 2^29-1]. The exact range for a given implementation can be determined by using 'Prelude.minBound' and 'Prelude.maxBound' from the 'Prelude.Bounded' class.

    data {-# CTYPE "HsInt" #-} Int = I# Int#

A 'Word' is an unsigned integral type, with the same size as 'Int'.

    data {-# CTYPE "HsWord" #-} Word = W# Word#

Single-precision floating point numbers. It is desirable that this type be at least equal in range and precision to the IEEE single-precision type.

    data {-# CTYPE "HsFloat" #-} Float = F# Float#

Double-precision floating point numbers. It is desirable that this type be at least equal in range and precision to the IEEE double-precision type.

    data {-# CTYPE "HsDouble" #-} Double = D# Double#



## IO

A value of type `IO a` is a computation which, when performed, does some IO before returning a value of type `a`.

There is really only one way to "perform" an IO action: bind it to `Main.main` in the program. When it runs, the IO will be performed. It isn't possible to perform IO from an arbitrary function, unless that function is itself in the `IO` monad and called at some point, directly or indirectly, from `Main.main`.

`IO` is a monad, so `IO` actions can be combined using either the *do-notation* or the `Prelude.>>` and `Prelude.>>=` operations from the 'Prelude.Monad' class.

    newtype IO a = IO (State# RealWorld -> (# State# RealWorld, a #))
    type role IO representational

The *type role* role annotation for IO is redundant but is included because this role is significant in the normalisation of FFI types. Specifically, if this role were to become nominal (which would be very strange, indeed!), changes elsewhere in GHC would be necessary (See FFI type roles in `TcForeign`).


## (~) and Coercible

## Bool, and isTrue#

## SPEC

## Levity polymorphism

## Runtime representation of TyCon



https://downloads.haskell.org/~ghc/latest/docs/html/libraries/ghc-prim-0.5.3/src/GHC-Types.html#Bool
