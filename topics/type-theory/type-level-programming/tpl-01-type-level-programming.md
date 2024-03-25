# Type-level programming

**Type-level computation** (TLC) is a type of computation that runs statically (run silent, run deep), i.e. only during the compile-time. Naturally, the entry level to it is a statically-typed language, but it seems practical only in strongly-typed functional languages, taken furthermost with the pure-bred FPLs like Haskell.

**Type-level programming** (TLP) is than the approach to maximize the utility of type-level computation. Most compiled PLs perform some types of computation during compile-time (CT), but these are largly limited to a small set of very safe computations, such as constant folding. FPLs remove such restrictions and allow users to run virtually any kind of computation they can encode at the type level.



so it is a very favorable way to keep one's invariants solid (the program would not compile otherwise).

Type-level Programming (TLP) is writing programs that run at compile-time, unlike term-level programming which is writing programs that run at run-time.


One of the canonical examples of type level programming is expressing numerical computations solely with types, like enhancing a list type with a type that encodes its length. For such task, it is necessary to lift the representation of numbers to the type level, and a common way to do it, is a unary representation of natural numbers due to Peano.

More preciselly, we first define Peano numbers as a new data type:

```hs
data Peano = Z | S Z
```

which only gives us a single new type, `Peano`. However, we need to express *numbers as types*, meaning each number must have a unique type repr. Another problem is that such a repr must be around during runtime, which won't be the case when we promote `Peano` type using *DataKinds* extension. It makes available type-level natural numbers, of kind `Nat`, type level strings, kind `Symbol`, type level lists and tuples, etc.







































## type-combinators package

https://hackage.haskell.org/package/type-combinators
type-combinators: A collection of data types for type-level programming

Modules:

Data.Type
- Data.Type.Boolean
- Data.Type.Combinator
- Data.Type.Conjunction
- Data.Type.Difference
- Data.Type.Disjunction
- Data.Type.Fin
- Data.Type.Fin.Indexed
- Data.Type.Index
- Data.Type.Index.Trans
- Data.Type.Length
- Data.Type.Nat
- Data.Type.Nat.Inequality
- Data.Type.Option
- Data.Type.Product
- Data.Type.Product.Env
- Data.Type.Product.Lifted
- Data.Type.Remove
- Data.Type.Subset
- Data.Type.Sum
- Data.Type.Sum.Lifted
- Data.Type.Vector

Type.Class
- Type.Class.Higher
- Type.Class.Known
- Type.Class.Witness

Type.Family
- Type.Family.Bool
- Type.Family.Constraint
- Type.Family.Either
- Type.Family.List
- Type.Family.Maybe
- Type.Family.Monoid
- Type.Family.Nat
- Type.Family.Tuple
