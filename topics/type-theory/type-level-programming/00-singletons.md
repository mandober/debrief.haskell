# Singleton types

Haskell programmers have been experimenting with dependent
types for at least a decade, using clever encodings that push the
limits of the Haskell type system. However, the cleverness of these
encodings is also their main drawback. Although the ideas are inspired by dependently typed programs, the code looks significantly
different. As a result, GHC implementors have responded with extensions to Haskell's type system, such as GADTs, type families,
and datatype promotion. However, there remains a significant difference between programming in Haskell and in full-spectrum dependently typed languages. Haskell enforces a phase separation between runtime values and compile-time types. Therefore, singleton
types are necessary to express the dependency between values and
types. These singleton types introduce overhead and redundancy
for the programmer.


We link the type-level literals to specific run-time values via singleton types. The singleton types are defined in the module `GHC.TypeLits`.

A singleton type is a type with one interesting inhabitant. We define a whole family of singleton types, indexed (parameterized) by type-level literals:

```hs
data Sing :: a -> Type
```

For example, `Sing 127` and `Sing "oh"` are singleton types. 
The intuition is that the only inhabitant of `Sing n` is the value `n`.

Note that `Sing` has a polymorphic kind because sometimes we apply it to naturals (of kind `Nat`) and sometimes to symbols (of `Symbol` kind).

Values of type `Sing n` are build using the special overloaded constant `sing`:

```hs
class SingI a where
  sing :: Sing a

-- Built-in instances for all type-literals
instance SingI 0        where sing = … -- singleton representing 0
instance SingI 1        where sing = …
instance SingI "hello"  where sing = …
-- ... etc.
```
