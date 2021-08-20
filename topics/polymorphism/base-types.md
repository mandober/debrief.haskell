# Base types

Every PL has a set of base types, which are the most elementary types that necessarily include integers and floats, possibly some other simple types as well, like characters.

Usually, although it is not required, these base types correspond to raw machine types, which are machine integers of the same bitness (64-bit wide integers) as the size of a machine word (8 bytes at x86_64). The same size, 8 bytes, is the size of double-precision floating point numbers. These two, machine integers and floats, are the true machine types since each has its own dedicated circutry units for doing arihmetic operations. All the other primitive machine types are derived from them by restricting their size. If the size of proper machine integers is 8 bytes, then `Int64` denotes them (or just `Int` since these are the default). `Int64` has the size of 64 bits, that is 8 bytes, and can represent 2⁶⁴ distinct values.

## Language primitive types

Every PL has a set of *base types*, which are the most elementary types, often, but not necessarily, corresponding to the set of machine primitive types. In fact, the base types are sometimes also referred to by the name *primitive types*, but this is from the aspect of a particular PL, i.e. they belong to the set of language primitives, which has other language coinstructs besides types. The language primitive types shouldn't be confused with the machine primitive types, although in some PLs they are indeed the same. Yet another name for them is *scalar types*.

Anyway, the purpose of the base types, as the most elementary types offered by a PL, is to be used as the building blocks for constructing more complex types, due to their essential property of *atomicity*, i.e. they cannot be decomposed further.

This means that when you writing an evaluator for a language, you keep evaluating a complex expression until it gets completely decomposed into the atomic components, each of some base types. At that moment the evaluation is completed because each base types stands for itself (cannot be decomposed further).

### Example: simple evaluator

<!-- #region Example: simple evaluator -->

Reaching the base type is the goal of evaluation. Consider, for example, a simple arithmetic evaluator for integers, `EInt`, with just two operation, viz. addition, `EAdd`, and multiplication, `EMul`.

The `eval` function takes an expression, `Exp`, that part is intuitive, since `eval` is bound to evaluate expressions. But what to? What type shall it return?

It should return a type from the set of base types. Base types are the goal of evaluation! In this simple example, there is only one base type, `Int`, and, correspondingly, there is only one data ctor, `EInt`, that just wraps `Int`'s so they can be also trated as expressions, `Exp` (you might say `EInt` data ctor serves just to lift the base type `Int` into expression level/context `Exp`).

So `eval` reduces expressions of non-base types into a value of base type.

```hs
data Exp
  = EInt Int
  | EAdd Exp Exp
  | EMul Exp Exp

eval :: Exp -> Int
eval (EInt n) = n   -- EInt reduces to itself, to an Int
eval (EAdd e1 e2) = eval e1 + eval e2
eval (EMul e1 e2) = eval e1 * eval e2

x1 = eval (EInt 42) -- 42
x2 = eval (EAdd (EMul (EInt 2) (EAdd (EInt 5) (EInt 3))) (EInt 7)) -- 23
```

The expression `EInt n` reduces to the `n`, i.e. it is the only expression that needs no further evaluation as it represents the base type `Int`; it has reached the final form, the form that cannot be reduced or decomposed further.

<!-- #endregion -->
