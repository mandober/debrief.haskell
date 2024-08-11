# Generic programming :: In general

A Haskell data type is *possibly recursive sum of products*. Thus, 
a Haskell data type is really a tree with Sum types as nodes, and product types as leaves, with atoms (regular types like Int, (), Bool, etc.) as product's types children (fields).


```
                Exp a
  |     |     |      |         |
 Add   Neg   Mul    Var       Lam
 / \    |    / \     |        / \
a   a   a   a   a  String  Par   Exp
                            |     |
                          String  a
```

3 ways to do generic programming: `Data`, `Typeable`, `Generic`.

## Typeable and Data

Normally, Haskell erases types by the runtime, `Typeable` allows to preserve them (and maybe do something differently for a one type compared to what we doi with others).

The module `Data.Typeable` is older, the newer API is in `Data.Reflection`.

`Typeable` allows runtime type identification. But only of the type (ctor), i.e. only of the left-hand side of a type declaration. `Data` is used to get the right-hand side of a type declaration, i.e. to get the info on data ctors.


## GHC.Generic

- `Generic` is for type ctors of kind `Type`
- `Generic1` is for type ctors of kind `Type -> Type`
