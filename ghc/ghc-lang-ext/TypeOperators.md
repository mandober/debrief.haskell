# Type operators

`TypeOperators`
Enable type operators. Implies: `ExplicitNamespaces`. 
Allow the use and definition of types with operator names.

In types, an operator symbol like `(+)` is normally treated as a *type param*, just like `a`. Thus in Haskell 98 you can say

```hs
type T (+) = ((+), (+))
-- Just like: type T a = (a,a)

f :: T Int -> Int
f (x,y)= x
```

As you can see, using operators in this way is not very useful, and Haskell 98 does not even allow you to write them infix.

The language `TypeOperators` changes this behaviour:

* Operator symbols become type constructors rather than type variables.

* Operator symbols in types can be written infix, both in definitions and uses.
  For example:

```hs
data a + b = Plus a b
type Foo = Int + Bool
```

* There is now some potential ambiguity in import and export lists; for example if you write `import M( (+) )` do you mean the function `(+)` or the type constructor `(+)`? The default is the function, but with *ExplicitNamespaces*, which is implied by *TypeOperators*, GHC allows you to specify the latter by preceding it with the keyword `type`, thus:

```hs
import M ((+))          -- import (+) function

{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TypeOperators      #-}

import M (type (+))     -- modify import (+) as type ctor with "type" modifier
```

* The fixity of a type operator may be set using the usual fixity declarations but, as with infix type ctors, the function and type constructor share a single fixity.
