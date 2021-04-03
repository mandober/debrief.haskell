# Constructors

Haskell has type and data constructors, both of which appear in the declaration of algebraic data types.

The two most common forms of ADTs are product types and sum types.

(i.e., tagged or disjoint unions, coproduct types or variant types).
(i.e. tuples and records)

The sum ADT brings different types together under a single umbrella type, such that 

There is a single type constructor on the left-hand side (left to the equals sign, or left to the `where` clause when using the GADTs syntax).

and there are one or more data constructors on the right-hand side in a declaration.

Type constructors produce a sum type, given the initial tag type and the corresponding type.


```hs
-- sum type
data SumTy a b = SC₁ a | SC₂ b

-- product type
data ProdTy a b = PC a b

newtype T2 a b = C2 a b
newtype T2 a b = C2 a b
type    T3 a b = C3 a b

data T a where
  MkT :: forall k (a :: k). Proxy a -> T a
```
