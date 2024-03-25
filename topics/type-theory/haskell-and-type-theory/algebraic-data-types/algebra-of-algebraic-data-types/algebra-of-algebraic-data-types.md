# Algebra of ADTs

https://bartoszmilewski.com/2015/01/13/simple-algebraic-data-types/

The algebra of algebraic data types (ADTs) is justified by the correspondencies between type constructions, type expressions and type cardinalities on one side, and algebraic (arithmetic) operations and identities on the other.

The key to calling types algebraic starts by considering the type cardinality (i.e. the number of values in a type).


```hs
0 ≅ Void
1 ≅ ()
+ ≅ Either
⨯ ≅ (,)
^ ≅ (→)
```

The sum types have `Void` as the identity, and the product types have unit or `()` as the identity. They are analogous to addition and multiplication, with `Void` corresponding to 0, and `()` to 1.


algebra     | type             | note
------------|------------------|----------------------------------------
0           | `Void`           | sum type (additive) identity
1           | `()`             | product type (mulitplicative) identity
a + b       | `Either a b`     | sum type (additive binop)
a × b       | `(a, b)`         | product type (mulitplicative binop)
bᵃ          | `a -> b`         | exponential type (exponential object)
1 + 1       | `Boolean`        |
1 + a       | `Maybe a`        |
x = 1 + ax  | `List a`         |


## Correspondencies between ADTs and algebra

```hs
-- Basic constants

-- 0
data Void

-- 1
data () = ()

-- Basic operations

-- (+) that is a+b, Either is canonical sum
data Either a b = Left a | Right b

-- (×) that is a×b, Pair is canonical sum
data (,) a b = (,) a b

-- (^) that is b^a
data (->) a b = (->) a b

-- Compound types

-- 1 + 1
data Bool = True | False
type Bool = Either () ()                          -- canonically

-- 1 + a
data Maybe a = Nothing | Just a
type Maybe a = Either () a                        -- canonically

-- L = 1 + a×L
data List a = Nil | Cons a (List a)
data List a = Nil | Cons (a, List a)
type List a = Either () (a, List a)               -- canonically

-- T = 1 + a×T×T
data Tree a = Nil | Branch a (Tree a) (Tree a)
data Tree a = Nil | Branch (a, Tree a, Tree a)
type Tree a = Either () (a, Tree a, Tree a))      -- canonically

-- Sum of products, a⨯b + c⨯d
type SoP a b c d = Either (a, b) (c, d)

-- Product of sums, (a+b) ⨯ (c+d)
type PoS a b c d = (Either a b, Either c d)
```


Either a (Either b (c, d))    ~ a + b + cd
a -> b -> c ≅ (a, b) -> c     ~ (cᵇ)ᵃ = (cᵃ)ᵇ = c^(a*b) = cᵃ + cᵇ
Either (a, ()) ((), d)          ~ ab + cd
Either (a b) (a c)            ~ ab + ac

data List a = Nil | (a, List a)                     L = 1 + L × a
data List a = Nil | (List a, a)                     L = L × a + 1
