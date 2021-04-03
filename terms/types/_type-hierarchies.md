# Type Hierarchies


## Primitive Types

The fundamental machine primitives are also the primitive types in Haskell: `Int`, `Integer`, `Float`, `Double` (`Char` may be also included, but it isn't ASCII character but a Unicode codepoint, and not really a machine type).

$$Primitives = \{ Int, Integer, Float, Double, Char \}$$

Save for memory concerns, `Integer` corresponds to the integer set in math:
$$Integer \equiv \mathbb{Z}$$. The members of are the whole numbers in math and in Haskell, $$x \in Integer$$. Here $$x$$ is a *value parameter* that **ranges over** the $$Integer$$ set.

$$
Integer = \{\forall x : x \in \mathbb{Z} \} \\
Integer = \{- \infty, \cdots, -1, 0, 1, \cdots, \infty\}
$$


## Type classes

All number primitive types are members of the `Num` type class.

$$Num = \{ Int, Integer, Float, Double \}$$


set of all typeclasses, Ξ    
ζ ranges over typeclasses: ∀ζ:ζ ∈ Ξ

set of all type ctors, P    
ρ ranges over typeclass ctors = {functor, applicative, monad}

τ :: (Monad m) => m a -> (a -> m b) -> m b


## Types as Sets

There are simple primitive types, scalars like `Int`, `Integer` and `Float`, which have an immediate primitive value like 42.

For example, `42` is a primitive value, an inhabitant of the primitive set `Int`, and so denoted by `42 :: Int`. Thus, from a set-theoretical aspect, 42 is a member of the `Int` set, so primitive sets are like lower-order sets.

However, there are also compound types, like `[]` and `Maybe`, which are like higher-order sets - they are sets whose elements are the primitive sets (`Int`, `Integer`, `Float`, `Double`). Primitive sets/types are the members, but at the same time also the proper subsets/subtypes of higher-order sets/types.

It could be said that higher-order types (HOT) are incomplete, so a `Maybe` is never the concrete type. For that, it has to be "instantiated" with a primitive type, like `Int`, to becomes concrete and complete as `Maybe Int`.

Arithmetics is concerned with concrete values, which may be the integers, but the set itself is rarely mentioned. That is, the type of numbers is secondary, understood to be some appropriate set like the set of integers. On the other hand, in algebra, where the focus is shifted from numbers to the operations, concrete numbers are rarely mentioned, being replaced with variables. There, variables like $$x$$, $$y$$, $$z$$ range over the domain set, that could be the set of integers, $$\mathbb{Z}$$, which is denoted by $$x,y,z \in \mathbb{Z}$$.

Similarly, when dealing with HOTs, if the concrete (primitive) types that complete them, are not central to the analyses, they are relaced with type variables, `a`, `b`, `c`. In that case we can speak of the `Maybe a` type, where the type variable `a` ranges over *any* type, and it may be instantiated as an `Int`. However, unlike primitive types, which cannot contain other primitive types, HOTs can actually contain other HOTs, so the `a` in `Maybe a` can also be instantiated with a `Maybe Int`, producing `Maybe (Maybe Int)`.

A generic type like `Maybe a` may produce types like    
`Maybe Int`, `Maybe (Maybe Int)`, `Maybe (Maybe (Maybe Int))`,     
but a more specific generic type like `Maybe (Maybe a)`    
(cannot produce `Maybe Int`) can only produce `Maybe (Maybe Int)`, etc.

As a further complication, a HOT can also be replaced by a variable, `m`, `f`, producing types like `f a`, where `f` stands for some typeclass (e.g. Functor, Applicative, Monad).

`f a` may be instantiated as `Maybe Int` or `[Int]`.
