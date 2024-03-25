# Constraints

The `Constraint` kind is reserved for things that can appear on the LHS of the context arrow. There are 3 sorts of constraints:
- class (fully-saturated class) constraints, like `Show a `
- tuples of constraints
- type equalities, like `Int ∼ a`

1. Class constraints are the most common, e.g. (==) has the `Eq` constraint:   
`(==) :: Eq a => a -> a -> Bool`

2. Tuples of constraints are similarly well-known:    
`sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)`

3. *Type equalities* are enabled via *GADTs* language pragma.


Compare the following two programs:

```hs
{-# LANGUAGE GADTs #-}

five :: Int
five = 5

five' :: (a ∼ Int) => a
five' = 5
```

Both five and five' are identical as far as Haskell is concerned.

While five has type Int, five' has type a, along with a constraint saying that a equals Int. Of course, nobody would actually write five', but it's a neat feature of the type system regardless.

Type equalities form an equivalence relation, meaning they obey the axioms of:
* reflexivity:  `a ∼ a`
* symmetry:     `a ∼ b <-> b ∼ a`
* transitivity: `a ∼ b ∧ b ∼ c -> a ∼ c`
