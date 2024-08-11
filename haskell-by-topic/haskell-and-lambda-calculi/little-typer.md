# Little Typer

## Atoms

```hs
data Atom = ' String

-- examples of atoms
_ = 'a :: Atom
_ = 'atom :: Atom
_ = 'Africa :: Atom
_ = 'o-bla-di :: Atom
```


## Expressions, values, forms

Given a type, every expression described by that type has a **normal form**, which is the most direct way of writing it. If two expressions are the same, then they have identical normal forms; if they have identical normal forms, then they are the same.

*Sameness* (equality) is always according to a type, so normal forms are also determined by a type.

Every expression that is a type has a normal form, which is the most direct way of writing that type. If two expressions are the same type, then they have identical normal forms, and if two types have identical normal forms, then they are the same type.

An expression with a constructor at the top is called a **value**.

*Values and Normal Forms*: not every value is in normal form. This is because the arguments to a data constructor need not be normal. Each expression has only one normal form, but it is sometimes possible to write it as a value in more than one way.

*Constructors and Eliminators*: constructors build values, and eliminators take apart values built by constructors.

*Eliminating functions (lambdas)*: Applying a function to arguments is the eliminator for functions.

Two λ-expressions that expect the same number of arguments are the same if their bodies are the same after consistently renaming their variables.

*α-equivalence*: consistently renaming all occurrecnces of a (binding/bound) variable does not change the meaning of the lambda expression.

Expressions that are not values and cannot yet be evaluated (due to a free variable) are called **neutral**.

*Neutral expressions* make it necessary to expand our view on what it means to be the same. Each variable is the same as itself, no matter what type it has. This is because variables are only replaced consistently, so two occurrences of a variable cannot be replaced by values that are not the same.

```hs
data Pair a b = (a, b)

pair :: Pair a b
pair a b = (a, b)

car :: a -> b -> a
car (x, y) = x

cdr :: a -> b -> b
cdr (x, y) = y
```

`λx → car (x, 'atomblah) = λx.x :: Nat → Nat` 
because the neutral expression x is the same Nat as x.


Is `λx. car x` the same `(Nat, Nat) → Nat` as `λx. car x`?

```hs
e1,e2 :: (Nat, Nat) → Nat
e1 = \x -> car x
e2 = \x -> car x
```

Are `e1` and `e2` the same expressions? Yes, assuming that `car x` is the same `Nat` as `car x`; but `car x` is not a variable, and it is not possible to find its value until the value of `x` is known.

>If two expressions have identical eliminators at the top and all arguments to the eliminators are the same, then the expressions are the same. Neutral expressions that are written identically are the same, no matter their type.

So `car x` is indeed the same `Nat` as `car x` assuming `x` is `(Nat, Nat)`.


If `p` is a `(Pair A D)`, then it is the same `(Pair A D)` as `(cons (car p) (cdr p))`; i.e. `(fst p, snd p) = p`.

```hs
eta :: (a, b) -> Bool
eta = \ p -> (fst p, snd p) == p
```

## Naturals

```hs
data Nat
  zero : Nat
  add1 : Nat → Nat
```

### Eliminator for Naturals

The `which-Nat` is an eliminator for `Nat` that can distinguish between Nats whose values are `zero` and Nats whose values have `add1` at the top.

A `which-Nat` expression has 3 args: *target*, *base* and *step*:

```scheme
(which-Nat target base step)
```

`which-Nat` checks whether target is `zero`; if so, 
the value of the which-Nat expression is the value of `base`. 
Otherwise, if target is `add1 n`, then 
the value of the which-Nat expression is the value of `step n`.

```hs
whichNat :: Nat -> p -> (Nat -> p) -> p
whichNat Z     base step = base
whichNat (S y) base step = step y
-- i.e.
whichNat :: p -> (Nat -> p) -> Nat -> p
whichNat base step = \case
  Z   -> base
  S y -> step y

primRec :: p -> (Nat -> p -> p) -> Nat -> p
primRec base step = \case
  Z   -> base
  S y -> step y (primRec base step y)

{-
whichNat applies the fn `step` on the "smaller Nat" (i.e. `n of `S n`) and that's it. The primRec is similar: it also applies the 'step' fn on the "smaller nat", but here the step fn is binary and the second arg is the recursive call `primRec base step y`, recursing (back again) to primRec fn. Thus, priMRec is more powerful than whichNat.

For example, w1 returns the passed in Nat as a String, while p1 returns the same (the passed in Nat as a String), plus the entire construction that lead to it (as a String).
-}
w1 :: String
w1 = whichNat "(end)" (\ n -> show n) (S (S (S (S Z))))
-- "S (S (S Z))"

p1 :: String
p1 = prim1 "(end)" (\ n c -> show n ++ " : " ++ c) (S (S (S Z)))
-- "S (S (S Z)) : S (S Z) : S Z : Z : (end)"
```

Thus, `which-Nat` both checks whether a number is `zero` and removes the `add1` (i.e. `S`) from the top when the number is not zero.

What is the normal form of

```scheme
(which-Nat 4
  'naught
  (λ (n) 'more))
```

It is `(λ (n) 'more) 3`, i.e. `'more`. Remember, the input Nat is pattern matched against `S n`, and then the `step` fn is applied to `n` (if the input number is non-zero).
