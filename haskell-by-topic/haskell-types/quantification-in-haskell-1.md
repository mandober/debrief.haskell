# Quantification in Haskell

* Universal and Existential Quantification in Haskell - Stepan Prudnikov, 2022
https://serokell.io/blog/universal-and-existential-quantification

## Universal quantification

`forall` is implicit in signatures, it is added automatically. `ExplicitForAll` extension enables us to add forall ourselves explicitly, but what for? That is, are there situation that make a difference? Yes. Consider this example function that has an annotated helper function:

```hs
example :: a -> [a] -> [a]
example x rest = pair ++ rest
  where
  pair :: [a]
  pair = [x, x]
```

It won't compile because of the implicit `forall` insertions, as if we had:

```hs
example :: forall a. a -> [a] -> [a]
example x rest = pair ++ rest
  where
  pair :: forall a. [a]
  pair = [x, x]
```

which means `a` in the main sig and `a` in the helper function's sig are distinct when they, in fact, should be the same. That is, the `pair` is a polymorphic function that promises to return a list of any type `a`, but its implementation actually returns a list of type `a`, where this `a` is the same `a` as instantiated by the example function. So, what we need to say is exactly that - we need to fic the sig of `pair` so its its type param `a` refers to the example's type param `a`. The `pair` should be a monomorphic function that return a list of the same type that was instantiated in the `example`. We can fix this by removing the `forall` quantifier in `pair`:

```hs
example :: forall a. a -> [a] -> [a]
example x rest = pair ++ rest
  where
  pair :: [a]
  pair = [x, x]
```

By enabling the `ScopedTypeVariables` extension the type param `a` from the main function scopes over its entire definition, especially scoping over the definitions of the nested functions (if so is desired).

It is usually these nested function where the difference between explicit and implicit forall quantification arises.

Other extensions that benefit from the usage of `ExplicitForAll` are `LiberalTypeSynonyms`, `RankNTypes`, and a few more.

## Existential quantification

>The use of the existentially quantified type variable makes the instantiation happen at the *definition site*, as opposed to the *call site* in case of the universally quantified type variable.

Haskell also supports existential quantification, which is also done using the `forall` keyword. This is valid due to the equality of these two constructs:

>`(exists a. f a) -> g` ≡ `forall a. (f a -> g)`

These two formulas are equivalent in terms of first-order predicate logic:

`∃xPx -> Q` ≡ `∀x(Px -> Q)`

For a theoretical proof of this statement, see this thread:

* What is the theoretical basis for existential types?
https://stackoverflow.com/questions/10753073/whats-the-theoretical-basis-for-existential-types?rq=1



## Equivalence of formulas

Intuitionistic logic (IL) is like the classic logic without the law of the excluded middle (LEM) and double negation elimination (DNE):

- DNE is not an axiom: `¬¬P ⇒ P` (but DNI, `P ⇒ ¬¬P`, is an axiom)
- LEM is not an axiom: `P ∨ ¬P`

Intuitionistic logic (IL), like classic logic (CL) except that
1. LEM, `P ∨ ¬P`, is not an axiom in IL
2. DNE is not an axiom in IL, so `¬¬P ⇏ P`
3. DNI is an axiom is IL, so `P ⇒ ¬¬P`


>In IL, `P ⇒ ¬¬P ⇏ P` since LEM, `P ∨ ¬P`, is not an axiom.


DeMorgan's laws:
- `∀x.P(x)` ≡ `¬∃x.¬P(x)`
- `∃x.P(x)` ≡ `¬∀x.¬P(x)`

thus
- `∀x.¬P(x)` ≡ `¬∃x.¬¬P(x)` ≡ ¬∃x.P(x)  … uses DNE, P ⇒ ¬¬P (!)



We can derive the equivalences:
1. `(∀x. P ⇒ Q(x))`  ≡  `P ⇒ (∀x. Q(x))`
2. `(∀x. Q(x) ⇒ P)`  ≡  `(∃x. Q(x)) ⇒ P`


1. Equivalence: `(∀x. P ⇒ Q(x))` ≡ `P ⇒ (∀x. Q(x))`

```hs fol
1  (∀x. P ⇒ Q(x))
2  (∀x. ¬P ∨ Q(x))
3  ¬P ∨ (∀x. Q(x))
4  P ⇒ (∀x. Q(x))

-- tidy up
1  ∀x( P ⇒ Q(x))
2  ∀x(¬P ∨ Q(x))        -- ⇒ to ∨
3  ¬P ∨ (∀x.Q(x))       -- pull in ∀ since P does not depend on it
4  P ⇒ ∀x.Q(x)          -- ∨ to ⇒
```


2. Equivalence: `(∀x. Q(x) ⇒ P)` ≡ `(∃x. Q(x)) ⇒ P`   
   (this one will be used later)

```hs fol
1  (∀x. Q(x) ⇒ P)
2  (∀x. ¬Q(x) ∨ P)
3  (¬¬∀x. ¬Q(x)) ∨ P -- how does DNI saves you from DNE later?!
4  (¬∃x. Q(x)) ∨ P   -- this is really: (¬¬(¬∃x).¬(¬Q(x))) ∨ P (not in IL 📛)
5  (∃x. Q(x)) ⇒ P

-- tidy up
1  ( ∀x(  Q(x) ⇒ P))
2  ( ∀x( ¬Q(x) ∨ P))      -- ⇒ to ∨
3  ( ∀x( ¬Q(x))) ∨ P      -- since P does not depend on x
4  (¬∃x(¬¬Q(x))) ∨ P      -- since ∀x¬Qx ≡ ¬∃x¬¬Qx ≡ ¬∃xQx in CL (not in IL 📛)
5  ¬(∃x(  Q(x))) ∨ P      -- DNE (not valid in IL! 📛)
6   (∃x(  Q(x)))  ⇒ P     -- ∨ to ⇒
```

Note that these laws hold in intuitionistic logic as well.
~~No, they do not since DNE cannot be avoided (steps 3-4 or 3-4-5). Either way you try there just gotta be DNE. But anyway, let's just take it for granted.~~


The two derived laws are cited in the paper:    
"First-class Polymorphism with Type Inference", Mark P. Jones, 1997
http://web.cecs.pdx.edu/%7Empj/pubs/fcp.html


### Note on notation

In these two formulas, `Q` (or `P`) does not mention (depend on) `x`, so only `Q` is written. So, `Q` actually stands for some formula, not for some predicate, since writing just `P` for a predicate makes no sense - a predicate must mention the names it is applied to, e.g. `Pa` or `Q(a)` or `Lab`, where `a` and `b` are names (constants), not variables. A predicate can also be applied to variables, in w hich case those variables are noramlly bound by a quantifier that has that predicate in scope, or they appear free (which makes less sense), e.g. `∀xPx` is a bona fide formula, as is `∀x∃y(Lxy)`; these are closed formulas since all vars are bound. A variable can appears free, but it is usually free in the context of a subformula, not free in the entire formula (although even this is possible, it makes little sense). 

On the other hand, we have formulas which we cannot quantify over - we cannot even quantify over predicates in FOL, let alone over formulas - but we can quantify and otherwise manipulate formulas in the metalanguage. So, `∀x.P ⇒ Q(x)` must be interpreted as a *sentence in the metalanguage*, i.e. `P` must be interpreted as a formula not as a predicate.

This occurs when we want to say that a formula, `F`, does not mention some name or variable `x`, so we write only `F`. And when we want to say that the formula `F` does actually contains (mentions) `x`, we write something like `F(x)`, which represents some arbitrary long formula, we label `F`, with `x` occurring one or more times somewhere in it; it is like saying `F(…x…)`. Thus, `F` alone just represents some arbitrary formula which doesn't mention some variable we are interested in. This is especially used when specifying substitutions, where we say something like `F[x/t]`, meaning replace all vars `x` in the formula `F` with term `t`.

>Singling out a variable of interest `x` in the formula `P` is often written as `P(x)`. This may create ambiguity with (sub)expressions like `P(x)` that denote a predicate `P` applied to a variable `x`. A formula that does not single any particular name is often written as a bare `P`.


## Simple Types

The simplest types are easy to work with. For example:

data T = Con Int | Nil
The constructors and accessors have the following type signatures:

Con :: Int -> T
Nil :: T

unCon :: T -> Int
unCon (Con x) = x
Type Constructors
Now let's tackle type constructors. Take the following data definition:

data T a = Con a | Nil
This creates two constructors,

Con :: a -> T a
Nil :: T a
Of course, in Haskell, type variables are implicitly universally quantified, so these are really:

Con :: ∀a. a -> T a
Nil :: ∀a. T a
And the accessor is similarly easy:

unCon :: ∀a. T a -> a
unCon (Con x) = x
Quantified types
Let's add the existential quantifier, ∃, to our original type (the first one, without the type constructor). Rather than introducing it in the type definition, which doesn't look like logic, introduce it in the constructor / accessor definitions, which do look like logic. We'll fix the data definition later to match.

Instead of Int, we will now use ∃x. t. Here, t is some kind of type expression.

Con :: (∃x. t) -> T
unCon :: T -> (∃x. t)
Based on the rules of logic (the second rule above), we can rewrite the type of Con to:

Con :: ∀x. t -> T
When we moved the existential quantifier to the outside (prenex form), it turned into a universal quantifier.

So the following are theoretically equivalent:

data T = Con (exists x. t) | Nil
data T = forall x. Con t | Nil
Except there is no syntax for exists in Haskell.

In non-intuitionistic logic, it is permissible to derive the following from the type of unCon:

unCon :: ∃ T -> t -- invalid!
The reason this is invalid is because such a transformation is not permitted in intuitionistic logic. So it is impossible to write the type for unCon without an exists keyword, and it is impossible to put the type signature in prenex form. It's hard to make a type checker guaranteed to terminate in such conditions, which is why Haskell doesn't support arbitrary existential quantifiers.

Sources
"First-class Polymorphism with Type Inference", Mark P. Jones, Proceedings of the 24th ACM SIGPLAN-SIGACT symposium on Principles of programming languages (web)
