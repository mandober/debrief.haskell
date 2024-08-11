# Bidirectional type-checking

* Bidirectional Type Checking, @Compose NYC 2019, D. R. Christiansen, 2019
https://www.youtube.com/watch?v=utyBNDj7s2w


When implementing a type checker, we can easily check the types of `Var` and `App`, but we run into a problem with `Abs`. An abstraction like `λx.b` is not annotated, so we don't know the type of `x`. A HM-based type system could figure out the most general type for it, but here we don't assume we have facilities like unification at our disposal.

```hs
findType :: Env -> Exp -> Typ
findType env exp = case exp of
  Var x -> case lookup x env of
    Just t  -> t
    Nothing -> error "No such type"
  App f arg -> case findType env f of
    Arr t1 t2 -> if sameType t1 (findType env arg)
                 then t2
                 else error "Type mismatch"
    _ -> error "Wrong type"
  Abs x b -> let t2 = findType (extend env x ???) body -- PROBLEM 🛑
             in arr ??? t2                             -- PROBLEM 🛑
```

We need to find the type of the formal param, and like the typing rule says, we do this by extending the evironment (var-type pairs) with the variable `x`, except we don't have its type...An abstraction should have the type `t1 → t2` and we only know the type `t2`.

This problem can be fixed by demanding that the users specify the type of all formal parameters. But then we must also augment the data type definition by adding a new data ctor for *annotated variables*, `Ann Typ String`. They are similar to unannotated variables, `Var String`, except they also have a type attached. Moreover, abstractions will now use the new annotated variables for formal params because this is the whole point - to have annotated formal params.

So now we can just read off the type of the formal param when checking the type of functions. The hole `???` above can be filled with a type (`t1`) that was attached to the function's parameter. We also need to record it in the evironment before we look for the function's return type.

```hs
-- ... (Var and App are the same as before)
  Abs x t1 b -> let t2 = findType (extend env x t1) body
                in arr t1 t2
```

However, having to annotate the types is a burdon, so we look for better solutions.

## Type Checking

Getting the type by looking at the expression works in some restricted cases, but not in general.

Instead of having the type be something that we have to discover from the lambda, we are going to say that the user can tell what the type is, and we will check that the exp they gave us indeed has that type.

In other words, some types will flow from the outside toward the program and we'll represent them with a leftward facing double arrow, `=>`, instead of the colon. These types are type-checked. 

```hs
Γ, x : τ₁ ⊢ b ⇐ τ₂
------------------- (T-Abs)
Γ ⊢ λx.b ⇐ τ₁ → τ₂
```

In `Abs`, i.e. for `λx.B`, we can be checked to have the type `τ₁ → τ₂`, when we can check that `b` is has the type `τ₂`, under the assumptions we had before and the additional assumption that `x` has the type `τ₁`.


```hs
Γ ⊢ e₁ ⇐ τ₁ → τ₂     Γ ⊢ e₂ ⇐ τ₁
--------------------------------- (T-App)
        Γ ⊢ e₁ e₂ ⇐ τ₂
```

In `App`, we check that `e₁` applied to `e₂` has the type `τ₂`. We also need to check that `e₁` is a function type `τ₁ → τ₂`, and that the arg exp `e₂` has the expected type, `τ₁`.

```hs
checkType :: Env -> Exp -> Typ -> ()
checkType env exp ty = case exp of
  Var x -> varTypeIs x env ty
  App f arg -> checkType env f (Arr ??? ty) -- PROBLEM 🛑
  Abs x b -> case ty of
    Arr t1 t2 -> checkType (extend env x t1) body t2
    _         -> error "Not a function type."
```

Now we have a problem with `App`: when we check the type of the function in an application `App f a`, we get stuck. This is because we know `f` needs to be a function, but we don't know what type its argument `a` needs to have.

## Bidirectional type system

- typing judgements have type-checking and type-sythesis modes
- annotations are still required! usualy top-level signatures
- rules to change the direction between type-checking and type-sythesis modes
- selecting the direction for each piece of syntax
- bi-di type checker is syntax-directed (deterministic, forced)
- scales to advanced type features (higher-rank poly, subtyping, dep. types)
- predictable code location where the error points



Bidirectional checking splits the typing judgment `Γ ⊢ t : τ` into 2 judgments:
- **inference judgment** `Γ ⊢ t ⇒ τ`, sometimes `Γ ⊢ t ⇑ τ`, 
  read as: the type of `t` can be inferred as τ in the context Γ
- **checking judgment**  `Γ ⊢ t ⇐ τ`, sometimes `Γ ⊢ t ⇓ τ`, 
  read as: `t` can be checked to have the type τ in the context Γ


## Subsumption rule

In languages that support subtyping, there is also the subsumption rule:

```hs
Γ ⊢ e₁ : τ₁   τ₁ :< τ₂
-----------------------
      Γ ⊢ e₁ : τ₂
```

The **subsumption rule** dictates when can the type `τ₁` - which is a subtype of type `τ₂` - be used when the type `τ₂` is required.

The judgement `τ₁ :< τ₂` means that the type `τ₁` is a subtype of type `τ₂`.

In other words, type `τ₁` is more specific or more specialized then type `τ₂`. Equivalently, type `τ₂` is more general or more generic then type `τ₁`.

When some type `τ` is required, we can provide a subtype of `τ` that is more specific; e.g. when the context requests an integer, we can provide a natural, thanks to the naturals being a subtype of integers - which is something a language determines; if they were not, i.e. if nat and int were types without this subtyping relation, then we cannot provide a nat when an int is expected.

Subtyping relations
- each type is its own subtype, `τ :< τ`
- naturals are a subtype of integers, `Nat :< Int`
- if `τ₁ :< τ₂` then `List τ₁ :< List τ₂` since lists are covariant
- if `σ₁ :< τ₁` and `τ₂ :< σ₂` then `τ₁ → τ₂ :< σ₁ → σ₂` since functions are covariant in the return type and contravariant in the input type.

```hs
------- (each type is its own subtype)
τ :< τ

----------      ----------      ----------
Nat :< Nat      Nat :< Int      Int :< Int

     τ₁ :< τ₂
---------------------
List τ₁ :< List τ₂

---------------------
List Nat :< List Int


σ₁ :< τ₁     τ₂ :< σ₂
---------------------
τ₁ → τ₂ :< σ₁ → σ₂
```

The *subtyping relation* also has a lot to do with **variance**. For example, `List` is a covariant type since we can provide `List Nat` when a `List Int` is required because `Nat :< Int` and thus `List Nat :< List Int`.

Functions are profunctors: functions are 
- *covariant in the return type*, and
- *contravariant in the input type*

### Variance of function types

**Covariance of function types**: a function of type `τ -> Nat` can be provided when the function type `τ -> Int` is expected, i.e.

give `τ -> Nat` when `τ -> Int` is wanted

which implies that `τ -> Nat` is a subtype of `τ -> Int`:

`τ -> Nat` :< `τ -> Int`


```hs
σ₁ :< τ₁     τ₂ :< σ₂
--------------------- (Fn)
τ₁ → τ₂ :< σ₁ → σ₂

-- τ₁ = τ
-- τ₂ = Nat
-- σ₁ = τ
-- σ₂ = Int

τ :< τ     Nat :< Int
----------------------  (Covariance of Fn type)
τ → Nat :< τ → Int


-- τ₁ = Int
-- τ₂ = τ
-- σ₁ = Nat
-- σ₂ = τ

Nat :< Int     τ :< τ
---------------------- (Contravariance of Fn type)
Int → τ :< Nat → τ
```


**Contravariance of function types**: a function of type `Nat -> τ` can be provided when the function type `Int -> τ` is expected, i.e.

give `Nat -> τ` when `Int -> τ` is wanted

which implies `Nat -> τ` is a subtype of `Int -> τ`, even though `Nat :< Int`

`Nat -> τ` :< `Int -> τ` even though `Nat` :< `Int`.


## Syntax-directed type system

The problem with the subsumption rule is that we have no information when to use it. The other rules are syntax-directed: we scrutinize an exp to reveal its data ctor and then we know which rule to use. So if have a `Var`, we use the "T-Var" rule, if we have an `Abs` we use the "T-Abs" rule, etc. Every syntactic element has an associated rule.

**Syntax-directed**: a type system is syntax-directed if the choice of a rule to apply depends only on (the language category of) the exp being checked.

There is never guessing or backtracking. Since every term (piece of syntax) has has its own associated rule, the process of type-checking is completely deterministic and *forced* (syntax-directed).

## Bi-di type system (continued)

We have 2 typing judgements:
- `Γ ⊢ e ⇒ τ` means the exp `e` synthesizes type `τ`
- `Γ ⊢ e ⇐ τ` means the exp `e` is checked to have type `τ`

Algorithmically, we have *input* and *output* modes:
- input (synth) mode: takes 2 args (`Γ`, `e`) and returns synthesized type `τ`
- output (check) mode: takes 3 args (`Γ`, `e`, `τ`) and returns a Boolean

*Type-synthesis*, `Γ ⊢ t ⇒ τ`, takes an env and exp, producing a type. *Type-checking*, `Γ ⊢ t ⇐ τ`, takes an env, exp and type, and produces a truth value (a `Bool`), or perhaps the unit `()` to signal type inhabitation (using unit type as the ultimate "it is true" or "it exists" type).

```hs
typeSyth :: Env -> Exp -> Ty
typeCheck :: Env -> Exp -> Ty -> Bool
```

### Changing directions (modes)

```hs
Γ ⊢ e ⇐ τ    [τ : Type]
------------------------ (1)
Γ ⊢ e ∈ τ ⇒ τ̅


Γ ⊢ e ⇒ τ̅₁    τ₁ :< τ₂
------------------------ (2)
Γ ⊢ e ⇐ τ₂
```

Note: The "element of" symbol (`∈`) is used since the colon symbol is reserved. It denotes the fact that exp `e` has type `τ` as provided by th user; but it is not a judgement (for some reason or another), only an expression (type exp, annotation).

Note: The metavariable `τ` with an overline signifies an *output type*, i.e. that the type is a result of type sythesis (also inferrable from the direction of the fat arrow).

Rule (1) is "trust but verify": if the user has annotated an exp `e` with a type `τ`, we check that the exp `e` indeed has the type `τ`.

Rule (1) is a fallback rule if no other rules apply. We synthesize the type, then check that the sythesized type is a subtype of the expected type. This is the only time we check for subtyping. In type systems that do not support subtyping, but express type equivalence with *α-equivalence module computation* rules, this is where we'd compute normal forms, or where we'd insert implicit args (e.g. in Agda).

These two rules would look something like this in pseudo-Haskell code:

```hs
-- …
synth Γ (Ann e t) =
  if check Γ e t
  then t
  else error "type error"

-- ⫶
-- default case (in type-checking equations):
check Γ other t2 ->
  let t1 = synth Γ other
  in  isSubtypeOf t1 t2
```

## Bidi rules

```hs
x : τ ∈ Γ
------------------------ Var
Γ ⊢ x ⇒ τ


```

Translated to pseudo-Haskell

```hs

```
