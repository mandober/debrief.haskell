# Bidirectional type-checking

* Bidirectional Type Checking, @Compose NYC 2019, D. R. Christiansen, 2019
https://www.youtube.com/watch?v=utyBNDj7s2w

* Bidirectional Typing Rules: A Tutorial, David Raymond Christiansen, 2013
https://davidchristiansen.dk/tutorials/bidirectional.pdf


## Abstract

When implementing a type checker, we must answer 2 questions:
- *how to compare types for sameness*, be it a subsumption check for subtyping, a unification algorithm for type inference, or normalization for dependent types
- *when to check for sameness*

The way most type systems are written provides little guidance. One solution to the problem is *bidirectional type checking*, in which the typing judgment is split into two modes:
- mode that checks an expression against the given type
- mode that infers (reconstructs) a type for an expression

Bidirectional type checking requires a relatively low amount of user annotations, it scales to powerful type systems, and does a good job associating type errors with source locations.

We discuss the history of bidirectional type checking, show how to bidirectionalize known type systems, and walk through some implementations.

## Introduction

A type system for a programming language is not the same thing as an efficient algorithm to check that a term inhabits some type, or a means of synthesizing a type for some term.

For example, the typing rules for *ML-style let-polymorphism* are quite different from the *union-find algorithm* that can efficiently infer types for practical programs. Indeed, we may not even be able to translate typing rules into an algorithm straightforwardly, by treating each rule as a recursive function where premises are called to check the conclusion.

Only systems that are *syntax-directed* can be straightforwardly translated in this way, and many actual PLs are not described in a syntax-directed manner, as this is not always the easiest kind of system to understand.

>A *syntax-directed system* is one in which each typing judgment has a unique derivation, determined entirely by the syntactic structure of the term in question.

Syntax-directed type systems allow the typing rules to be considered as pattern-match cases in a recursive function over the term being checked.

Bidirectional typing rules are one technique for making typing rules syntax-directed (and therefore providing an algorithm for typechecking) while maintaining a close correspondence to the original presentation. 

The technique has a number of advantages: the resulting system is quite understandable and produces good error messages, relatively few type annotations are required by programmers, and the technique seems to scale well when new features are added to the language. In particular, bidirectional systems support a style of type annotation where an entire term is annotated once at the top level, rather than sprinkling annotations throughout.

## Bidirectional Rules

There are 2 primary ways in which a typing rule might not be syntax-directed: 
- it might be ambiguous in the conclusion, requiring an implementation to guess which rule to apply
- it might contain a premise that is not determined by the conclusion

We present a version of the simply-typed λ-calculus that does not have explicit type annotations on variable binders, which makes the system not syntax directed. Then, we demonstrate how to convert it to a bidirectional type system.

### 1.1 Simply-typed lambda calculus with Booleans

For an example, we translate the STλC with Booleans to the bidirectional presentation.

First, the traditional presentation:

```hs
Terms, t := x, y, z                   -- Variables
          | t t                       -- Application
          | λ x . t                   -- Abstraction
          | true                      -- Boolean constant
          | false                     -- Boolean constant
          | if t then t else t        -- Conditional expressions

Types, τ := Bool                      -- Boolean type
          | τ → τ                     -- Function type


Typing rules:


(x : τ) ∈ Γ
------------- (T-Var)
Γ ⊢ x : τ


Γ, x : τ₁ ⊢ t : τ₂
---------------------- (T-Abs)
Γ ⊢ λx.t : τ₁ → τ₂


Γ ⊢ t₁ : τ₁ → τ₂     Γ ⊢ t₂ : τ₁
--------------------------------- (T-App)
        Γ ⊢ t₁ t₂ : τ₂


------------------ (T-True)
Γ ⊢ true : Bool

------------------ (T-False)
Γ ⊢ false : Bool


Γ ⊢ t₁ : Bool    Γ ⊢ t₂ : τ    Γ ⊢ t₃ : τ
--------------------------------------- (T-If)
     Γ ⊢ if t₁ then t₂ else t₃ : τ
```

If we attempt to read the above rules algorithmically, we consider the context `Γ` in each conclusion, and the term being checked, as arguments to a function whose result is either failure or the appropriate type.

In pseudocode, for instance, *T-If* might look like this:

```hs
inferType ctx (If t1 t2 t3) = 
  case (inferType ctx t1, inferType ctx t2, inferType ctx t3) of
    (Just BoolT, Just ty2, Just ty3) ->
      if ty2 == ty3 then Just ty2 else Nothing
    _ -> Nothing
```

However, we'd run into problems when translating *T-Abs*:

```hs
inferType ctx (Lam x t) =
  case inferType ((x, ???) : ctx) t of
    Just ty2 -> Just (Fun ??? ty2)
    Nothing  -> Nothing
```

What type should go in `???`? The rule T-Abs requires we invent a type `τ₁`. This is not convenient for mechanization.

In other words, we can check (`findType`) the types of vars and applications, but we run into a problem with abstractions. An abstraction like `λx.b` is not annotated so we don't know the type of `x`. A HM-based type system could figure out the type, but here we don't assume we have facilities like unification at our disposal.

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
  Abs x b -> let t2 = findType (extend env x _0) body
             in arr _0 t2
```


We need to find the type of the formal param, and like the typing rule says, we do this by extending the evironment (var-type pairs) with the variable `x`, except we don't have its type...An abstraction should have the type `t1 → t2` and we only know the type `t2`.

This problem can be fixed by demanding that the users specify the type of all formal parameters. But then we must also augment the data type definition by adding a new data ctor for *annotated variables*, `Ann Typ String`. They are similar to unannotated variables, `Var String`, except they also have a type attached. Moreover, abstractions will now use the new annotated variables for formal params because this is the whole point - to have annotated formal params.

So now we can just read off the type of the formal param when checking the type of functions. The hole `_0` above can be filled with a type (`t1`) that was attached to the function's parameter. We also need to record it in the evironment before we look for the function's return type.

```hs
-- ... (Var and App are the same as before)
  Abs x t1 b -> let t2 = findType (extend env x t1) body
                in arr t1 t2
```

However, having to annotate the types is a burdon, so we look for better solutions.

A bidirectional type system is not the only way to fix this. The simplest way would be to annotate every λ-expression with the type of the argument, but placing annotations all over can be a noisy burdon. Another solution is given by the *Algorithm W* (Damas and Milner, 1982) that can infer the principle, most general, type. Bidirectional type checking is useful when we want a straightforward means of checking something closer to a surface syntax of a real PL.



### 1.2 Bidirectional STLC with Booleans

Bidirectional checking splits the typing judgment `Γ ⊢ t : τ` into 2 judgments:
- **inference judgment** `Γ ⊢ t ⇒ τ`, sometimes `Γ ⊢ t ⇑ τ`, 
  read as: the type of `t` can be inferred as τ in the context Γ
- **checking judgment**  `Γ ⊢ t ⇐ τ`, sometimes `Γ ⊢ t ⇓ τ`, 
  read as: `t` can be checked to have the type τ in the context Γ
