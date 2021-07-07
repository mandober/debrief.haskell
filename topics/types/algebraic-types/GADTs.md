# GADTs

**Generalized algebraic datatypes** (GADTs), are an extension to Haskell's type system, enabled via *GADTs* pragma, that allow explicit type signatures to be written for data constructors.

The canonical example of a GADT is a type safe syntax tree. For example, we can declare a small language with integers, booleans, addition, logical negation, and if statements.

```hs
{-# LANGUAGE GADTs #-}

data Expr a where                                       -- (1)
    LitInt  :: Int  -> Expr Int                         -- (2)
    LitBool :: Bool -> Expr Bool
    Add     :: Expr Int  -> Expr Int -> Expr Int
    Not     :: Expr Bool -> Expr Bool
    If      :: Expr Bool -> Expr a -> Expr a -> Expr a  -- (3)
```

The `where` at (1) is what turns on GADT syntax for the rest of the data declaration. Each of `LitInt`, `LitBool`, `Add`, etc. corresponds to a data ctor of `Expr`. These constructors all take some number of args before resulting in an `Expr`. For example, `LitInt` at (2) takes an `Int` before returning a `Expr Int`. On the other hand, the data ctor `If` at (3) takes 3 args (one `Expr Bool` and two `Expr a`) and returns an `Expr a`.

It is this ability to specify the return type that is of particular interest. `Expr` is now correct by construction; we cannot build poorly-typed `Expr`. We have reflected the typing rules of `Expr` in the type system of Haskell - for example, we're unable to build an AST which attempts to add an `Expr Int` to a `Expr Bool`.

Because GADTs allow us to specify a data constructor's type explicitly, we can use them to constrain a type variable, which is not possible otherwise. Haskell can then use the knowledge of these constrained types.

We can now write a typesafe evaluator over Expr:

```hs
evalExpr :: Expr a -> a
evalExpr (LitInt i)  = i                         -- (1)
evalExpr (LitBool b) = b                         -- (2)
evalExpr (Add x y)   = evalExpr x + evalExpr y
evalExpr (Not x)     = not $ evalExpr x
evalExpr (If b x y)  = if evalExpr b then evalExpr x else evalExpr y

-- In just this amount of code, we have a fully functioning interpreter:
evalExpr . If (LitBool False) (LitInt 3) . Add (LitInt 5) $ (LitInt 10)
```

Attention! At (1), `evalExpr` returns an `Int`, but at (2) it returns a `Bool`! This is possible because Haskell can reason about GADTs. In the LitInt case, the only way such a pattern could have matched is if `a ∼ Int`, in which case it's certainly okay to return an Int. The same line of reasoning goes for the Bool and other patterns. Haskell can use information from inside a pattern match to drive type inference.

## Type equalities

The *GADTs* pragma also enables type equalities. In fact, GADTs are merely syntactic sugar over type equalities. We could have declared `Expr` as a regular Haskell datatype but using type equalities as contexts for each ctor.

```hs
data Exp a
    = (a ~ Int)  => LInt Int
    | (a ~ Bool) => LBool Bool
    | (a ~ Int)  => LAdd (Exp Int) (Exp Int)
    | (a ~ Bool) => LNot (Exp Bool)
    | Cond (Exp Bool) (Exp a) (Exp a)
```

Viewed like this, it's easier to see what is happening behind the scenes: each data constructor of `Expr` carries with itself a type equality constraint. Like any constraint inside a data ctor, Haskell will require the constraint to be proven when the data ctor is called.

As such, when we pattern match on a data ctor which contains a constraint, this satisfied constraint comes back into scope. That is, a function of type 
`Expr a -> a` can return an `Int` when pattern matching on `LitInt`, but return a `Bool` when matching on `LitBool`.

The type equality constraining `a` *only comes back into scope after pattern matching on the data ctor that contains it*. This technique enables us to pack constraints inside data ctors.
