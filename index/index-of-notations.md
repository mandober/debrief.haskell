# Index of notations and notational conventions

## MT

Monads, monad precursors, monad transformers, monadic classes

Ωμϵγα : runΩμϵγα : ΩμϵγαT : runΩμϵγαT : MonadΩμϵγα

* Reader
  - precursor   : Reader
  - trandformer : ReaderT
  - accessor    : runReaderT
  - type class  : MonadReader



## Type names

Notational convention used in Haskell Language 2010 Report

Description         | Symbol  | Type
--------------------|---------|--------
kind                | k k₁ k₂ | Kind
type-ctor           | tc      | Tycon
type-var, tvar      | v       | Tyvar
tvar-fixed          | f       | Tyvar
tvar-generic        | g       | Tyvar
type                | t       | Type
class               | c       | Class
instance            | it      | Inst
predicate           | p, q    | Pred
predicate-deferred  | d       | Pred
predicate-retained  | r       | Pred
qualified-type      | qt      | QualType
class-environment   | ce      | ClassEnv
scheme              | sc      | Scheme
substitution        | s       | Subst
unifier             | u       | Subst
assumption          | a       | Assump
identifier          | i       | Id
literal             | l       | Literal
pattern             | pat     | Pat
expression          | e, f    | Expr
alternative         | alt     | Alt
binding-group       | bg      | BindGroup


## Others

kind            Kind      k
kind-star       KType     Type
kind-function   KFun      ∀k. k -> Type

data Type
  TVar Tyvar        type-var
       Tyvar = Tyvar Id Kind
  TCon Tycon        type-ctor
  TAp Type Type     type-appl
