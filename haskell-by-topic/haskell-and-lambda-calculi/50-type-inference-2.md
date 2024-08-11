# Type inference and constraints
https://cs3110.github.io/textbook/chapters/interp/inference.html

## Summary

- Type checking relation:  Γ |- e : t
- Type inference relation: Γ |- e : t -| C
- Type inference
  - base types:
    - int:  Γ |- i : int  -| {}
    - bool: Γ |- b : bool -| {}
  - names:  Γ |- n : Γ(n) -| {}
  - if-exp: Γ |- if e1 then e2 else e3 : 't -| C1, C2, C3, C
  - abs:    Γ |- λx.e : 't1 -> t2 -| C
  - app:    Γ |- e1 e2 : 't -| C1, C2, C
- Each type of inference propages the accumulated constraints.
- Infrence of constants, names and Abs does not generate any new constraints.
- `If-exp` and `App` generate new constraints that are unionized with the existing ones and propagated.

## Constraints

Type constraint is an equation t1 = t2 between any two types.

Type variables are prefixed with a tick, meta-type variables are not, they may end up being a type var or a base types.

A static typing environment is denoted by `Γ` or `env`. 
It is a set of _(tvar, type)_ pairs.

For example
- 'a = int
- 'a -> 'b = int -> bool -> int, so 'a = int and 'b = bool -> int

## Inference

Type inference relation extends the type checking relation, `Γ ⊢ e : t`, to

      Γ |- e : t -| C

where `C` are type constraints as a set of equations on types.

As an algorithm, inference takes a static typing env `Γ` (or `env`) and an exp `e`, and produces a type of that exp as `t`, along with a set of constraints `C`. Then we have to slove the set of constraints to reconstruct the type fully.

### Inference of term constants
https://www.youtube.com/watch?v=NkAt9eApGSw&list=PLre5AT9JnKShBOPeuiD9b-I4XROIJhkIU&index=192

- Γ |- i : int  -| {}
- Γ |- b : bool -| {}

For example
- {} |- 1 : int -| {}
- {} |- true : bool -| {}

>Inference of term constants
>does not generate new constraints
>only propagates the existing constraints.

### Inference of names

In the static typing env `Γ`, a name `n` has whatever type the env has it has.

      Γ |- n : Γ(n) -| {}

To find a name in the env, we use a lookup function on sets. If the name is not found, an error is returned.

For example
- {x: int} |- x : int -| {}
- {} |- x is an error: "x unbound"

To type-check a builtin name, like (+), we look up its signature in the *initial static type environment*.

- { (+) : int -> int -> int } |- (+) : int -> int -> int -| {}

The initial static type environment (Δ) possibly contains
- (+) : int -> int -> int
- (*) : int -> int -> int
- (<=) : int -> int -> bool

>Inference of names
>does not generate new constraints
>only propagates the existing constraints.


### Inference of IF

```ocaml hs
-- THE RULE FOR IF
Γ |- if e1 then e2 else e3 : 't -| C1, C2, C3, C
  if fresh 't
  and Γ |- e1 :: t1 -| C1
  and Γ |- e2 :: t2 -| C2
  and Γ |- e3 :: t3 -| C3
  and C = { t1 = bool, t2 = 't, t3 = 't }
```

1. The whole exp `(if e1 then e2 else e3)` is assigned a fresh tvar `'t`.
2. A fresh meta-tvar is assigned to each of 3 subexps:
  `e1 : t1`, `e2 : t2` and `e3 : t3`.
3. if-exp invariants are specified in the set of constraints `C`:
  - the guard exp `e1 : t1` must have the `bool` type, `t2 = bool`.
  - the types of the two branches must be the same, `t2 = t3`.
This means the set of constraints `C` will be the same every time: 
`C={t2 = bool, t2 = t3}`.

- We have reached the bottom, we now ascend up, accumulating the sets of constraints.

(...)

- The constraint `C` will be unionized with other 3 constraints (comma represents set union) `C1`, `C2`, `C3`, i.e. the overall constraint set will be 
`{ C1 ∪ C2 ∪ C3 ∪ C }`.

#### Example: IF inference

Infer the type of the exp: `if true then 0 else 1`. 
(we repeat the IF rule for convenience)

```ocaml hs
------------------------------------------------------
-- THE RULE FOR IF
------------------------------------------------------
Γ |- if e1 then e2 else e3 : 't -| C1, C2, C3, C
  if fresh 't
  and Γ |- e1 :: t1 -| ,C1
  and Γ |- e2 :: t2 -| ,C2
  and Γ |- e3 :: t3 -| ,C3
  and C = { t1 = bool, t2 = 't, t3 = 't }

---- type the exp: -----------------------------------
xmpl1 = if true then 0 else 1
------------------------------------------------------
1 Γ |- (if true then 0 else 1) : 'a -| ⌛
---- descend -----------------------------------------
²   Γ |- true :: bool -| ∅
³   Γ |-    0 :: int  -| ∅
⁴   Γ |-    1 :: int  -| ∅
⁵   C = { bool = bool, int = 'a, int = 'a }
⁶   C = { int = 'a } -- simplifying
---- ascend ------------------------------------------
⁷ Γ |- (if true then 0 else 1) : 'a -| { int = 'a }
    Γ |- true :: bool -| ∅
    Γ |-    0 :: int  -| ∅
    Γ |-    1 :: int  -| ∅
    C = { int = 'a }
```


- (1) the overall IF exp is assigned the type `'t` (fresh type var)
- we don't write the constraint sets now, this will be done on ascend
- we indent and proceed to type the 3 subexp
- (2) the guard exp `e1` is the constant `true`, so already a bool. This generates no constraints (`C1` is ∅).
- (3) the exp `e2` is the constant `0`, known to be `int` type. This generates no constraints (`C2` is ∅).
- (4) the exp `e3` is the constant `1`, known to be `int` type. This generates no constraints (`C2` is ∅).
- (5) The rule for the constraint set `C` says that `t1 = bool`, i.e. that `t1` must be `bool`.

In the current exp, the guard exp is already a bool, so `bool = bool` is the resulting (quasi) constraint. 

This constraint is obtained by referring to the 1st constraint in `C` in the rule for IF, which says that `t1 = bool`, or more elaborately `e1:(t1 = bool)`.

Whatever type we infer for `e1`, say `x`, we put that type in place of `t1` to get the constraint 'x = bool'. Here, that type is instead inferred as `bool` because `e1` is a constant term `true` (e1 = true). And we can *immediately* infer its type as `bool`. So `bool` replaces `t1` (and not some unknown or more complex type) and we get `bool = bool` (rather then 'x = bool', which would also generate some extra constraints, instead of C1 being ∅).

~~For `t2` and `t3` (which must be the same type), the constraint will equalize both to the type `'a` which we assigned to the overall exp, thus `t2 = 'a` and `t3 = 'a`. So the invariant constraint set `C` is `{bool = bool, t2 = 'a, t3 = 'a}`, which is really just `C = {t2 = 'a, t3 = 'a}`.~~

```
              fixed in stone
                      ↓
RULE-IF: C = { t1  = bool,  t2 = 't,  t3 = 't }
                ↓                 ↓         ↓
CURR-C: C = { bool = bool,  t2 = 'a,  t3 = 'a }
                ↓                 ↓         ↓
CURR-C: C = { bool = bool, int = 'a, int = 'a }
```

On ascent (7), becasue this is a very simple IF exp, nothing intereseting happens except that the constraint set `C` is propagated up. Since the 3 other sets are empty, `C` becomes the only overall set of constraints. As we go up, only the initial line is changed: it gets the set of constraint part on the far right, `-| { int = 'a }`.


Inference of IF type can and should be optimized to avoid unnecessary computations and calculations of constraints. On the other hand, we canot just declare that the overall type is the type of the first branch we infer successfully - we also need to check for consistency.



>Inference of if-exp generates new constraints
>which are unionized with the existing constraints and propagated.


### Inference of abstractions

>Γ |- λx.e : 't1 -> t2 -| C

The input type will be a tvar `'t1`, the return type is `t2` that might not be a tvar but a base type. So `t2` is a meta-tvar.

We generate a fresh tvar name `'t1` as the type of the formal param `x`. Next, we infer the type of the body `e`, assigning it a meta tvar `t2` in the typing env `Γ` extended by `x : 't1`.

```ocaml hs
Γ |- λx.e : 't1 -> t2 -| C
  if fresh 't1
  and Γ, x : 't1 |- e : t2 -| C

-- which should be suggestive of the E-Abs typing rule:
Γ, x : 't1 |- e : t2
---------------------- E-Abs
Γ |- λx.e : 't1 -> t2

-- Forming a lambda abs is really the opposite of abstraction - instead of parameterizing an exp 'M' by turning one of its values into a param 'x' (abstracting 'x' from M), we are synthisizing an abs from parts. We take a (free) var 'x' (of type t1) and join it with an arbitrary exp 'M' (of type t2) to assemble a lambda abs. The var 'x' becomes the binder that binds all the free x's in M (it literally captures the free vars 'x' in there, which we'd otherwise go to great length to avoid). And sometimes it happens that M doesn't even contain a single x.
```



For example, 
infer the type of exp `λx. if x then 1 else 0`

```ocaml hs
{} |- λx. if x then 1 else 0 : 'a → 'b -| {'a = bool, 'b = int}
  {x : 'a} |- if x then 1 else 0 : 'b -| {'a = bool, 'b = int}
    {x : 'a} |- x : 'a  -| {}
    {x : 'a} |- 1 : int -| {}
    {x : 'a} |- 0 : int -| {}
    C = {'a = bool, 'b = int, 'b = int}
```

Later, when we learn how to solve the constraints, this exp will obviously have the final type `λx. if x then 1 else 0 : bool → int`.

>Inference of abstraction
>does not generate new constraints
>only propagates the existing constraints.


### Inference of application

>Γ |- e1 e2 : 't -| C1, C2, C

We generate a new tvar name `'t` as the type of the application. Next, we infer the type of `e1` as `t1` and `e2` as `t2`, where t1 and t2 are mata-tvars, each with its own set of constraints.

```ocaml hs
Γ |- e1 e2 : 't -| C1, C2, C
  if fresh 't
  and Γ |- e1 : t1 -| C1
  and Γ |- e2 : t2 -| C2
  -- generate a fn constraint: e1 is a fn so its type (e1 : t1) is:
  -- from the type of arg (e2 : t2) to the type of App ('t),
  -- thus: t1 = t2 -> 't
  and C = {t1 = t2 → 't}
```

For example, 
infer the type of the exp `(+) 1`

`Δ` is the initial static environment with types for all builtin names:
```hs ml
Δ = { (+) : int → int → int
    , (*) : int → int → int
    , (≤) : int → int → bool
    }
```

The exp is a partial application of (+) to arg 1. We assign it the type `'a`.

```hs ml
Δ |- (+) 1 : 'a -| { int → int → int = int → 'a }
  Δ |- (+) : int → int → int -| {}
  Δ |- 1 : int -| {}
  C = { int → int → int = int → 'a }
```

When we solve the constraints later, we'll see that the type of this exp is `int → int`.

>Inference of application generates new constraints
>which are unionized with the existing constraints and propagated.

## Unification

- Substitution unifies a set of equations.
- A substitution may be a sequence S1, … Sₙ of smalled individual substitution carried out in order.
- A substitution `S` unifies a constraint, `t1 = t2`, if `S t1 = S t2`, i.e. if the substitution applied to both types makes the types equal.
- A substitution `S` unifies a set of constraints `C` if it unifies all the constraints in the set.

Unifications
- no new substitution or constraint
  - `'x = 'x`
  - `int = int`
  - `bool = bool`
- function type:
  `(t1 → t2) = (t3 → t4)` add 2 new constraints 
  `t1 = t3` and `t2 = t4`
- `'x = t`, where `'x ∉ FV(t)`
  - replace all `'x` by `t` in the constraint set, 
    thus eliminating `'x` from the system of equations.
  - append substitution `{'x := t}` to the solution set.
- any other case: error

It turns out this algorithm, called the *Robinson's algorithm*, is optimal since it guarantees that its output is the **most general unifier** (mgu). That is, any other unifier has more specific substitutions.

For example, given the constraint `{'a = 'b → 'b}`, the Robinson's algorithm would produce the substitution `{'a := 'b → 'b}`, not more specific substitutions like `{'a := int → int}; {'b := int}`.

>Formally, if `S = unify C`
>and `S1` also unifies `C`
>then `S1 = S ∘ S2`, for some `S2`.

Procedure
- start with exp `e` and init env `Δ`
- collect constraints `C` and type `t` with inference relation   
   `Δ |- e : t -| C`
- run inification to solve `C`, obtaining a substitution `S`
- apply solution of `C` to `t`
- inferred type is `S t` (apply subst `S` to type `t`), also denoted `t S`

Example:
- `λf. λx. f (x + 1)`, i.e. `λf. λx. f ((+) x 1)`

```hs ml
Δ |- λf. λx. f ((+) x 1) : 'a → 'b → 'e -| C₅
  -- assign fresh tvar to f
  Δ, f : 'a |- λx. f ((+) x 1) : 'b → 'e -| C₄
    -- assign fresh tvar to x
    Δ, f : 'a, x : 'b |- f ((+) x 1) : 'e -| C₃
      -- body is App of f to some arg
      -- f is already assigned a type 'a in Γ
      -- Γ is the current type env, not the init type env Δ
      Δ, f : 'a, x : 'b |- f : 'a -| {}
      -- the arg to f is App so descend into it
      Δ, f : 'a, x : 'b |- (((+) x) 1) : 'd -| {} = C₂
        -- App ((+) x) is assigned fresh type 'c
        -- When we finish descending, we figure out C₁
        Δ, f : 'a, x : 'b |- (+) x : 'c -| C₁
          -- (+) is a fn whose type we read off from Δ
          Δ, f : 'a, x : 'b |- (+) : int → int → int -| {}
          -- we look up the type of x in Γ
          Δ, f : 'a, x : 'b |- x : 'b -| {}
          -- We now generate the constraint set C₁
          -- for the parent node. See (🍹) for details.
        Δ, f : 'a, x : 'b |- 1 : int -| {}


C₁ = { int → int → int = 'b → 'c }
C₂ = { 'c = int → 'd } ∪ C₁
C₃ = { 'a = 'd → 'e } ∪ C₂
C₄ = C₃
C₅ = C₄
```

In more details (🍹)

```hs
-- We have App
e1 e2 : 't
  -- so we generate a fresh type for App, (e1 e2 : 't)
  -- and fresh types for e1 and e2
  e1 : t1
  e2 : t2
  -- but e1 is a fn, so its type is
  -- from the type of arg (e2 : t2) to the type of App ('t)
  -- thus: t1 = t2 -> 't

-- We have App
(+) x : 't
  -- so we generate a fresh type for App ('t)
  -- and fresh types for e1 and e2
  e1 : t1
  e2 : t2
  -- but e1 is (+), so its type is
  -- from the type of arg (e2 : t2) to the type of App ('t)
  -- thus: t1 = t2 -> 't
  --
  -- But the type of e1 is known, (+) : int → int → int, thus:
  -- t1 === (+) === int → int → int === t2 -> 't
    t1 = t2 -> 't
    t1 = (+) = int → int → int
  -- and we know the arg type to be 'b, thus:
    t1 = 'b -> 't
  -- and we have App type assigned as 'c
    t1 = 'b -> 'c
  -- thus:
    (+) = 'b -> 'c
  -- and C is the set
    { int → int → int = 'b -> 'c }
```

Finally, the generated set of constraints is:

    C = { int → int → int = 'b → 'c
        , 'c = int → 'd
        , 'a = 'd → 'e
        }

which needs to be solved to get the proper type. Solving means generating the set of substitutions that when applied will enacuate these type equalities.

```hs
'a = 'd → 'e                (1)
'c = int → 'd               (2)
int → int → int = 'b → 'c   (3)
----------------------------------------------
S₁ = { 'a := 'd → 'e }
-- but since 'a does not occur anywhere
-- else we eliminate the equation (1)
-- (but we'll propageate the substitution!)
----------------------------------------------
'c = int → 'd
int → int → int = 'b → 'c
----------------------------------------------
S₂ = { 'c := int → 'd }
-- this time 'c does occur in (3)
-- so applying this subst to (3) we get
int → int → int = 'b → int → 'd
----------------------------------------------
-- and we're left with one equation
int → int → int = 'b → int → 'd
-- break it down
int → (int → int) = 'b → (int → 'd)
S₃ = { 'b := int }
-- now we are left with
(int → int) = (int → 'd)
S₄ = { 'd := int }
-- and that's it
----------------------------------------------
-- Collect the substitutions
S₁ = { 'a := 'd → 'e }
S₂ = { 'c := int → 'd }
S₃ = { 'b := int }
S₄ = { 'd := int }
```

Inferring the program in different order could have produced a different but compatible set of substitutions.

Now that we have the solution set of substitution we apply it to the inferred type of the expression, which was `'a → 'b → 'e`.

- e₁          = 'a         → 'b  → 'e
- e₂ = S₁(e₁) = ('d → 'e)  → 'b  → 'e
- e₃ = S₂(e₂) = ('d → 'e)  → 'b  → 'e
- e₄ = S₃(e₃) = ('d → 'e)  → int → 'e
- e₅ = S₄(e₄) = (int → 'e) → int → 'e

Finally, the inferred type is    
`(int → 'e) → int → 'e`.

The last, "makeover", step could be to lower the tvars towards to beggining of the alphabet, thus obtaining the nicer-looking type:   
`(int → 'a) → int → 'a`.
