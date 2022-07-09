# The equality types story

GHC.Builtin.Types.Prim
https://hackage-search.serokell.io/viewfile/ghc-9.2.2/GHC/Builtin/Types/Prim.hs

Often referenced note: *The equality types story*

GHC sports a veritable menagerie of equality types:

Name      | TC | L | HE/HO  | Role    | TyCon            | Module
----------|----|---|--------|---------|-----------------:|-------------------|
~#        | ty | U | hetero | nominal |      eqPrimTyCon | GHC.Prim
~~        | Cs | L | hetero | nominal |         heqTyCon | GHC.Types
~         | Cs | L | homo   | nominal |          eqTyCon | GHC.Types
:~:       | ty | L | homo   | nominal | n/b              | Data.Type.Equality
:~~:      | ty | L | hetero | nominal | n/b              | Data.Type.Equality
~R#       | ty | U | hetero | repr    |     eqReprPrimTy | GHC.Prim
Coercible | Cs | L | homo   | repr    |   coercibleTyCon | GHC.Types
Coercion  | ty | L | homo   | repr    | n/b              | Data.Type.Coercion
~P#       | ty | U | hetero | phantom | eqPhantPrimTyCon | GHC.Prim


Legend
- TC     : type or class
- L      : lifted or unlifted
- HE/HO  : heterogeneous or homogeneous
- Role   : nominal, representational, or phantom role
- TyCon  : The name of the built-in type constructor; 
  - `n/b`: not built-in (user-definable)
- Module : Source module


## Heterogeneous equality

Heterogeneous equalities: `~~`, `:~~:`, `~#`, `~R#`, `~P#`

- `~~`    heqTyCon
- `:~~:`  (propositional equality)
- `~#`    eqPrimTyCon
- `~R#`   eqReprPrimTy
- `~P#`   eqPhantPrimTyCon

Heterogeneous equality can relate types of different kinds.

That is, knowing that 
`t₁ ~#  t₂`  or 
`t₁ ~R# t₂`  or even that 
`t₁ ~P# t₂`  
also means that 
`k₁ ~#  k₂` 
where 
`t₁ :: k₁` and 
`t₂ :: k₂`.

To produce less confusion for end users, when not dumping 
and without *-fprint-equality-relations*, 
some of these symbols are printed differently:
- `~#` and `~~` are rendered as `~` in error messages
- `~R#` is rendered as `Coercible`



## `~#`

- symbols-name:       `~#`
- friendly-rendition: `~` in error messages ▣
- lang-entity:        type
- liftedness:         unlifted
- equality-flavor:    heterogeneous
- role:               nominal
- built-in:           yes
- user-definable:     no
- TyCon name:         `eqPrimTyCon` built-in type ctor
- defined in:         GHC.Prim


`~#` is __The Type Of Equality in GHC__. It classifies *nominal coercions*.

```hs
(~#) :: forall k₁ k₂. k₁ -> k₂ -> #
```

This type is used in the solver for recording equality constraints.

It responds "yes" to `Type.isEqPrimPred`, and classifies as an `EqPred` in
`Type.classifyPredType`.

All wanted constraints of this type are built with coercion holes.
See the note about [Coercion holes][] in `GHC.Core.TyCo.Rep`.

But see also the note about [Deferred errors for coercion holes][] in `GHC.Tc.Errors` to see how equality constraints are deferred.

Within GHC, `~#` is called `eqPrimTyCon`, and it is defined in `GHC.Builtin.Types.Prim`




## `~~`

```hs
(~~) :: forall k₁ k₂. k₁ -> k₂ -> Constraint
```

This is almost-ordinary class, defined as if by:

```hs
-- declaring the class itself and immediately
class    (a ~# b) => a ~~ b
-- defining the one instance of the class
instance (a ~# b) => a ~~ b
```

However, it is so unusual because
- We can't actually declare it that way because we don't have syntax for `~#`.
  `~#` isn't a constraint, so even if we could write it, it wouldn't kind-check
- Users cannot write instances of it.
- It is "naturally coherent".
  This means that the solver won't hesitate to solve a goal of type `(a ~~ b)` even if there is, say `(Int ~~ c)` in the context (normally, the solver waits to learn more, just in case "the givens" influences what happens next).
  See Note [Naturally coherent classes][] in `GHC.Tc.Solver.Interact`.
- It always terminates.
  That is, in the `UndecidableInstances` checks, we don't worry if a `~~` constraint is too big, as we know that solving equality terminates.


On the other hand, this behaves just like any class with regards to *eager superclass unpacking* in the solver. So, a lifted equality "given" quickly becomes an unlifted equality "given", which is good because the solver knows all about unlifted equalities. 

There is some special-casing in `GHC.Tc.Solver.Interact.matchClassInst` that pretends there is an instance of this class, as we can't write the instance in Haskell.

Within GHC, `~~` is called `heqTyCon`, defined in `GHC.Builtin.Types`.

## `~`

`~` is exactly like `~~`, except with a homogeneous kind.

```hs
(~) :: forall k. k -> k -> Constraint
```

It is an almost-ordinary class defined as if by:

```hs
class    (a ~# b) => (a :: k) ~ (b :: k)
instance (a ~# b) =>  a       ~  b
```

- All the bullets for `~~` apply also here
- Also, `~` is *magical syntax* because `~` is a reserved symbol; it cannot be exported or imported.

Within GHC, `~` is called `eqTyCon`, defined in `GHC.Builtin.Types`.

Historical note: prior to July 20YY, `~` was defined as a more-ordinary class with `~~` as a superclass. But that made it special in different ways; and the extra superclass selections to get from `~` to `~#` via `~~` were tiresome. Now, it is defined uniformly with `~~` and `Coercible` which is much nicer.



```hs
(:~:)  :: forall k    . k  -> k  -> Type
(:~~:) :: forall k₁ k₂. k₁ -> k₂ -> Type
```

These are perfectly ordinary GADTs, wrapping `~` and `~~`, respectively. 
They are not defined within GHC at all.

```hs
(~R#) :: forall k₁ k₂. k₁ -> k₂ -> #
```

The is the representational analogue of ~#. This is the type of *representational equalities* that the solver works on. All wanted constraints of this type are built with coercion holes.


Within GHC, `~R#` is called `eqReprPrimTyCon`, and it is defined in `GHC.Builtin.Types.Prim`.


```hs
Coercible :: forall k. k -> k -> Constraint
```

This is quite like `~~` in the way it's defined and treated within GHC, but it's homogeneous. Homogeneity helps with type inference (as GHC can solve one kind from the other) and is more intuitive for users.

An alternative design included `HCoercible` (like `~~`) and `Coercible` (like `~`). One annoyance was that we want `coerce :: Coercible a b => a -> b` 
and we need the type of coerce to be fully wired-in. 
So the `HCoercible`/`Coercible` split required that both types be fully wired-in. Instead of doing this, I just got rid of `HCoercible`, as I'm not sure who would use it, anyway. Within GHC, `Coercible` is called `coercibleTyCon`, and it is defined in `GHC.Builtin.Types`.


```hs
Coercion :: forall k. k -> k -> Type
```

This is a perfectly ordinary GADT, wrapping `Coercible`. It is not defined within GHC at all.


```hs
(~P#) :: forall k1 k2. k1 -> k2 -> #
```

This is the phantom analogue of `~#` and it is barely used at all (the solver has no idea about it).

Here is the motivation:

```hs
data Phant a = MkPhant
type role Phant phantom
Phant <Int, Bool>_P :: Phant Int ~P# Phant Bool
```

We just need to have something to put on that last line (you probably don't need to worry about it).



## Primitive equality constraints

(See Note: The equality types story)

```hs
-- | The representation type for equality predicates
eqPrimTyCon :: TyCon
eqPrimTyCon  = mkPrimTyCon eqPrimTyConName binders res_kind roles
  where
    -- Kind :: forall k1 k2. k1 -> k2 -> TYPE (Tuple '[])
    binders  = mkTemplateTyConBinders [liftedTypeKind, liftedTypeKind] id
    res_kind = unboxedTupleKind []
    roles    = [Nominal, Nominal, Nominal, Nominal]


-- | Like `eqPrimTyCon`, but the type for Representational coercions.
-- This should only ever appear as the type of a covar.
-- Its role is interpreted in `coercionRole`.
eqReprPrimTyCon :: TyCon
eqReprPrimTyCon = mkPrimTyCon eqReprPrimTyConName binders res_kind roles
  where
    -- Kind :: forall k1 k2. k1 -> k2 -> TYPE (Tuple '[])
    binders  = mkTemplateTyConBinders [liftedTypeKind, liftedTypeKind] id
    res_kind = unboxedTupleKind []
    roles    = [Nominal, Nominal, Representational, Representational]


-- | Like `eqPrimTyCon`, but the type for Phantom coercions.
-- This is only used to make higher-order equalities.
-- Nothing should ever actually have this type!
eqPhantPrimTyCon :: TyCon
eqPhantPrimTyCon = mkPrimTyCon eqPhantPrimTyConName binders res_kind roles
  where
    -- Kind :: forall k1 k2. k1 -> k2 -> TYPE (Tuple '[])
    binders  = mkTemplateTyConBinders [liftedTypeKind, liftedTypeKind] id
    res_kind = unboxedTupleKind []
    roles    = [Nominal, Nominal, Phantom, Phantom]


-- | Given a Role, what TyCon is the type of equality predicates at that role?
equalityTyCon :: Role -> TyCon
equalityTyCon Nominal          = eqPrimTyCon
equalityTyCon Representational = eqReprPrimTyCon
equalityTyCon Phantom          = eqPhantPrimTyCon
```
