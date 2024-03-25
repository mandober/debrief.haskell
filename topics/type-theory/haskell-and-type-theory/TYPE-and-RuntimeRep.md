# TYPE and RuntimeRep

https://hackage-search.serokell.io/viewfile/ghc-9.2.2/GHC/Builtin/Types/Prim.hs


## FunTyCon


```hs
funTyConName :: Name
funTyConName = mkPrimTyConName (fsLit "FUN") funTyConKey funTyCon
-- | The @FUN@ type constructor.
--
-- @
-- FUN :: forall (m :: Multiplicity) ->
--        forall {rep1 :: RuntimeRep} {rep2 :: RuntimeRep}.
--        TYPE rep1 -> TYPE rep2 -> *
-- @
--
-- The runtime representations quantification is left inferred. This
-- means they cannot be specified with @-XTypeApplications@.
--
-- This is a deliberate choice to allow future extensions to the
-- function arrow. To allow visible application a type synonym can be
-- defined:
--
-- @
-- type Arr :: forall (rep1 :: RuntimeRep) (rep2 :: RuntimeRep).
--             TYPE rep1 -> TYPE rep2 -> Type
-- type Arr = FUN 'Many
-- @
--
funTyCon :: TyCon
funTyCon = mkFunTyCon funTyConName tc_bndrs tc_rep_nm
  where
    -- See also unrestrictedFunTyCon
    tc_bndrs = [ mkNamedTyConBinder Required multiplicityTyVar1
               , mkNamedTyConBinder Inferred runtimeRep1TyVar
               , mkNamedTyConBinder Inferred runtimeRep2TyVar ]
               ++ mkTemplateAnonTyConBinders [ tYPE runtimeRep1Ty
                                             , tYPE runtimeRep2Ty
                                             ]
    tc_rep_nm = mkPrelTyConRepName funTyConName
```


## Kinds

Note [TYPE and RuntimeRep][]


All types that classify values have a kind of the form `TYPE rr`, where

```hs
-- Defined in ghc-prim:GHC.Types
data RuntimeRep
  = BoxedRep Levity
  | IntRep
  | FloatRep
  -- etc

data Levity = Lifted | Unlifted
rr :: RuntimeRep
TYPE :: RuntimeRep -> TYPE 'LiftedRep  -- Built in

-- So for example:
Int        :: TYPE ('BoxedRep 'Lifted)
Array# Int :: TYPE ('BoxedRep 'Unlifted)
Int#       :: TYPE 'IntRep
Float#     :: TYPE 'FloatRep
Maybe      :: TYPE ('BoxedRep 'Lifted) -> TYPE ('BoxedRep 'Lifted)
(# , #)    :: TYPE r1 -> TYPE r2 -> TYPE (TupleRep [r1, r2])
```


We abbreviate `*` specially:

```hs
type LiftedRep = 'BoxedRep 'Lifted
type * = TYPE LiftedRep
```

The `rr` parameter tells us how the value is represented at runtime.

Generally speaking, you can't be polymorphic in `rr`, e.g

```hs
f :: forall (rr:RuntimeRep) (a:TYPE rr). a -> [a]
f = \(rr:RuntimeRep) (a:rr) \(a:rr). …
```

This is no good: we could not generate code for `f` because the calling convention for `f` varies depending on whether the arg is an `Int`, `Int#`, or `Float#` (you could imagine generating specialized code, one for each instantiation of `rr`, but we don't do that).

Certain functions *can* be runtime-rep-polymorphic, because the code generator never has to manipulate a value of type `a :: TYPE rr`.

- Code generator never has to manipulate the return value:

```hs
error :: forall (rr:RuntimeRep) (a:TYPE rr). String -> a
```

- `unsafeCoerce#` defined in `Desugar.mkUnsafeCoercePair`: 
  always inlined to be a no-op

```hs
unsafeCoerce# :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
                 (a :: TYPE r1) (b :: TYPE r2).
                 a -> b
```

- Unboxed tuples and unboxed sums, defined in `GHC.Builtin.Types`
  Always inlined, and hence specialised to the call site

```hs
(#,#) :: forall (r1 :: RuntimeRep) (r2 :: RuntimeRep)
         (a :: TYPE r1) (b :: TYPE r2).
         a -> b -> TYPE ('TupleRep '[r1, r2])
```


## PrimRep and kindPrimRep

Note [PrimRep and kindPrimRep][]

As part of its source code, in GHC.Core.TyCon, GHC has

```hs
data PrimRep = BoxedRep Levity | IntRep | FloatRep | …
```

Notice that
- `RuntimeRep` is part of the syntax tree of the program being compiled;
  defined in a library: ghc-prim:GHC.Types
- `PrimRep` is part of GHC's source code; defined in GHC.Core.TyCon

We need to get from one to the other - that is what `kindPrimRep` does.

Suppose we have a value

```hs
-- Suppose we have a value
(v :: t) where (t :: k)

-- Given this kind
k = TyConApp "TYPE" [rep]
```

GHC needs to be able to figure out how `v` is represented at runtime. 
It expects `rep` to be form

```hs
TyConApp rr_dc args
```

where `rr_dc` is a promoteed data constructor from `RuntimeRep`. 
So now we need to go from `dc` to the corresponding `PrimRep`. 
We store this `PrimRep` in the promoted data constructor itself, 
see `TyCon.promDcRepInfo`.


```hs
tYPETyCon :: TyCon
tYPETyConName :: Name
tYPETyCon = mkKindTyCon tYPETyConName
            (mkTemplateAnonTyConBinders [runtimeRepTy])
            liftedTypeKind
            [Nominal]
            (mkPrelTyConRepName tYPETyConName)
```


>…and now their names

If you edit these, you may need to update the GHC formalism.

* See Note [GHC Formalism][] in GHC.Core.Lint

```hs
tYPETyConName = mkPrimTyConName (fsLit "TYPE") tYPETyConKey tYPETyCon


mkPrimTyConName :: FastString -> Unique -> TyCon -> Name
mkPrimTyConName = mkPrimTcName BuiltInSyntax

-- | All of the super kinds and kinds are defined in Prim,
-- and use BuiltInSyntax, because they are never in scope in the source
mkPrimTcName :: BuiltInSyntax -> FastString -> Unique -> TyCon -> Name
mkPrimTcName built_in_syntax occ key tycon
  = mkWiredInName gHC_PRIM (mkTcOccFS occ) key (mkATyCon tycon) built_in_syntax


-- | Given a Multiplicity, applies FUN to it.
functionWithMultiplicity :: Type -> Type
functionWithMultiplicity mul = TyConApp funTyCon [mul]
```



## `State#` TyCon

Note: [The State# TyCon][]

`State#` is the primitive, unlifted type of states.

It has one type parameter, thus `State# RealWorld` or `State# s` where `s` is a type variable. The only purpose of the type parameter is to keep different state threads separate. It is represented by nothing at all.

The type parameter to `State#` is intended to keep separate threads separate. Even though this parameter is not used in the definition of `State#`, it is given role 'Nominal' to enforce its intended use.

`RealWorld` is deeply magical. It is *primitive*, but it is not *unlifted* (hence `ptrArg`). We never manipulate values of type `RealWorld`; it's only used in the type system, to parameterise `State#`.
