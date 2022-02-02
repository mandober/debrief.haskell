# Roles
(Thinking with types)

The *type system* ensures terms are used correctly, the *kind system* ensures types are logical, and *the role system ensures coercions are safe*.

Every type parameter of a type ctor is assigned a role. *Roles* describe how a type's representational equality is related to its type params' coercion-safety.

There are 3 roles:

* **nominal**: regular *type-equality* corresponding to the `a ∼ b` constraint.

If a type parameter has a nominal role, then the two types that differ must not actually differ at all: **they must be identical** (after type family reduction)

* **representational**: two types `a` and `b` are *representationally equal* iff it is safe to reinterpret the memory of an `a` as a `b`.

If a type parameter has a representational role, then the two types must have the same representation. (If `T`'s first parameter's role is representational, then `T Age Bool c` and `T Int Bool c` would have the same representation, because `Age` and `Int` have the same representation).

* **phantom**: two types are always *phantom-equal* to one another.

If a type parameter has a phantom role, then we need no further information.



In the newtype `Sum a`, we say that `a` has *representational* role, which means that if `Coercible a b => Coercible (Sum a) (Sum b)`, then `Sum a` and `Sum b` are representationally equal whenever `a` and `b` are.

This is also the case for `v` in `Map k v`. However, `Coercible k1 k2` does not imply `Coercible (Map k₁ v) (Map k₂ v)`, and this is because `k` must have *nominal* role. `Coercible (Map k₁ v) (Map k₂ v)` is only the case when `k₁ ~ k₂`, and so this nominal role on `k` is what keeps `Map` safe.

The *phantom* role is reserved for phantom type parameters. `Proxy`, for example, has a phantom type variable, `data Proxy a = Proxy`. The type param `a` is at role *phantom*, and as expected, `Coercible (Proxy a) (Proxy b)` is always true. Since `a` doesn't actually ever exist at runtime, it is safe to change it whenever we like.

There is an inherent ordering in roles: phantom types can be coerced in more situations than representational types, which themselves can be coerced more often than nominal types.

Upgrading from a weaker role (usable in more situations) to a stronger one is known as *role strengthening*.

The inference process is relatively simple:
1. All type params are initially assumed to be at role *phantom*.
2. The function type constructor (->) has two representational roles:
  - any type param applied to it gets upgraded to *representational*
  - data ctors count as applying (->)
3. The type ctor (`~`) has two nominal roles:
  - any type param applied to it gets upgraded to *nominal*
  - *GADTs* count as applying (~)
  - *Type families* count as applying (~)


```hs
-- What is the role signature of `Either a b`?
data Either a b = Left a | Right b
-- data ctors count as applying (->)
type role Either representational representational

-- What is the role signature of Proxy a?
data Proxy a = Proxy
-- initially, all type params are phantom
type role Proxy phantom

-- but...
type Proxy :: forall {k}. k -> *
data Proxy @{k} t = Proxy
type role Proxy nominal phantom
```

While it is logical that a GADT counts as an application of (~), it might be less clear *why types used by type families must be at role nominal*.

Consider a type family that replaces Int with Bool, but otherwise leaves its arg alone.

```hs
type family IntToBool a where
    IntToBool Int = Bool
    IntToBool a = a
```

It is not safe to say `a` is at role representational because `Coercible a b => Coercible (IntToBool a) (IntToBool b)` doesn't hold in general. In particular, it fails whenever `a ~ Int`.

As a result, any type that a *type family* can potentially match on must be given *role nominal*.

While roles are automatically inferred via the compiler, it is possible to strengthen an inferred role to a less permissive one by providing a *role signature*.

For example, BSTs, like Maps, have an implicit memory dependency on their `Ord` instance. Given a data-type:

```hs
{-# LANGUAGE RoleAnnotations #-}

data BST v = Empty | Branch (BST v) v (BST v)

type role BST nominal
```

We can provide a role signature to strengthen the inferred (representational) role to nominal.

The syntax for role annotations is: `type role TyCon role1 role2...` 
where roles are given for type vars in the same order they are defined.


## 6.4.20. Roles

https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/roles.html

The goal of the roles system is to track when two types have the same underlying representation.

## Role inference

GHC performs role inference to determine the correct role for every parameter.

Role inference starts with a few base facts:
- the function type ctor (->) has *2 representational* type params
- the type equality (~) has *2 nominal* type params
- all type families' type params are *nominal*
- all GADT-like type params are *nominal*

Then, these facts are propagated to all places where these types are used.

- the default role for datatypes and synonyms is *phantom*
- the default role for classes is *nominal*

Thus, for datatypes and synonyms, any TPs unused in the RHS (or used only in other types in phantom positions) will be *phantom*.

Whenever a TP is used in a representational position 
(as a type arg to a ctor whose corresponding TP is at role representational), 
we upgrade its role from phantom to representational.

Similarly, when a TP is used in a nominal position, 
its role is upgraded to nominal.

We never downgrade a role 
from nominal to phantom or representational, or 
from representational to phantom.

In this way, we infer the *most-general role for each TP*.
