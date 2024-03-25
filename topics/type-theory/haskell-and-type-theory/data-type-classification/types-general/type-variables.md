# Type variables

Type variables, wrt binding quantifiers:
- universal type variables
- existential type variables

Type variables, wrt roles:
- nominal role
- representational role
- phantom role

Type variables, wrt declaration occurrences:
- appears on both sides   (balanced, normal, universal)
- appears on the RHS only (existential)
- appears on the LHS only (phantom)

Multipart type declarations:
- 1-part (one-liner)
- 2-part: kind-type
- 3-part: role-kind-type


* Balanced type params
Normally, a type var appears on both sides of a type declaration: first on the LHS, which is its *declaration occurrence*, after which it can appear one or more times on the RHS, which are its *application occurrence*.

```hs
data Void
data One = One
data Many a = Many a
data Tree a = Nil | Leaf a | Node a (Tree a) (Tree a) ≅
data Tree a = Nil | Leaf a | Node (a, Tree a, Tree a)
data Fix f = Fix (f (Fix f))
```

* Existential type params
An existential type var appears only on the RHS of a type declaration. However, it does get declared, using the `forall` keyword, except with *GADT* declaration that doesn't require it.

```hs
-- ADT
data Cont a = Cont (forall r. (a -> r) -> r)

-- GADT
data Cont a where Cont :: (a -> r) -> r
```

* Phantomas
A phantom type var appears only on the LHS of a type declaration. Thus, it only gets declared but is never actually used. However, this allows for interesting type-related stunts (type coloring or indexing).

```hs
-- split declaration:
type Proxy :: forall {k}. k -> *
data Proxy t = Proxy

-- one-liner declaration:
data Foxy (t :: forall k. k -> *) = Foxy

-- 3-part declaration (role/kind/type) of a type with 2 phantom TPs:
type role Trip phantom phantom
type Trip :: forall k k₁. k -> k₁ -> *
data Trip s z = MkTrip String

-- type with 1 nominal, 2 phantom TPs:
type role Trip representational phantom phantom -- each TP has a role
type Trip :: forall k k₁. * -> k -> k₁ -> *
data Trip a s z = MkTrip a

-- type with 2 phantom, 1 nominal TPs:
type role Trip phantom phantom representational -- flipped order of TPs
type Trip :: forall k k₁. k -> k₁ -> * -> *
data Trip s z a = MkTrip a
```
