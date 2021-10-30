---
downloaded:       2021-09-20
author:           December 17th, 201819 min read
page-url:         https://serokell.io/blog/how-dependent-haskell-can-improve-industry-projects
page-title:       How Dependent Haskell Can Improve Industry Projects
article-title:    How Dependent Haskell Can Improve Industry Projects
article-length:   28329
article-created:  {Date-Creation-yyyymmdd}
article-modified: {Date-Revision-yyyymmdd}
desc:             In this post, we talk about Dependent Haskell and show how it could be used to simplify and improve the code in a large production codebase.
---
# How Dependent Haskell Can Improve Industry Projects

In this post, we talk about Dependent Haskell and show how it could be used to simplify and improve the code in a large production codebase.
Dependent types are a hot topic in the Haskell community. Many voices advocate for adding dependent types to Haskell, and a lot of effort is being invested towards that end. At the same time, the sceptics raise various concerns, one being that dependent types are more of a research project than a tool applicable in industrial software development.

That is, however, a false dichotomy. While dependent types are still a subject of active research, we already have a good enough understanding of them to see how to apply them in real-world situations and reap immediate benefits.

In this post, we show that Dependent Haskell can be used to simplify and improve the code in a large production codebase. The subject of our case study is [Morley][1]: an implementation of the smart contract language of the [Tezos blockchain][2].

## 

Getting started with Dependent Haskell

Dependent types are a broad concept. If you take a close look at the existing dependently typed languages, such as [Agda][3], [Coq][4], and [Idris][5], you will find that their type systems all have certain unique characteristics. In that regard, Haskell is also about to get its own flavour of dependent types, designed to fit as well as possible with the rest of the language, rather than blindly imitate prior work.

Most of the theory has been developed in Adam Gundry’s [“Type Inference, Haskell and Dependent Types”][6] and then Richard Eisenberg’s [“Dependent Types in Haskell: Theory and Practice”][7]. The reader is encouraged to get acquainted with these bodies of work, especially the latter (since it’s more recent).

As a more casual reading, there is the [GHC GitLab Wiki Page][8] on the topic and [GHC Proposal #378][9]. While not as extensive, these resources provide a solid starting point for learning about the proposed design of Dependent Haskell.

Finally, for the sake of completeness, we shall now recap the most important concepts here: quantifiers, dependence, visibility, and erasure.

### 

Quantifiers and their attributes

When we say “quantifier”, we mean the part of a type that corresponds to a place in a term where you could, if you wanted, introduce a variable (source: [comment 775422563][10]). As usual, the definition alone is rather hard to grasp, so let’s make things concrete with a couple of examples.

Consider the lambda abstraction `\a -> a == 0`. The corresponding type is `Int -> Bool`. We can split both of these as follows:

__Binder__

__Body__

__Type__

`Int ->`

`Bool`

__Term__

`\a ->`

`a == 0`

The part of the type corresponding to the term-level binder is `Int -> ...`, so that’s the quantifier.

Note that the lambda abstraction may be obscured by other language features, such as pattern matching. For example:

```
f :: Int -> Bool
f 0 = True
f _ = False
```

Even though there’s no explicit lambda abstraction or variable here, we still call the `Int -> ...` part of the type a quantifier. For this discussion, we look at terms through the lens of their desugaring. The above code snippet is really equivalent to:

```
f :: Int -> Bool
f = \a -> case a of
  0 -> True
  _ -> False
```

And now the lambda abstraction is explicit. Read [our article][11] about Haskell desugaring to learn more about this transformation.

Another example of a quantifier is `ctx => ...`, which introduces a class constraint. If you are not familiar with __[dictionary passing][12]__, then it might not be apparent how constraints are related to variables and quantification. And what makes it tricky is that we can’t do a transformation to make this apparent in surface Haskell, we need to look at the internal language of GHC called Core. Fortunately, this is also covered by the aforementioned article about desugaring.

In short, a constrained function of type `Num a => a -> a` is equivalent to a function of type `NumDict a -> a -> a`, where `NumDict` is a record containing implementations of numeric methods such as `(+)`, `(*)`, `negate`, and so on.

That’s also where the concept of visibility comes into play. We call `a ->` a __visible__ quantifier because at the term level, both abstraction and application are explicit. On the other hand, `ctx => ...` is an __invisible__ quantifier, as the term-level class method dictionaries are hidden from the programmer.

In Haskell, as specified by the [Haskell 2010 Report][13], these are the only two quantifiers. However, with the `ExplicitForAll` extension, we get another one: `forall a. ...` . Just as the double arrow, `forall` is an invisible quantifier, so it may be hard to see why it’s a quantifier at all (and it is also covered by the article on desugaring). However, with the `TypeApplications` extension, you can override the visibility at use sites. Instead of writing `map (>0)`, one can write `map @Int @Bool (>0)`. The three inputs correspond to the first three quantifiers in the type of map.

```
map ::
  forall a.     
  forall b.     
  (a -> b) ->   
  ([a] -> [b])
```

This leaves us with three quantifiers in today’s Haskell, which are either visible or invisible:

`a -> ...`

visible

`ctx => ...`

invisible

`forall a. ...`

invisible (but can be used visibly with `TypeApplications`)

Visibility, however, is not the only attribute we care about. Another one is __erasure__. We call a quantifier retained (as opposed to erased) if it is possible to pattern match on the variable it introduces.

For example, the following code is *not* valid:

```
evil_id :: forall a. a -> a
evil_id x =
  case a of   
    Int -> 42
    _ -> x
```

In the above code snippet, the idea is that `evil_id` would behave mostly as `id`, but return `42` when applied to an `Int`. That is not possible, though, as the type variable `a` is not available for case analysis. We, therefore, call `forall` an __erased__ quantifier. Since erased arguments cannot be subjected to case analysis, they never affect which code branch is taken.

Erased arguments are not passed at runtime.

Also, since class method dictionaries are passed at runtime, we say that `ctx => ...` is a retained quantifier. Naturally, the data contained in the dictionary is available for case analysis.

For example, `evil_id` can be implemented by utilising the `Typeable` class:

```
import Type.Reflection
import Data.Type.Equality

evil_id :: forall a. Typeable a => a -> a
evil_id x =
  case testEquality (typeRep @a) (typeRep @Int) of
    Just Refl -> 42
    Nothing -> x
```

Finally, let us discuss __dependence__. A quantifier is considered dependent if the variable it introduces can be mentioned in the rest of the type.

For example, ordinary functions are not dependent:

```
f :: Bool -> ...  
f = \x  -> ...
```

On the other hand, `forall` is a dependent quantifier:

```
f :: forall x. ... 
f = ...
```

This means that the value taken by a dependent variable can affect the rest of the type. The type of `(+) @Int` is `Int -> Int -> Int`, whereas the type of `(+) @Double` is `Double -> Double -> Double`.

Let us conclude this subsection with a summary of the quantifiers available today and their attributes:

__Quantifier__

__Visible__

__Erased__

__Dependent__

`a -> ...`

✔️

❌

❌

`ctx => ...`

❌

❌

❌

`forall a. ...`

❌

✔️

✔️

### 

New quantifiers of Dependent Haskell

You may notice that the quantifier table has quite a few missing rows. What about visible erased dependent quantification, or invisible retained dependent quantification, and so on?

The main focus of Dependent Haskell is adding the most powerful form of quantification that would be simultaneously retained and dependent. We shall call the new quantifier `foreach`. Visibility is not that important, so the plan is to offer both the visible and the invisible variation. And while we’re at it, we might as well throw visible erased dependent quantification into the mix.

__Quantifier__

__Visible__

__Erased__

__Dependent__

`forall a -> ...`

✔️

✔️

✔️

`foreach a. ...`

❌

❌

✔️

`foreach a -> ...`

✔️

❌

✔️

The new quantifiers would provide a more principled replacement to some current practices, including `Proxy`, `Typeable`, `TypeRep`, `Sing`, and `SingI`. That is precisely what we are about to explore since Tezos Morley happens to make use of these definitions.

Specifically, we will perform the following (mostly mechanical) transformations:

__Before__

__After__

`forall a. Sing a -> b`

`foreach a -> b`

`forall a. SingI a => b`

`foreach a. b`

`forall a. Proxy a -> b`

`forall a -> b`

As a result, the code shall become more laconic and easier to maintain. We will no longer require the `singletons` package, which defines `Sing` and `SingI`, since we use all these new quantifiers instead of intricate machinery from `singletons`.

## 

How dependent types can help industry

In this section, we discuss several examples of how one can simplify industrial code that makes use of advanced types by means of Dependent Haskell. Programmers already simulate dependent types (e.g., using singletons) in their projects for miscellaneous purposes. We show what such projects might look like if we can get rid of those simulacrums in favour of real dependent types.

Our case study is [Morley][14], which is a part of [Tezos][15].

__Tezos__ is a blockchain system with a proof-of-stake consensus algorithm. [Michelson][16] is a functional smart contract language for the Tezos blockchain. It’s a stack-based language with strong typing, and it is inspired by such functional languages as ML and Scheme. There’s also a formal description of the Michelson operational semantics available at [https://tezos.gitlab.io/alpha/michelson][17].

__Morley__ is a set of tools for writing Michelson smart contracts. The word ‘Morley’ is a bit overloaded since it refers to [the Haskell package][18], [the smart contract language][19] that extends Michelson, and [the same-named framework][20]. The package consists of the Morley interpreter implementation and the type checker.

### 

Getting rid of `singletons`

`singletons` is a library that emulates dependent types in Haskell. You can learn more about it from [its README][21] and the paper that introduced the library: [“Dependently Typed Programming with Singletons”][22] by Richard Eisenberg and Stephanie Weirich.

We assume that constructions such as the `Sing` type family and the `SingI` class are already known to the reader. Otherwise, one may have a glance at [the documentation][23].

#### 

Example 1: `T` and `getWTP`

Let us have a look at [the data type `T`][24] from `morley`:

```
data T =
    TKey
  | TUnit | TSignature | TChainId | TOption T | TList T | TSet T | TOperation
  | TContract T | TPair T T | TOr T T | TLambda T T | TMap T T | TBigMap T T
  | TInt | TNat | TString | TBytes | TMutez | TBool | TKeyHash | TBls12381Fr
  | TBls12381G1 | TBls12381G2 | TTimestamp | TAddress | TNever
```

This is a regular ADT that describes the types of Michelson values. If we wanted to verify that a type is well-formed, we could implement a predicate:

```
isWellFormed :: T -> Bool
```

However, using a mere `Bool` means we don’t have any evidence that validation succeeded. Instead, `morley` defines the `getWTP` of the following type:

```
getWTP :: forall (t :: T). (SingI t) => Either NotWellTyped (Dict (WellTyped t))
```

If the input type `t :: T` is well-formed, the function produces `Right` with the evidence. Otherwise, it fails and returns `Left`. Notably, the type of evidence `WellTyped t` refers to the value of `t`. That is why we had to employ the elaborate construction `forall t. SingI t =>` instead of adding a simple function parameter `T ->`.

Quite a few complications arise from this. Firstly, we need to generate singletons for `T`:

```
$(let singPrefix, sPrefix :: Name -> Name
      singPrefix nm = mkName ("Sing" ++ nameBase nm)
      sPrefix nm = mkName ("S" ++ nameBase nm) in

  withOptions defaultOptions{singledDataConName = sPrefix, singledDataTypeName = singPrefix} $
  concat <$> sequence [genSingletons [''T], singDecideInstance ''T]
```

This snippet of Template Haskell generates `SingT`, which is the singleton type for `T`, and also `SingI` and `SDecide` instances:

```
data SingT t where
  STUnit :: SingT TUnit
  STSignature :: SingT TSignature
  ...
  STPair :: SingT t1 -> SingT t2 -> SingT (TPair t1 t2)
  ...
  
```

Secondly, the implementation of `getWTP` now has to work with `SingT` values instead of plain `T` values.

Let’s have a look at one of the branches of `getWTP` that handles `STPair`:

```
getWTP :: forall t. (SingI t) => Either NotWellTyped (Dict (WellTyped t))
getWTP = case sing @t of
...
 STPair s1 s2 ->
    withSingI s1 $
    withSingI s2 $
    fromEDict (getWTP_ s1) $
    fromEDict (getWTP_ s2) $ Right Dict
…

getWTP_ :: forall t. Sing t -> Either NotWellTyped (Dict (WellTyped t))
getWTP_ s = withSingI s $ getWTP @t
```

`TPair` is a constructor of `T` that corresponds to the Michelson tuple data type, and `STPair a b` is the corresponding singleton. This function has the `SingI` constraint, and here we pattern match on `sing @t`, where `t` is a type variable of kind `T`.

With Dependent Haskell, we are getting the `foreach` quantifier, which could be used to simplify all the above. The type of `getWTP` would become:

```
getWTP :: foreach (t :: T). Either NotWellTyped (Dict (WellTyped t))
```

And the implementation of `getWTP` could pattern match on regular `T` values rather than `SingT`:

```
getWTP @(TPair s1 s2) =
    fromEDict (getWTP @s1) $
    fromEDict (getWTP @s2) $ Right Dict
```

Moreover, we no longer need the `getWTP_` helper function, which was used for recursive calls. Instead, we simply use a visibility override `@`.

#### 

Example 2: `Peano` and `UpdateN`

Here’s another example. In `morley` we need both term-level and type-level natural numbers to index the elements on the stack of the stack machine.

At the moment, we have the classic data type that defines natural numbers inductively à la Peano:

```
data Peano = Z | S Peano
```

And with the `DataKinds` language extension, we can use it at the type level, as we do in the type of instructions for the stack machine:

```
data Instr (inp :: [T]) (out :: [T]) where
…
  UPDATEN
    :: forall (ix :: Peano) (val :: T) (pair :: T) (s :: [T]).
       ConstraintUpdateN ix pair
    => PeanoNatural ix
    -> Instr (val : pair : s) (UpdateN ix val pair ': s)
...
```

But in addition to the type-level variable `ix :: Peano`, we also need to mirror it at the term level to pattern match on it. That is the purpose of the `PeanoNatural ix` field.

Typically, one would use a singleton type for this purpose:

```
data SingNat (n :: Nat) where
  SZ :: SingNat 'Z
  SS :: !(SingNat n) -> SingNat ('S n)
```

But we take it one step further. There are plenty of situations when we need to convert this singleton value to a natural number represented as non-inductive `Natural`.

The straightforward solution is to utilise a conversion function like the following one:

```
toPeano :: SingNat n -> Natural
toPeano SZ = 0
toPeano (SS n) = 1 + (toPeano n)
```

Such a conversion is O(n)O(n) at runtime, and it would be inefficient to invoke it repeatedly. Instead of such a conversion, we define the `PeanoNatural` data type that caches the result of such conversion next to the singleton.

```
data PeanoNatural (n :: Peano) = PN !(SingNat n) !Natural
```

Of course, we don’t want to make an element of `PeanoNatural` from an arbitrary pair of `SingNat n` and `Natural`. We would like to have an invariant that might be formulated as `PN s k :: PeanoNatural n` iff `k = toPeano s`. We formalise this idea by introducing pattern synonyms `Zero` and `Succ`.

```
data MatchPS n where
  PS_Match :: PeanoNatural n -> MatchPS ('S n)
  PS_Mismatch :: MatchPS n

matchPS :: PeanoNatural n -> MatchPS n
matchPS (PN (SS m) k) = PS_Match (PN m (k - 1))
matchPS _ = PS_Mismatch

pattern Zero :: () => (n ~ 'Z) => PeanoNatural n
pattern Zero = PN SZ 0

pattern Succ :: () => (n ~ 'S m) => PeanoNatural m -> PeanoNatural n
pattern Succ s <- (matchPS -> PS_Match s) where
  Succ (PN n k) = PN (SS n) (k+1)
{-# COMPLETE Zero, Succ #-}
```

Those patterns cover all possible cases, but GHC can’t figure it out on its own, so we have to use the `COMPLETE` pragma to avoid the `incomplete-patterns` warnings.

With Dependent Haskell, we could rewrite `PeanoNatural` to avoid the use of singletons:

```
data PeanoNatural (n :: Peano) where
  PN :: foreach !(n :: Peano) -> !Natural -> PeanoNatural n
```

Interestingly, this reveals a need for strict `foreach` – a topic not previously discussed in the literature, so it’s worth investigating separately.

Back to the `UPDATEN` constructor of `Instr`:

```
  UPDATEN
    :: forall (ix :: Peano) (val :: T) (pair :: T) (s :: [T]).
       ConstraintUpdateN ix pair
    => PeanoNatural ix
    -> Instr (val : pair : s) (UpdateN ix val pair ': s)
```

`UPDATEN` is the instruction for the stack machine to update the n-th node of a given right-combed pair on top of the stack.

Here, `UpdateN` is a type-level list operation:

```
type family UpdateN (ix :: Peano) (val :: T) (pair :: T) :: T where
  UpdateN 'Z           val _                   = val
  UpdateN ('S 'Z)      val ('TPair _  right)   = 'TPair val right
  UpdateN ('S ('S n))  val ('TPair left right) = 'TPair left (UpdateN n val right)
```

In Dependent Haskell, we can redefine `UpdateN` as a term-level function:

```
updateN :: Peano -> T -> T -> T
updateN Z         val  _                 = val
updateN (S Z)     val (TPair _  right)   = TPair val right
updateN (S (S n)) val (TPair left right) = TPair left (updateN n val right)

```

### 

Getting rid of `Proxy`

In Morley, we use phantom labels to identify arithmetic and other algebraic operations:

```
data Add           
data Sub           
data Mul           
data And           
data Or            
data Xor           
...
```

Then to implement these operations, we have a class called `ArithOp`:

```
class (Typeable n, Typeable m) =>
      ArithOp aop (n :: T) (m :: T) where
  type ArithRes aop n m :: T
  evalOp
    :: proxy aop
    -> Value' instr n
    -> Value' instr m
    -> Either (ArithError (Value' instr n) (Value' instr m)) (Value' instr (ArithRes aop n m))
```

The `aop` type variable stands for one of the aforementioned operations. The `n` and `m` type variables stand for the input types of the operation, and a single operation can be overloaded to work on various inputs.

```
instance ArithOp 'Add TInt TInt   
instance ArithOp 'Add TNat TNat   
...

instance ArithOp 'And TBool TBool   
instance ArithOp 'And TNat TNat     
```

The `ArithRes` type family specifies the type of the result:

```
instance ArithOp 'Add TInt TInt where
  type ArithRes 'Add TInt TInt = TInt
  ...


instance ArithOp 'Add TInt TTimestamp where
  type ArithRes 'Add TInt TTimestamp = TTimestamp
  ...
```

Finally, we have the `evalOp` method which actually implements the operation at the term level:

```
instance ArithOp Or 'TNat 'TNat where
  type ArithRes Or 'TNat 'TNat = 'TNat
  evalOp _ (VNat i) (VNat j) = Right $ VNat (i .|. j)
```

You will notice that `evalOp` ignores its first argument, which is a proxy value. Its only role is to specify the operation at the use site.

```
let k = evalOp (Proxy :: Proxy Add) n m
```

The problem with `Proxy` is that it’s a value passed at runtime, so it incurs a certain amount of overhead. The optimiser can’t always get rid of it. Another problem is that constructing it at use sites introduces syntactic noise and makes the API less convenient. We would rather write `evalOp Add` than `evalOp (Proxy :: Proxy Add)`.

What if we simply removed it? Like so:

```
class ... => ArithOp aop (n :: T) (m :: T) where
  ...
  evalOp   
    :: Value' instr n
    -> Value' instr m
    -> Either (ArithError (Value' instr n) (Value' instr m))
              (Value' instr (ArithRes aop n m))
```

Then we would solve both problems: no input to pass at runtime, and at use sites we could simply write `evalOp @Add`. But the cost is that we’d introduce a new problem: the `aop` type variables would become ambiguous. That is permitted if the `AllowAmbiguousTypes` extension is enabled, but it leads to major deterioration of error messages if one forgets to specify the ambiguous type variable at the use site.

One of the quantifiers of Dependent Haskell offers a better solution. The visible `forall a ->` quantifier is mostly equivalent to a regular `forall a.`, but the type variable must be always specified at use sites and is never ambiguous.

The type we want for `evalOp` is:

```
evalOp
    :: forall instr n m. forall aop ->
       ArithOp aop n m
    => Value' instr n
    -> Value' instr m
    -> Either (ArithError (Value' instr n) (Value' instr m))
              (Value' instr (ArithRes aop n m))
```

For variables we want the compiler to infer at use sites, we use the ordinary quantifier `forall instr n m.`; but for `aop`, which must be specified explicitly at the use site, we use `forall aop ->`.

We could apply the same trick to other functions which involve this variable. For example, in today’s Morley there’s a wrapper around `evalOp` that operates on values from the stack of the stack machine.

```
runArithOp
  :: (ArithOp aop n m, EvalM monad)
  => proxy aop
  -> StkEl n
  -> StkEl m
  -> monad (StkEl (ArithRes aop n m))
runArithOp op l r = case evalOp op (seValue l) (seValue r) of
  Left  err -> throwError (MichelsonArithError err)
  Right res -> pure $ starNotesStkEl res
```

With the visible forall, we would rewrite it as follows:

```
runArithOp
  :: forall aop ->
     (ArithOp aop n m, EvalM monad)
  => StkEl n
  -> StkEl m
  -> monad (StkEl (ArithRes aop n m))
runArithOp op l r = case evalOp op (seValue l) (seValue r) of
  Left  err -> throwError (MichelsonArithError err)
  Right res -> pure $ starNotesStkEl res
```

The `runArithOp` function evaluates arithmetic operations and either succeeds or fails. The first argument is `proxy aop`, which specifies the arithmetic operation itself. The function uses `evalOp`, a method of the type class `ArithOp` with the associated type `ArithRes` that represents the resulting type of operation.

This change cascades downstream to other functions that make use of `runArithOp`. For example, the `runInstrImpl` function has the following equation:

```
type InstrRunner m = forall inp out. Instr inp out -> Rec StkEl inp -> m (Rec StkEl out)

runInstrImpl :: EvalM m => InstrRunner m -> InstrRunner m
…
runInstrImpl _ OR (l :& r :& rest)     = (:& rest) <$> runArithOp (Proxy @Or) l r
```

Instead of `Proxy @Or`, we would simply write `Or` here:

```
runInstrImpl _ OR (l :& r :& rest)     = (:& rest) <$> runArithOp Or l r
```

### 

Term-level functions instead of type families

#### 

Example 1: `Drop` and `Take`

In Dependent Haskell, we will be able to use functions at the type level. In particular, we can get rid of type families in favour of usual term-level functions.

For example, we may replace the following type families with the corresponding functions on lists:

```
type family Drop (n :: Peano) (s :: [k]) :: [k] where
  Drop  'Z s = s
  Drop ('S _) '[] = '[]
  Drop ('S n) (_ ': s) = Drop n s

type family Take (n :: Peano) (s :: [k]) :: [k] where
  Take  'Z _ = '[]
  Take _ '[] = '[]
  Take ('S n) (a ': s) = a ': Take n s
```

These type families are from `morley` as well. We replace `Drop` and `Take` with their usual term-level counterparts:

```
drop :: Peano -> [a] -> [a]
drop Z l = l
drop _ [] = []
drop (S n) (_ : s) = drop n s

take :: Peano -> [a] -> [a]
take Z _ = []
take _ [] = []
take (S n) (x : xs) = x : take n s
```

In addition to operations on lists, we use type families to enforce invariants:

```
type family IsLongerThan (l :: [k]) (a :: Peano) :: Bool where
  IsLongerThan (_ ': _) 'Z = 'True
  IsLongerThan (_ ': xs) ('S a) = IsLongerThan xs a
  IsLongerThan '[] _ = 'False

type LongerThan l a = IsLongerThan l a ~ 'True
```

The `IsLongerThan` is a binary predicate that is true iff the length of a list is greater than a given natural number.

One may reformulate this piece of code in Dependent Haskell as follows:

```
isLongerThan :: [a] -> Peano -> Bool
isLongerThan xs n = length xs > n


longerThan l n = isLongerThan l n ~ True
```

#### 

Example 2: `IsoValue` and `ToT`

Now we have a look at the `IsoValue` type class. This type class defines a mapping from Haskell types to Michelson ones using an associate type:

```
class (WellTypedToT a) => IsoValue a where
  
  type ToT a :: T
  type ToT a = GValueType (G.Rep a)
```

In Dependent Haskell, we can replace associated types with methods (assuming [#267][25] to control visibility):

```
class (WellTypedToT a) => IsoValue a where
  toT a :: T
```

We can further translate type families that make use of `ToT`. For example, currently, we have `ToTs` that applies `ToT` to a list of types:

```
type family ToTs (ts :: [Type]) :: [T] where
  ToTs '[] = '[]
  ToTs (x ': xs) = ToT x ': ToTs xs
```

With DH, one could simply write `map toT`.

#### 

Example 3: `DUPN`

Now let us consider an example directly related to the Morley stack machine.

Recall that `Instr` is the data type that represents the stack machine instructions, such as `UPDATEN` or `DUPN`:

```
data Instr (inp :: [T]) (out :: [T]) where
...
  DUPN
    :: forall (n :: Peano) inp out a. (ConstraintDUPN n inp out a)
    => PeanoNatural n -> Instr inp out
...
```

As with `UPDATEN` discussed in an earlier section, we rewrite `DUPN` to use `foreach` instead of `PeanoNatural`:

```
data Instr (inp :: [T]) (out :: [T]) where
...
  DUPN
    :: foreach  (n :: Peano) 
    -> forall inp out a. (ConstraintDUPN n inp out a)
    => Instr inp out
...
```

We have already discussed this transformation, and now we are interested in something else: the `ConstraintDUPN` constraint.

```
type ConstraintDUPN n inp out a = ConstraintDUPN' T n inp out a

type ConstraintDUPN' kind (n :: Peano) (inp :: [kind]) (out :: [kind]) (a :: kind) =
  ( RequireLongerOrSameLength inp n
  , n > 'Z ~ 'True
  , inp ~ (Take (Decrement n) inp ++ (a ': Drop n inp))
  , out ~ (a ': inp)
  )
```

Let’s focus on this line in particular:

```
 inp ~ (Take (Decrement n) inp ++ (a ': Drop n inp))
```

There are four type families involved here: `Take`, `Drop`, `Decrement`, and `++`. We already discussed `Take` and `Drop`.

`Decrement` is defined as follows:

```
type family Decrement (a :: Peano) :: Peano where
  Decrement 'Z = TypeError ('Text "Expected n > 0")
  Decrement ('S n) = n
```

Once again, in Dependent Haskell, we don’t need to replicate arithmetic operations at the type level as type families. So we replace the capital ‘D’ with the lowercase one in `Decrement`.

```
decrement :: Peano -> Peano
decrement Z = error "Expected n > 0"
decrement (S n) = n
```

The call to `TypeError` can be translated to the familiar term-level `error`. As to `Take` and `Drop`, we have demonstrated their translation above. As to `++`, currently we use one from the `vinyl` library.

```
type family (as :: [k]) ++ (bs :: [k]) :: [k] where
  '[] ++ bs = bs
  (a ': as) ++ bs = a ': (as ++ bs)
```

In Dependent Haskell, we could use the term-level one from `base`. So, we redefine `ConstraintDUPN'` in the following way:

```
type ConstraintDUPN' kind (n :: Peano) (inp :: [kind]) (out :: [kind]) (a :: kind) =
  ( RequireLongerOrSameLength inp n
  , n > 'Z ~ 'True
  , inp ~ (take (decrement n) inp ++ (a ': drop n inp))
  , out ~ (a ': inp)
  )
```

## 

Future of Dependent Haskell

We hope this article explains how these changes in the language will allow writing a more transparent Haskell code. In particular, these changes are of interest in those situations when we would like to guarantee safety at the type level.

Let us quickly discuss further steps for Dependent Haskell. At the time of writing, the proposal with [design for dependent types][26] has been recently accepted. That’s quite a remarkable achievement since this topic was rather controversial amongst the Haskell community. But we still have plenty to do to ‘materialise’ these enhancements that look fairly speculative since full-fledged dependent types in Haskell are not ready yet.

Right now, we are working on such issues as enabling [visible `forall`s][27] and [binding type variables in functions][28]. These are examples of issues the solution of which makes dependent types in Haskell a bit closer. However, introducing foreach quantifiers requires extensive research since we have only a design sketch at the moment.

### 

How to participate?

The GHC developers community is always open for new enthusiasts, so some readers of this post might want to participate in Dependent Haskell development.

If so, you may have a look at [ghc.dev][29]. This page contains basic commands for building and debugging GHC. See also [the GHC chapter][30] by Simon Marlow and Simon Peyton Jones in ‘The Architecture of Open Source Applications’. We also recommend overviewing [the GHC list of issues for newcomers][31].

Feel free to contact the authors – [Vladislav Zavialov][32] or [Danya][33] – on Twitter if any of this sounds interesting to you.

[1]: https://gitlab.com/morley-framework/morley
[2]: https://tezos.com/
[3]: https://agda.readthedocs.io/en/v2.6.0.1/getting-started/what-is-agda.html
[4]: https://coq.inria.fr/
[5]: https://www.idris-lang.org/
[6]: https://adam.gundry.co.uk/pub/thesis/thesis-2013-12-03.pdf
[7]: https://www.seas.upenn.edu/~sweirich/papers/eisenberg-thesis.pdf
[8]: https://gitlab.haskell.org/ghc/ghc/-/wikis/dependent-haskell
[9]: https://github.com/ghc-proposals/ghc-proposals/pull/378
[10]: https://github.com/ghc-proposals/ghc-proposals/pull/378#issuecomment-775422563
[11]: https://serokell.io/blog/haskell-to-core
[12]: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.53.3952&rep=rep1&type=pdf
[13]: https://www.haskell.org/onlinereport/haskell2010/
[14]: https://gitlab.com/morley-framework/morley
[15]: https://tezos.com/
[16]: https://www.michelson.org/
[17]: https://tezos.gitlab.io/alpha/michelson
[18]: https://hackage.haskell.org/package/morley
[19]: https://gitlab.com/morley-framework/morley/-/blob/master/code/morley/docs/language/morleyLanguage.md
[20]: https://gitlab.com/morley-framework
[21]: https://github.com/goldfirere/singletons/blob/master/README.md
[22]: https://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf
[23]: https://hackage.haskell.org/package/singletons-3.0/docs/Data-Singletons.html
[24]: https://gitlab.com/morley-framework/morley/-/blob/master/code/morley/src/Michelson/Typed/T.hs
[25]: https://github.com/ghc-proposals/ghc-proposals/pull/267
[26]: https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0378-dependent-type-design.rst
[27]: https://github.com/int-index/ghc-proposals/blob/visible-forall/proposals/0000-visible-forall.rst
[28]: https://github.com/goldfirere/ghc-proposals/blob/type-abstractions/proposals/0050-type-lambda.rst
[29]: https://ghc.dev/
[30]: https://www.aosabook.org/en/ghc.html
[31]: https://gitlab.haskell.org/ghc/ghc/-/issues?label_name%5B%5D=newcomer
[32]: https://twitter.com/int_index
[33]: https://twitter.com/p_morphism
