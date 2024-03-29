# Type is an extensible GADT

A blog about functional programming, Posted on July 9, 2018    
https://blog.poisson.chat/posts/2018-07-09-type-gadt.html


We can define new types in Haskell, so the kind of types is clearly **extensible**. Looking closer, it turns out that it is also oddly similar to a **GADT**. [1](#fn1)

(`Type` is the [better name](https://github.com/ghc-proposals/ghc-proposals/pull/143) for `*`. It can be imported from [`Data.Kind`](https://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Kind.html).)

`Type` is an ADT
----------------

⸨ ... ⸩


Declaring a new type using the `data` or `newtype` keyword introduces a new type constructor[2](#fn2). This suggests a view of `Type` as a “type”:

    data Type
      = Int
      | Bool
      | Maybe Type
      | IO Type
    
      -- A couple of fancier types
      | Fix (Type -> Type)
      | ReaderT Type (Type -> Type) Type  -- transformers
      | HList [Type]                      -- (heterogeneous lists)
      | V Nat Type                        -- linear (fixed-length vectors)
      | Header' [Type] Symbol Type        -- servant
    
      ...  -- very open

Constructions can be destructed, this is true of both the term level and the type level. In particular, type families give us type-level pattern matching. One difference with pattern matching as we’re used to at the term level is that there is no way to handle every case individually. The upside is that clauses are defined independently. They can be in any order, or in different modules.

For example, the `UnMTL` type family below unfolds stacks of common monad transformers to equivalent but more basic types. ([gist](https://gist.github.com/Lysxia/147a593d32bf7d8d468b61b882c1b9ba))

    type family   UnMTL (a :: Type) :: Type
    type instance UnMTL (ReaderT r m a) = r -> UnMTL (m a)
    type instance UnMTL (StateT  s m a) = s -> UnMTL (m (a, s))
    type instance UnMTL (ExceptT e m a) = UnMTL (m (Either e a))
    type instance UnMTL (Identity a) = a
    type instance UnMTL (IO a) = IO a

If a type family is applied to a type it can’t handle, nothing really happens, until the type checker tries to unify it with another type for example, causing an error at compile-time. (“Stuck” types don’t unify.)

    ghci> :kind! UnMTL (StateT Int (Except String ()))
    Int -> Either String (Int, ())
    ghci> :kind! UnMTL Double  -- not a transformer stack
    UnMTL Double               -- not an error

`Type` is an existential type
-----------------------------

The main common point of `Type` with GADTs is that constructors can quantify variables that don’t appear in their result types, which are then said to be “existential types”.

Standard examples of polykinded type constructors:

    data Proxy  :: forall   k.      k -> Type  -- Data.Proxy
    data (:~:)  :: forall   k. k -> k -> Type  -- Data.Type.Equality
    data (:~~:) :: forall j k. j -> k -> Type  -- idem

(We’ll keep the `forall` implicit from now on.)

We need the `PolyKinds` extension to quantify over kinds at all. But since there’s no place for type variables in `Type`, any polykinded type constructor makes `Type` an existential type.

    data Type where
      Proxy  ::                  k -> Type
      (:~:)  ::             k -> k -> Type
      (:~~:) ::             j -> k -> Type
      Rec    :: (u -> Type) -> [u] -> Type  -- vinyl
      ...

A note about indexed types
--------------------------

GADTs are also a form of indexed types: types with parameters that may depend on the constructor.[3](#fn3) For example, singleton types are types indexed by values:

    data SBool (_ :: Bool) where
      --        ^-------------------+- indices
      STrue  :: SBool 'True   -- <--|
      SFalse :: SBool 'False  -- <--/

Each constructor of `SBool` corresponds to one constructor for the index. `SBool` is a “singleton type” because for every index `i :: Bool`, there is exactly one value of type `SBool i`.

But `Type` has no index, why am I talking about indexed types? You’ve been tricked into reading so far, `Type` is not the most interesting GADT in this post!

To get there, we must take another small detour. An interesting feature of type families is that constructors don’t need to be fully applied to be matched. This is a notable difference between the kind-level `(->)` and the type-level `(->)`.[4](#fn4)

    type family   IsOnlyJust (x :: a -> Maybe a) :: Bool
    type instance IsOnlyJust 'Just = 'True
    
    {- -- Term-level equivalent:
       case (x :: a -> Maybe a) of
         Just -> True
       -- !? -}
    
    type family   IsOnlyMaybe (t :: Type -> Type) :: Bool
    type instance IsOnlyMaybe Maybe = 'True
    type instance IsOnlyMaybe IO    = 'False

Thus a type constructor, such as `Maybe` and `IO`, somehow also behaves like a constructor of `Type -> Type`. For the same reasons as given above for `Type`, `Type -> Type` is also GADT-like if we consider it as an atom:

    data Type->Type where
      Maybe   ::                           Type->Type
      (,)     ::                  Type  -> Type->Type
      ReaderT :: Type -> (Type -> Type) -> Type->Type
      ...

More generally, the left hand side of an arrow can be different from `Type`, so this is a thing:

    data (Type->Type)->Type where
      Fix, Mu, Nu ::             (Type->Type)->Type  -- recursion-schemes
      (:~>) :: (Type -> Type) -> (Type->Type)->Type  -- natural-transformation
      ...

`_ -> Type` is a GADT
---------------------

Now consider the kind of unary type constructors, indexed by the kind of their single type parameter.

Well, doesn’t this look like a fine GADT:

    data TyCon k where
      IO      ::                        TyCon Type
      Fix     ::                        TyCon (TyCon Type)
      Proxy   ::                        TyCon k
      (:~:)   ::                   k -> TyCon k
      Const   ::                Type -> TyCon k
      Compose :: TyCon j -> (k -> j) -> TyCon k
      Product :: TyCon k -> TyCon k  -> TyCon k
      Rec     ::         (u -> Type) -> TyCon [u]
      ...

One application of this observation is to implement type functions in Haskell via _typed_ defunctionalization ([Defunctionalization for the win](https://typesandkinds.wordpress.com/2013/04/01/defunctionalization-for-the-win/)). It makes abstract sense because GADTs are great to embed typed languages in typed languages.

In fact, this post was split off from an upcoming one about defunctionalization.

* * *

1.  I recently discovered that many people pronounce “GADaT” instead of spelling out the four letters “G.A.D.T.”. It still sounds weird when I think about it.[↩︎](#fnref1)
    
2.  It’s being argued that `newtype` should be the only mechanism for this introduction, with `data` being sugar on top of it. See [this reddit discussion about structural typing in Haskell](https://www.reddit.com/r/haskell/comments/8uhj1f/what_is_the_status_on_structural_typing_row_types/) for example.[↩︎](#fnref2)
    
3.  In Coq (and some similar languages, I assume), there is a clear distinction between “parameters” and “indices”, and they behave a bit differently. Not so much in Haskell, and the words are often used interchangeably.[↩︎](#fnref3)
    
4.  See also this Haskell-café discussion about [matchable arrows and partial type family application](https://mail.haskell.org/pipermail/haskell-cafe/2017-April/126893.html)[↩︎](#fnref4)
