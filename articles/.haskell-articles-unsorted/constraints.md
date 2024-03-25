[[https://www.reddit.com/r/haskell/comments/4yl9je/collection_of_type_class_and_constraint_tricks/][Reddit discussion]].

*Disclaimer 1:* Type classes are great but they are not the right tool for every job. Enjoy some [[http://www.haskellforall.com/2012/05/scrap-your-type-classes.html][balance]] and [[http://mpickering.github.io/posts/2018-03-20-recordsvstypeclasses.html][balance to your balance]].

*Disclaimer 2:* I should tidy this up but probably won't.

*Disclaimer 3:* Yeah called it, better to be realistic.

Type classes are a language of their own, this is an attempt to document features and give a name to them.

/Work in progress./

* Type classes without methods — Constraining types, precluding exotic types
** ([[https://hackage.haskell.org/package/ivory-0.1.0.3/docs/src/Ivory-Language-Ref.html][Hackage]]) EDSL, Ivory

: -- | Things that can be safely stored in references.
: class IvoryVar a => IvoryStore a where
: 
: -- simple types
: instance IvoryStore IBool
: instance IvoryStore IChar
: instance IvoryStore Uint8
: instance IvoryStore Uint16
: instance IvoryStore Uint32
: instance IvoryStore Uint64
: instance IvoryStore Sint8
: instance IvoryStore Sint16
: instance IvoryStore Sint32
: instance IvoryStore Sint64
** [[http://www.cse.chalmers.se/~josefs/publications/paper21_cameraready.pdf][Generic Monadic Constructs for Embedded Languages]] 

: class    Signature a
: instance Signature (Full a)
: instance Signature b => Signature (a :-> b)

used to constraint arguments to =Sym=
: data AST dom a where
:   Sym  :: Signature a => dom a -> AST dom a
:   (:$) :: Typeable  a => AST dom (a :-> b) -> AST dom (Full a) -> AST dom b 

** [[http://www.cse.chalmers.se/~emax/documents/svenningsson2015combining.pdf][Combining Deep and Shallow Embedding of Domain-Specific Languages]]

/7.4.1.  Enforcing First-Order Target Code/

We have seen that restricting programs so that higher-order types only appear for expressions that the code generator knows how to handle ensures that we can generate first-order code from =FunC=. This restriction can be enforced by constraining the type of =Lam=:

: Lam :: Type a => (FunC a -> FunC b) -> FunC (a -> b)

The =Type= class captures simple types that can be stored in variables in the target language (e.g. =Int=, =Bool=, =Float=, =Array Int Int=, etc.). This is a class without methods, and it is only used to restrict the set of expressions we can construct. We will now argue why the restricted type of =Lam= rules out arbitrary higher-order types. [...]
** [[http://homepages.inf.ed.ac.uk/slindley/papers/unembedding.pdf][Unembedding Domain-Specific Languages]]
Uses =Representable= similarly by not using the method =rep= in certain functions (the authors credit [[http://www.cs.ox.ac.uk/ralf.hinze/publications/HW02.pdf][A Lightweight Implementation of Generics and Dynamics]] with the type class =Representable=):

: data Rep :: Type -> Type where
:   Bool  :: Rep Bool
:   (:->) :: (Representable a, Representable b) => Rep a -> Rep b -> Rep (a -> b)
: 
: class Representable a where
:   rep :: Rep a
: 
: instance Representable Bool where
:   rep :: Rep Bool
:   rep = Bool
: 
: instance (Representable a, Representable b) => Representable (a -> b) where
:   rep :: Rep (a -> b)
:   rep = rep :-> rep

used

: class TypedLambda exp where
:   tlam :: (Representable a, Representable b) => (exp a -> exp b) -> exp (a -> b)
:   tapp :: (Representable a, Representable b) => exp (a -> b) -> (exp a -> exp b)

This is similar to the upcoming [[https://ghc.haskell.org/trac/ghc/wiki/TypeableT][type-indexed type representation]] of =Typeable= which is also kind polymorphic:

: data TypeRep (a :: k) -- abstract
: 
: class Typeable (a :: k) where
:   typeRep :: TypeRep a
** ([[http://homepages.inf.ed.ac.uk/jmorri14/d/final.pdf][pdf]]) Type Classes and Instance Chains: A Relational Approach

: class HasNone t l
: instance HasNone t Nil
: instance Fail (TypeExists t) => HasNone t (Cons t ts)
: instance HasNone t ts => HasNone t (Cons u ts)

“Next, we discuss the =HasNone= predicate. As this class is only used as a pre-condition for instances, it does not need any member functions.”
* ([[https://hackage.haskell.org/package/reflection][hackage]]) =reflection=
[[https://www.reddit.com/r/haskell/comments/2dw3zq/haskells_type_classes_why_we_can_do_better/cjtvsd7][reddit]]: "With reflection you can make up instances on the fly, but still reason about the resulting types and not lose coherence."

Provides functions that take type class methods (=mappend= / =mempty=) as arguments:

: foldBy    :: Foldable t => (m -> m -> m) -> m -> t m -> m
: foldMapBy :: Foldable t => (m -> m -> m) -> m -> (a -> m) -> t a -> m

We recover =fold=, =foldMap= by supplying the =Monoid= methods explicitly:

: fold :: (Monoid m, Foldable t) => t m -> m
: fold = foldBy mappend mempty

: foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
: foldMap = foldMapBy mappend mempty

Same with =traverse= where we supply the =Applicative= methods (=pure=, =<*>=) (don't worry about the scary types):

: traverseBy :: Traversable t => (forall x. x -> f x) -> (forall x y. f (x -> y) -> f x -> f y) -> (a -> f b) -> t a -> f (t b)
: sequenceBy :: Traversable t => (forall x. x -> f x) -> (forall x y. f (x -> y) -> f x -> f y) -> t (f a) -> f (t a) 

We recover =traverse=, =sequenceA= by supplying the =Applicative= methods explicitly: 

: traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
: traverse = traverseBy pure (<*>)

: sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
: sequenceA = sequenceBy pure (<*>)

This shows how to do this for more types: [[https://www.schoolofhaskell.com/user/thoughtpolice/using-reflection][Reflecting values to types and back]]
* Applicative lifting
Making instances of functions, hardly a trick but may be interesting:

: instance Monoid b => Monoid (a -> b)

effectively defines =mempty = pure mempty= and =mappend = liftA2 mappend=. Let's be explicit with types using visible type application =TypeApplications=:

: mempty  @(a -> b) = pure   @((->) _) (mempty  @b)
: mappend @(a -> b) = liftA2 @((->) _) (mappend @b)

means that =mempty= has the following types (this can be written =mempty=, =mempty @(_ -> _)= and =mempty @(_ -> _ -> _)=)

: mempty :: Monoid a => a

: mempty :: Monoid b => a -> b
: mempty _ = mempty

: mempty :: Monoid c => a -> b -> c
: mempty _ _ = mempty

and =mappend=

: mappend :: Monoid a => a -> a -> a
: mappend a b = mappend a b

: mappend :: Monoid b => (a -> b) -> (a -> b) -> (a -> b)
: (mappend f g) x = mappend (f x) (g x)

: mappend :: Monoid c => (a -> b -> c) -> (a -> b -> c) -> (a -> b -> c)
: (mappend f g) x y = mappend (f x y) (g x y)

These last two may be clearer when written infix =(<>) = mappend=

: (f <> g) x   = f x   <> g x
: (f <> g) x y = f x y <> g x y

** Any Applicative is Num

: instance Num a => Num (Maybe a) where
:   (+)         = liftA2 (+)
:   (-)         = liftA2 (-)
:   (*)         = liftA2 (*)
:   abs         = fmap abs
:   signum      = fmap signum
:   negate      = fmap negate
:   fromInteger = pure . fromInteger

Because =Applicative ((->) a)= this includes [[http://stackoverflow.com/questions/26515102/making-numeric-functions-an-instance-of-num][making functions numbers]]:

: instance Num b => Num (a -> b) where
:   (+)         = liftA2 (+)
:   (-)         = liftA2 (-)
:   (*)         = liftA2 (*)
:   abs         = fmap abs
:   signum      = fmap signum
:   negate      = fmap negate
:   fromInteger = pure . fromInteger

** ([[http://www.mimuw.edu.pl/~szynwelski/nlambda/nlambda.pdf][pdf]]) SMT Solving for Functional Programming over Infinite Structures

: class Conditional a where
:   cond :: Formula -> a -> a -> a

: instance Conditional Formula where
:   cond :: Formula -> Formula -> Formula -> Formula
:   cond f1 f2 f3 = (f1 /\ f2) \/ (not f1 /\ f3)

: instance Conditional b => Conditional (a -> b) where
:   cond :: Formula -> (a -> b) -> (a -> b) -> (a -> b)
:   (cond c f1 f2) x = cond c (f1 x) (f2 x)

** ([[https://github.com/mikeizbicki/subhask][github]]) Subhask
Used heavily in =subhask=, [[https://ghc.haskell.org/trac/ghc/ticket/10592#comment:12][simplified]] code:

: instance Eq b => Eq (a -> b) where
:   type Logic (a -> b) = a -> Logic b
: 
:   (==) :: (a -> b) -> (a -> b) -> (a -> Logic b)
:   (f == g) a = f a == g a

: instance POrd b => POrd (a -> b) where
:   (inf f g) a = inf (f a) (g a)

: instance MinBound b => MinBound (a -> b) where
:   (minBound) _ = minBound

: instance Lattice b => Lattice (a -> b) where
:   (sup f g) a = sup (f a) (g a)

: instance Bounded b => Bounded (a -> b) where
:   (maxBound) _ = maxBound

: instance Complemented b => Complemented (a -> b) where
:   (not f) a = not (f a)

: instance Heyting b => Heyting (a -> b) where
:   (f ==> g) a = f a ==> g a

: instance Boolean b => Boolean (a -> b)

* Accept values with varying number of arguments
Common technique used in /EDSLs/ (/embedded domain-specific languages/) to accept functions with varying number of arguments.

** ([[https://hackage.haskell.org/package/QuickCheck][Hackage]]) =QuickCheck=
: quickCheck :: Testable prop => prop -> IO ()

/QuickCheck/ allows testing values as well as functions of one, two or more arguments:

: quickCheck :: Bool                        -> IO ()
: quickCheck :: (Int -> Bool)               -> IO ()
: quickCheck :: (Int -> Int -> Bool)        -> IO ()
: quickCheck :: (Int -> Int -> Int -> Bool) -> IO ()

This is enabled by these instances

: instance                                         Testable Bool
: instance (Arbitrary a, Show a, Testable prop) => Testable (a -> prop)

Keep in mind that the type =Int -> Int -> Int -> Bool= is =Int -> (Int -> (Int -> Bool))=.

** ([[https://hackage.haskell.org/package/simple-reflect-0.3.2/docs/Debug-SimpleReflect-Expr.html][Hackage]], [[http://twanvl.nl/blog/haskell/simple-reflection-of-expressions][blog]]) =simple-reflect= (=import Debug.SimpleReflect=)


: class FromExpr a where
:   fromExpr :: Expr -> a
: 
: instance FromExpr Expr where
:   fromExpr :: Expr -> Expr
:   fromExpr = id @Expr
: 
: instance (Show a, FromExpr b) => FromExpr (a -> b) where
:   fromExpr :: Expr -> (a -> b)
:   fromExpr f a = fromExpr (op InfixL 10 " " f (lift a))

With functions like

: fun :: FromExpr a => String -> a

: fun @(Bool -> _)                   :: FromExpr t => String -> Bool -> t
: fun @(Bool -> () -> _)             :: FromExpr t => String -> Bool -> () -> t
: fun @(Bool -> () -> Float -> _)    :: FromExpr t => String -> Bool -> () -> Float -> t
: fun @(Bool -> () -> Float -> Expr) ::               String -> Bool -> () -> Float -> Expr

: >>> foobar = fun @(Bool -> () -> Float -> Expr) "foobar"
: >>> foobar False () pi 
: foobar False () 3.1415927
: >>> f (foobar False () pi) :: Expr
: f (foobar False () 3.1415927)

** ([[https://hackage.haskell.org/package/sbv][Hackage]]) =sbv=
[[https://hackage.haskell.org/package/sbv-5.12/docs/Data-SBV.html#v:prove][prove]] from /sbv/.

: prove :: Provable a => a -> IO ThmResult

has instances 
: instance                            Provable SBool
: instance (SymWord a, Provable p) => Provable (SBV a -> p)

so it can accept symbolic Booleans

: prove :: SBool -> IO ThmResult

as well as Haskell functions returning symbolic Booleans
: prove :: SymWord a => (SBV a -> SBool) -> IO ThmResult
** ([[https://github.com/Feldspar/feldspar-language/blob/74c29a7a5446915f91c5ec4126f4964ee56d893b/src/Feldspar/Core/Frontend.hs#L316][github]]) Feldspar
Test that two function of the same arity have the same semantics
: class Equal a where
:   (====) :: a -> a -> Property

: instance (Eq a, Show a) => Equal a where
:   (====) :: a -> a -> Property
:   x ==== y = x === y
: instance (Show a, Arbitrary a, Equal b) => Equal (a -> b) where
:   (====) :: (a -> b) -> (a -> b) -> Property
:   f ==== g = property (\x -> f x ==== g x)

So 
: (====) :: Equal a                        => a        -> a        -> Property
: (====) :: (Show a, Arbitrary a, Equal b) => (a -> b) -> (a -> b) -> Property

** ([[https://github.com/AccelerateHS/accelerate/blob/179fb230a6af1aa10789c96c1d9be45a2f627b13/Data/Array/Accelerate/Trafo/Vectorise.hs#L2285][github]]) Accelerate

: class    Afunction f               => Convertible f 
: instance Arrays a                  => Convertible (S.Acc a) 
: instance (Arrays a, Convertible b) => Convertible (S.Acc a -> b) 

: fromHOAS :: (Convertible f, Kit acc) => f -> AfunctionR acc aenv f

(Also in the EDSL [[https://github.com/GaloisInc/ivory/blob/master/ivory/src/Ivory/Language/Proc.hs#L30][Ivory]])
** ([[https://gist.github.com/Pitometsu/3b946d456dbda409ff31#file-solver-hs-L22][github]]) Get number of arguments of function
: class Arity f where
:   arity :: f -> Int
: 
: instance Arity x where
:   arity :: x -> Int
:   arity _ = 0
: 
: instance Arity b => Arity (a -> b) where
:   arity :: (a -> b) -> Int
:   arity f = 1 + arity (f undefined) 

([[cs.brynmawr.edu/~rae/papers/2016/thesis/eisenberg-thesis-draft.pdf][thesis]]) Note that Haskell is unique in the ability to write a type family (type function) that counts the number of arrows in a function type

: data Nat = O | S Nat
: 
: type family
:   CountArgs (f :: Type) :: Nat where
:   CountArgs (_ -> b) = S (CountArgs b)
:   CountArgs _        = O
* ([[https://hackage.haskell.org/package/lens-4.14/docs/Control-Lens-Internal-Indexed.html#t:Conjoined][Hackage]]) Conjoined
Discussed in [[https://ghc.haskell.org/trac/ghc/ticket/12466#comment:27][#12466]]. Expand later.

: class ... => Conjoined p where
:   conjoined :: ((p ~ (->)) => q (a -> b) r) -> q (p a b) r -> q (p a b) r
:   conjoined _ r = r

* Constraint Synonym Encoding (or [[http://haskell.1045720.n5.nabble.com/forall-in-constraint-td5866393.html]["class synonym"]])

(https://ryanglscott.github.io/2019/11/30/four-ways-to-partially-apply-constraint-tuples/)

Here is a very old refence by Hughes 1999, [[https://citeseerx.ist.psu.edu/viewdoc/download;jsessionid=5A590F2B91BB86EA05EA6B9BF870AB29?doi=10.1.1.39.2816&rep=rep1&type=pdf][Restricted Data Types in Haskell]]:

> A natural complement would be to give contexts names via /context synonyms/. Interestingly, we can almost do so in Haskell already. A ‘synonym’ for the context =(A a, B a)= can be modelled by a new class an instance

: class    (A a, B a) => AB a
: instance (A a, B a) => AB a

Recently I've heard Ryan Scott call them [[https://github.com/goldfirere/singletons/issues/318#issuecomment-464364638]["class newtypes"]] which is a cool name.

** Disclaimer
There are differences between using constraint synonyms and encoding them with type classes ([[https://www.reddit.com/r/haskell/comments/5nr5xb/kwangs_haskell_blog_constraint_kinds/dce8cxp/?utm_content=permalink&utm_medium=front&utm_source=reddit&utm_name=haskell][reddit comment]]): A type class creates a new type constructor so

: class    (Show a, Read a) => Text1 a
: instance (Show a, Read a) => Text1 a
: 
: class    (Show a, Read a) => Text2 a
: instance (Show a, Read a) => Text2 a

So they represent *different* constraints, `Text1 a ~ Text2 a` does not hold.

** Description
 ([[https://ghc.haskell.org/trac/ghc/ticket/11523][#11523]])  Cyclic definition of a class to make constraint alias

: class    (Foo f, Bar f) => Baz f
: instance (Foo f, Bar f) => Baz f

that we can partially apply unlike =type Baz f = (Foo f, Bar f)=. This wouldn't be needed if [[https://ghc.haskell.org/trac/ghc/ticket/11715#comment:14][* and Constraint are made the same]]. See [[https://www.reddit.com/r/haskell/comments/10clph/class_aliases_revisited/][Class Alias Proposal]] and its [[https://www.reddit.com/r/haskell/comments/10clph/class_aliases_revisited/][reddit discussion]].

[[https://www.reddit.com/r/haskell/comments/3mb8lb/monad_of_no_return_proposal_mrp/cvdzagm][Random comment]]:

"
: type Applicative f => (Pointed f, Apply f)

is strictly less useful than

: class    (Pointed f, Apply f) => Applicative f
: instance (Pointed f, Apply f) => Applicative f

the latter can be partially applied which is very important once you start using =ConstraintKinds= in anger and the former requires an extension to be turned on at the use site."

Also mentioned in [[https://www.reddit.com/r/haskell/comments/49em92/haskells_typeclasses_we_can_do_better/d0rcmwp][this Reddit thread]]:

"Can't you do exactly this with constraint kinds?

: {-# LANGUAGE ConstraintKinds #-}
: type MyClass t a = (Monad t, Monoid (t a))
: f :: MyClass t a => a -> a -> t a
: f x y = return x `mappend` return y
"

"Or with UndecidableInstances instead of ConstraintKinds:

: class (Monad t, Monoid (t a)) => MyClass t a
: instance (Monad t, Monoid (t a)) => MyClass t a
"

"This has the benefit of

a.) not requiring the end user to turn on an extension and

b.) letting you talk about =MyClass :: (* -> *) -> * -> Constraint= without having to apply it to type arguments.

Between those two advantages I can't bring myself to ever use a =Constraint=-kinded type synonym."

[[https://www.reddit.com/r/haskell/comments/2jg4y5/hiding_liftio/clbj0u2][reddit thread]] about when this matters.

** ([[http://cs.brynmawr.edu/~rae/papers/2012/singletons/paper.pdf][pdf]], 2012) History: Dependently Typed Programming with Singletons

The =SingRep= class is essentially a synonym for the combination of =SingI= and =SingE=.⁵ As such, it is unnecessary for the =singletons= library to generate instances for it. All parameters to singleton type constructors have a =SingRep= constraint, allowing a programmer to use =sing= and =fromSing= after pattern matching with these constructors.

: class    (SingI a, SingE a) => SingRep a
: instance (SingI a, SingE a) => SingRep a

⁵ With the new =ConstraintKinds= extension, it is possible to make a true synonym, using =type=, for a pair of class constraints. However, pairs of constraints are not yet compatible with Template Haskell, so we are unable to use the simplification.
** ([[https://ghc.haskell.org/trac/ghc/ticket/9838#comment:3][#9838]]) History

"These sorts of synonyms are actually quite common, I think I first used it back around 2006 when I first found haskell: e.g. TXOr in ​http://hackage.haskell.org/package/type-int-0.5.0.2/docs/src/Data-Type-Boolean.html#TXOr is an instance of this pattern, but it was a well known pattern long before the time I started using it.

The former requires "scarier" extensions than the latter for the person constructing it, but the latter requires an extension to be turned on by the user, hoisting the library designer upon the horns of a dilemma, do they take the burden upon themselves and use "the old way" of thinking about these things or do they make every user turn on `ConstraintKinds`?"

** ([[https://lirias.kuleuven.be/bitstream/123456789/259608/1/paper.pdf][pdf]])

A lightweight encoding for constraint synonyms can be given using a class and a single catch-all instance, e.g.

: class    (Additive a, Multiplicative a, FromInteger a) => Num a where ...
: instance (Additive a, Multiplicative a, FromInteger a) => Num a where ...

However, this approach requires GHC's /undecidable instances/ extension, removing conservative termination conditions on type class instances. This extension is globally applied, thus type-checking decidability can no longer be guaranteed — an unnecessary, undesirable side-effect of this encoding. An alternative to using /undecidable instances/ is to supply individual instances for each required type. This is tedious and even more inelegant than the above auxiliary class definition.

** ([[https://github.com/ekmett/hask][gihub]]) Heavily used in the =Hask= library
[[https://github.com/ekmett/hask/blob/master/src/Hask/Category.hs#L93][FunctorOf]] which allows it to be made the object constraint for the functor category:

: class    (Functor f, Dom f ~ p, Cod f ~ q) => FunctorOf p q f
: instance (Functor f, Dom f ~ p, Cod f ~ q) => FunctorOf p q f

and [[https://github.com/ekmett/hask/blob/master/src/Hask/Category/Polynomial.hs#L35][ProductOb]] which allows it to be made the object constraint for product categories:
: class    (Ob p (Fst a), Ob q (Snd a)) => ProductOb (p :: i -> i -> *) (q :: j -> j -> *) (a :: (i,j))
: instance (Ob p (Fst a), Ob q (Snd a)) => ProductOb (p :: i -> i -> *) (q :: j -> j -> *) (a :: (i,j))

** Alias for =(,) :: Constraint -> Constraint -> Constraint=
Same as =type p & q = ((p :: Constraint), (q :: Constraint))= except it can be partially applied.

: class    (p, q) => p & q
: instance (p, q) => p & q

: >>> :kind (&)
: (&) :: Constraint -> Constraint -> Constraint
: >>> :kind (&) (Eq _)
: (&) (Eq _) :: Constraint -> Constraint
: >>> :kind (&) (Eq _) (Ord _)
: (&) (Eq _) (Ord _) :: Constraint
** ([[https://hackage.haskell.org/package/exists-0.2/docs/Control-Constraint-Combine.html][Hackage]]) Combining constraints
Same as =type (c :&: d) a = (c a, d a)= except it can be partially applied.

: class    (c a, d a) => (c :&: d) a
: instance (c a, d a) => (c :&: d) a
: infixl 7 :&:

: type c `And` d = c :&: d
: infixl 7 `And`

: class    f (g x) => (f `Compose` g) x
: instance f (g x) => (f `Compose` g) x
: infixr 9 `Compose`

(called =Empty= in =exists= package)
: class    Top x
: instance Top x

This allows =Exists (Show :&: Eq)=

: data Exists c where
:   Exists :: c a => a -> Exists c

See also [[http://stackoverflow.com/questions/36964664/haskell-combine-multiple-typeclass-constraints][Haskell combine multiple typeclass constraints]], [[https://hackage.haskell.org/package/generics-sop-0.2.2.0/docs/src/Generics-SOP-Constraint.html#Compose][generics-sop]]. Used in [[https://hackage.haskell.org/package/syntactic-1.0/docs/src/Language-Syntactic-Constraint.html][syntactic]]
** ([[https://hackage.haskell.org/package/type-operators-0.1.0.3/docs/Control-Type-Operator.html][Hackage]]) Mapping constraints
=type-operators= contains type families for mapping constraints

:   Show <=> [a, b]
: = (Show a, Show b)

:   [Show, Read] <+> a
: = (Show a, Read a)

* ([[https://hackage.haskell.org/package/constraints][Hackage]]) No instances allowed!
From [[https://github.com/ekmett/constraints/blob/master/src/Data/Constraint.hs#L327][Data.Constraint]]:

: import GHC.Exts (Any)

=Any= inhabits every kind, including =Constraint= but is uninhabited, making it impossible to define an instance.
: class Any => Bottom where
:   no :: a
: 
: bottom :: Bottom :- a
: bottom = Sub no

We cannot define
: instance Bottom 

* ([[https://ghc.haskell.org/trac/ghc/ticket/9334#comment:9][#9334]]) “Fail instances”: An instance should never exist 
: class FailHasNoInstances a => Fail a
: class FailHasNoInstances a -- not exported to ban Fail instances

allowing

: instance Fail "Char may not have a Num instance" => Num Char

: >>> print ('1' + '1')
: <interactive>:129:16: error:
:     • No instance for (Fail "Char may not have a Num instance")
:       arising from a use of ‘+’

I don't know how this compares to [[https://ghc.haskell.org/trac/ghc/wiki/Proposal/CustomTypeErrors][custom type errors]] as a superclass:

: import GHC.TypeLits
: 
: instance TypeError (Text "Boo-urns!") => Num Char

: >>> '1' + '1'
: <interactive>:6:1: error:
:   • Boo-urns!
:   • In the expression: '1' + '1'
:     In an equation for ‘it’: it = '1' + '1'

** ([[http://homepages.inf.ed.ac.uk/jmorri14/d/final.pdf][pdf]]) Type Classes and Instance Chains: A Relational Approach
Example drawn from (old) definition of HLists, I don't know how this compares to previous versions:

: data Nil       = Nil
: data Cons t ts = t :*: ts

: class Fail t
: data TypeExists t

: class    HasNone t l
: instance HasNone t Nil
: instance Fail (TypeExists t) => HasNone t (Cons t ts)
: instance HasNone t ts        => HasNone t (Cons u ts)

“Instead, the original authors relied on creating a context that cannot be satisfied—in this case, the predicate =Fail (TypeExists t)=, named to give some indication as to the reason for the unsatisfiable predicate.  Of course, were the =Fail= class and =TypeExists= types accessible outside the definition of this instance, a new instance satisfying =Fail (TypeExists t)= could be added. Thus, either the class or the type must be hidden using an additional mechanism, such as the Haskell module system.”
* ([[http://www.diku.dk/~paba/pubs/files/serrano15haskell-paper.pdf][pdf]]) Cases where type families cannot be emulated by fundeps
“As we have seen above, type classes with functional dependencies can simulate type families. 
This translation works well in most situations, with the notable exception of certain data type definitions.
For example, take the following family-dependent data type:

: type family Family a
: data Example a = Example (Family a)

and its corresponding type class translation:

: class FunDep a b | a -> b
: data Example' a where
:   Example' :: FunDep a b => b -> Example' a

where =b= is implicitly existentially quantified. The compiler can type
check the following definition:

: f :: Example a -> Family a
: f (Example x) = x

but not the one with functional dependencies:

: g :: FunDep a b => Example' a -> b
: g (Example' x) = x

since the compiler does not know whether the type =b=, wrapped by the GADT constructor, is the same as in the signature.

Thus, at the moment, type class with functional dependencies do not cover all use cases of type families.
* ([[http://chrisdone.com/posts/haskell-constraint-trick][blog]], [[https://www.reddit.com/r/haskell/comments/3afi3t/the_constraint_trick_for_instances/][reddit]]) Constraint trick for instances

** Uses
*** Base
: instance a ~ ()          => PrintfType  (IO a)
: instance a ~ ()          => HPrintfType (IO a)
: instance a ~ Char        => IsString    [a]
: instance a ~ Char        => IsString    (DList a)
: instance (a ~ b, Data a) => Data        (a :~: b)

*** Lens
: instance (a ~ a', b ~ b') => Each (a, a') (b, b') a b
*** [[https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/MagicClasses][MagicClasses]]
: instance t ~ Float => HasField "area" Triangle t where
:    getField _ (Tri b h _) = 0.5 * b * h
: instance t ~ Float => HasField "area" Circle t where
:    getField _ (Circle r) = pi * r * r

** Workaround
Applying this trick changes the *instance head*, this usually doesn't matter but in cases with type instances ([[https://ghc.haskell.org/trac/ghc/ticket/12551][#12551]]) it can matter if you need to match on the original instance head or if you need to reference a variable outside of it. Minimal example which works fine:

: data Full :: Type -> Type
: 
: data Exp a where
:   Lit :: Int -> Exp (Full Int)
: 
: class Syntactic a where
:   type Rep a
: 
: instance Syntactic (Exp (Full a)) where
:   type Rep (Exp (Full a)) = a

But if you want to write a `Syntactic` instance with the instance head `Exp full_a`, we run into trouble

: -- • Type indexes must match class instance head
: --   Found ‘Exp (Full a)’ but expected ‘Exp full_a’
: -- • In the type instance declaration for ‘Rep’
: --   In the instance declaration for ‘Synt (Exp full_a)’
: 
: instance full_a ~ Full a => Synt (Exp full_a) where
:   type Rep (Exp (Full a)) = a

: -- The RHS of an associated type declaration mentions ‘a’
: -- All such variables must be bound on the LHS
: 
: instance full_a ~ Full a => Synt (Exp full_a) where
:   type Rep (Exp full_a) = a

To get around this we need to define a type family 

: type family
:   RemoveFull a where
:   RemoveFull (Full a) = a
: 
: instance full_a ~ Full a => Synt (Exp full_a) where
:   type Rep (Exp full_a) = RemoveFull full_a

* ([[https://github.com/mikeizbicki/ifcxt][github]], [[https://www.reddit.com/r/haskell/comments/3jf2tq/using_rankntypes_and_constraintkinds_to_create_an/][reddit]]) Constraint level /if/

[[http://chrisdone.com:10001/browse/haskell/?id=21383554&timestamp=1441590348#t1441590348][IRC]]:
: 2015-09-07 03:45:48 +0200	<edwardk> so in the end its basically an incredibly easily broken toy solution

If you have two versions of your function

: nubEq  :: Eq  a => [a] -> [a]
: nubOrd :: Ord a => [a] -> [a]

where =nubEq= is slower than =nubOrd=, we want to use =nubOrd= whenever an =Ord= constraint is available and =nubEq= otherwise:

: nub :: forall a. (Eq a, IfCxt (Ord a)) => [a] -> [a]
: nub = ifCxt (Proxy::Proxy (Ord a)) nubOrd nubEq

https://stackoverflow.com/questions/28571035/is-there-a-workaround-for-the-lack-of-type-class-backtracking

** ([[https://www.reddit.com/r/haskell/comments/3jf2tq/using_rankntypes_and_constraintkinds_to_create_an/cuoqern][reddit]]) Horrible horrible solution

"And if you're willing to pass in a witness as an argument, you can also use ekmett's constraints package." [[https://gist.github.com/jkarni/8d24a13b4be4cbbb1373][Example]]:

: import Data.Constraint
: 
: show' :: (Maybe (Dict (Show a))) -> a -> String
: show' (Just Dict) x = show x
: show' Nothing    _ = "<<not showable>>"
: 
: t1 = show' (Just Dict) 5
: -- "5"
: 
: t2 = show' Nothing id
: -- "<<not showable>>"
: 
: -- Doesn't typecheck
: -- t3 = show' (Just Dict) id

"Actually, with defer-type-errors you don't even need the witness!" [[https://gist.github.com/jkarni/0872c83bc4264a23fec1][Example]]:

: {-# LANGUAGE ConstraintKinds #-}
: {-# LANGUAGE ScopedTypeVariables #-}
: {-# OPTIONS_GHC -fdefer-type-errors #-}
: module Main where
: 
: import Data.Constraint
: import Control.Exception
: import System.IO.Unsafe
: 
: show' y = unsafePerformIO $ do
:     res <- try $ evaluate $ show'' Dict y
:     case res of
:       Right x -> return x
:       Left  (ErrorCall _) -> return "no show"
: 
: show'' :: (Dict (Show a)) -> a -> String
: show'' Dict x = show x
: 
: t1 = show' 5
: -- "5"
: 
: t2 = show' id
: -- "no show"

See also Deferrable.
* ([[https://hackage.haskell.org/package/constraints-0.8/docs/Data-Constraint-Deferrable.html][Hackage]]) Transform static errors into dynamic checks (Deferrable)
Originally from [[http://people.seas.harvard.edu/~pbuiras/publications/icfp2015.pdf][HLIO: Mixing Static and Dynamic Typing
for Information-Flow Control in Haskell]] which includes examples of use.

* ([[https://mail.haskell.org/pipermail/glasgow-haskell-users/2016-April/026185.html][mail]]) Avoid overlapping instances (see: [[http://web.cecs.pdx.edu/~mpj/pubs/instancechains.pdf][instance chains]])

: instance C [Int]
: instance C [a]

: class IsInt a ~ b => CHelper a b
: instance CHelper Int True
: instance IsInt a ~ False => CHelper a   False

: type family IsInt a where
:   IsInt Int = True
:   IsInt a   = False

: instance CHelper a (IsInt a) => C [a]

See also: [[https://www.reddit.com/r/haskell/comments/4qqz89/overlappinginstances_workarounds/][OverlappingInstances workarounds]]

** ([[https://gist.github.com/goldfirere/846c3faf7640e27025f0#file-basics-hs-L183][github]]) =Typeable=

This example basically mocks up [[http://web.cecs.pdx.edu/~mpj/pubs/instancechains.pdf][(closed) instance chains]] (or [[https://ghc.haskell.org/trac/ghc/ticket/9334#comment:6][instance groups]]?)

([[https://ghc.haskell.org/trac/ghc/ticket/9334][#9934]])

A type is either a constructor (=Int=, =Maybe=, =(->)=, ...) or a type
application (=Maybe Int=, =(->) Int=, ...). We create a dummy Typeable'
class with an extra argument which is =True=  when it is an application and =False= otherwise:

: type family 
:   CheckPrim a where
:   CheckPrim (_ _) = 'False
:   CheckPrim _     = 'True

: class Typeable' a (b :: Bool) where
:   typeRep' :: TypeRep a

: instance (Primitive a, TyConAble a) => Typeable' a 'True where
:   typeRep' = TyCon tyCon

: instance (Typeable a, Typeable b) => Typeable' (a b) 'False where
:   typeRep' = TyApp typeRep typeRep

We can then define our =Typeable= that matches =Typeable' a 'True= when
/a/ is a constructor and =Typeable' (a b) 'False= when it is an application:

: type Typeable a = Typeable' a (CheckPrim a)
* Closed type class
: class    ClosedTypeClass_ x
: instance ClosedTypeClass_ Int
: instance ClosedTypeClass_ Float

: type family
:   ClosedTypeClass x :: Constraint where
:   ClosedTypeClass x = ClosedTypeClass_ x

Only export =ClosedTypeClass=, now further instances cannot be added.

* ([[https://twitter.com/ezyang/status/750364746252165120][tweet]]) Type families v. functional dependencies

: Edward Kmett @kmett Jul 5

: @ezyang Also, it is easy to go from TFs to fundeps, by using silly shim classes / constraint aliases but you can't go back the other way.

* ([[https://www.reddit.com/r/haskell/comments/3afi3t/the_constraint_trick_for_instances/cscb33j][reddit]]) Backtracking
Why the instance resolution doesn't backtrack.

* ([[https://twitter.com/ezyang/status/751570379332804608][tweet]]) Relationship between type classes and logic programming
* ([[https://www.reddit.com/r/haskell/comments/4t089y/typeclasses_and_runtime_dependency_management/][reddit]]) Typeclasses and Run-Time Dependency Management 
* Things that are being discussed
** ([[https://ghc.haskell.org/trac/ghc/ticket/5927][#5927]]) Type level =Implies=
** ([[https://ghc.haskell.org/trac/ghc/ticket/2893][#2893]]) Quantified contexts
* ([[https://csks.wordpress.com/2012/10/17/redshift-phase-2-types-classes-for-types-with-arbitrary-numbers-of-parameters/][site]], [[https://www.reddit.com/r/haskell/comments/11nj47/a_class_that_generalizes_over_functor_functor2/][reddit]]) Type classes with arbitrary number of parameters

[[https://gist.github.com/phadej/2fc066c00e33b9486e1a3e5f7767a8d7][See also]].
* ([[https://ghc.haskell.org/trac/ghc/ticket/7543#comment:2][#7543]]) Pie in sky: Constraints in type synonyms
This is not a feature, quoting Edward:

"More pie-in-the-sky would be being able to use

: type Foo a b = Bar a => Baz a b

: instance Foo a b

as

: instance Bar a => Baz a b

it would be consistent with the other uses of type, but =Bar a => Baz a b= doesn't (currently) have a sensible kind."
* ([[https://gist.github.com/phadej/29335981507d81b4b2f219961772de25][github]]) Existential constraints

* ([[https://ghc.haskell.org/trac/ghc/ticket/9115][#9115]]) The kind of =(=>)=
* ([[http://pointless-haskell.tumblr.com/][tumblr]]) “Pointless Haskell”
Humorous stuff, most having to do with type classes or constraints.
* ([[https://www.reddit.com/r/haskell/comments/3jf2tq/using_rankntypes_and_constraintkinds_to_create_an/cupfujo][reddit]]) Random philosophy

"I remember asking myself the same question in the context of this function:

: {-# LANGUAGE GADTs, ScopedTypeVariables, TypeOperators #-}
: 
: import Data.Typeable
: 
: mwahaha :: forall a. Typeable a => a -> a
: mwahaha a = case eqT :: Maybe (a :~: String) of
:              Just Refl -> "mwahaha!!!"
:              Nothing -> a
The answer I came up with for myself was based on the logical concepts of [[https://en.wikipedia.org/wiki/Contingency_(philosophy)][contingent vs. tautological truth]].

It goes something like this: even though GHC can derive a =Typeable= instance for any type, that's however only /contingently/ true. Haskell's type system is compatible with that circumstance, but also with its negation. So even if it is true that every type is =Typeable= you still need to appeal to the =Typeable a= constraint—an "extralogical premise" (so to speak)—in order to write this function.

In contrast, =(\a -> a) :: a -> a= is just an inevitable consequence of the the way the type system works. So there's no need to appeal to an "extralogical" constraint. And then that's what parametricity is about."

** See also
https://www.reddit.com/r/haskell/comments/70ixk0/code_challenge_bad_id/
* ([[http://stackoverflow.com/questions/32576018/memoize-the-result-of-satisfying-a-constraint/32577838][StackOverflow]]) Memoize the result of satisfying a constraint

I haven't reviewed this but it looks interesting
* ([[https://github.com/haskell-servant/servant/blob/a844b7c297871e4a24afa8d30594ae755fdd34c7/servant/src/Servant/Utils/Links.hs#L133][github]]) =Or=, =And= for constraints
From [[https://hackage.haskell.org/package/servant][servant]]:

If either /a/ or /b/ produce an empty constraint, produce an empty constraint. This works because of [[https://ghc.haskell.org/trac/ghc/wiki/NewAxioms/CoincidentOverlap][coincident overlap within a closed type family]]:

: type family 
:   Or (a :: Constraint) (b :: Constraint) :: Constraint where
:   Or () b       = ()
:   Or a ()       = ()

If both /a/ or /b/ produce an empty constraint, produce an empty constraint.
: type family 
:   And (a :: Constraint) (b :: Constraint) :: Constraint where
:   And () () = ()
* ([[https://www.reddit.com/r/haskell/comments/37c4d7/til_you_can_write_show_a_read_a_a_a_instead_of/][reddit]]) You can write =Show a => Read a => a -> a= instead of =(Show a, Read a) => a -> a=
[[https://ghc.haskell.org/trac/ghc/ticket/9115#comment:3][#9115]]:

/goldfire/
"I will close, but your post made me realize something. Instead of writing =(Eq a, Show a, Read a) => a -> a=, I can write =Eq a => Show a => Read a => a -> a=, which I somehow like more. Haven't checked how it Haddocks, though..."

/ekmett/ 
"goldfire:

Note: the latter is slightly weaker, since in Eq a => Show a => Read a => .. you can only reference backwards up the chain."

https://ghc.haskell.org/trac/ghc/ticket/12087#comment:2
* ([[https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/glasgow_exts.html#constraints-in-kinds][GHC user guide]]) Constraints in kinds
As kinds and types are the same, kinds can now (with -XTypeInType) contain type constraints. Only equality constraints are currently supported, however. We expect this to extend to other constraints in the future.

Here is an example of a constrained kind:

: type family IsTypeLit a where
:   IsTypeLit Nat    = 'True
:   IsTypeLit Symbol = 'True
:   IsTypeLit a      = 'False

: data T :: forall a. (IsTypeLit a ~ 'True) => a -> * where
:   MkNat    :: T 42
:   MkSymbol :: T "Don't panic!"

----

* Terminology 
** Instance generator

From “Simulating Quantified Class Constraints”:

: data Bit = O | I
: 
: class Binary a where
:   showBIn :: a -> [Bin]

“An instance of /Binary/ for lists could be defined as follows:

: instance Binary a => Binary [a] where
:   showBin :: [a] -> [Bit]
:   showBin = concatMap showBin

This instance declaration actually represents an *“instance generator,”* or a proof that the type /[a]/ is an instance of /Binary/ whenever /a/ is; hence the type variable /a/ can be thought of as universally quantified, and the instance declaration as a proof of

: forall a. Binary a => Binary [a]

where the quantification is made explicit.”

*Nota bene*: this is valid syntax:

: instance forall a. Binary a => Binary [a]

In the language of [[https://hackage.haskell.org/package/constraints-0.8/docs/Data-Constraint-Lifting.html][constraints]]:

: instance Lifting Binary [] where
:   lifting :: forall a. Binary a :- Binary [a]
:   lifting = Sub Dict

** ([[https://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/glasgow_exts.html#instance-declarations][GHC user guide]]) Instance declaration

An instance declaration has the form

: instance (assertion1, ..., assertionn) => class type1 ... typem where ...

*** Context
The part before the “=>” is the *context*, [...]

*** Instance head
[...] while the part after the “=>” is the *head* of the *instance declaration*.

*** ([[http://www.diku.dk/~paba/pubs/files/serrano15haskell-paper.pdf][pdf]]) Elaboration
> When the compiler resolves a specific instance of a type class, it checks that the typing is correct, and also generates the corresponding code for the operations in the type class. This second process is called *elaboration*, and is a key reason for the usefulness of type classes.

[[https://ifl2014.github.io/submissions/ifl2014_submission_31.pdf][Type Families and Elaboration]] The search and combination of code performed by the compiler is called *elaboration*.

*** ([[http://www.diku.dk/~paba/pubs/files/serrano15haskell-paper.pdf][pdf]]) Instance improvement
Example from pdf. Given a type class with a functional dependency

: {-# Language InstanceSigs #-}
: 
: class Collection c e | c -> e where
:   empty :: c
:   add   :: e -> c -> c
: 
: instance Collection [a] a where
:   empty :: [a]
:   empty = []
: 
:   add :: a -> [a] -> [a]
:   add = (:)

The functional dependency =... | c -> e= says that /e/ is determined by the type /c/ so taking an instance constraint with an unknown /x/:

: Collection [Int] x

There is only one instance delcaration that can match that =[Int]= (assume no /overlapping instances/) — =Collection [a] a= — and we can deduce the value of the type /x/ (=x ~ Int=: /x/ is equal to =Int=). This is an instance of (*instance*) *improvement* with a dependency of the second argument (/e/) over the first (/c/).