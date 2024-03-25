# Type families

> The GHC Users Guide has a Type Family section.

**The [GHC Users Guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/) has [a Type Family section](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/type_families.html#).**


Indexed type families, or **type families** for short, are a Haskell extension supporting ad-hoc overloading of data types. Type families are parametric types that can be assigned specialized representations based on the type parameters they are instantiated with. They are the data type analogue of [type classes](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/Type_class "Type class"): families are used to define overloaded _data_ in the same way that classes are used to define overloaded _functions_. Type families are useful for generic programming, for creating highly parameterised library interfaces, and for creating interfaces with enhanced static information, much like dependent types.

Type families come in two flavors: _data families_ and _type synonym families_. Data families are the indexed form of data and newtype definitions. Type synonym families are the indexed form of type synonyms. Each of these flavors can be defined in a standalone manner or _associated_ with a type class. Standalone definitions are more general, while associated types can more clearly express how a type is used and lead to better error messages.


What are type families?
-----------------------

The concept of a type family comes from type theory. An indexed type family in type theory is a partial function at the type level. Applying the function to parameters (called _type indices_) yields a type. Type families permit a program to compute what data constructors it will operate on, rather than having them fixed statically (as with simple type systems) or treated as opaque unknowns (as with parametrically polymorphic types).

Type families are to vanilla data types what type class methods are to regular functions. Vanilla polymorphic data types and functions have a single definition, which is used at all type instances. Classes and type families, on the other hand, have an interface definition and any number of instance definitions. A type family's interface definition declares its [kind](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/Kind "Kind") and its arity, or the number of type indices it takes. Instance definitions define the type family over some part of the domain.

As a simple example of how type families differ from ordinary parametric data types, consider a strict list type. We can represent a list of `Char` in the usual way, with cons cells. We can do the same thing to represent a list of `()`, but since a strict `()` value carries no useful information, it would be more efficient to just store the length of the list. This can't be done with an ordinary parametric data type, because the data constructors used to represent the list would depend on the list's type parameter: if it's `Char` then the list consists of cons cells; if it's `()`, then the list consists of a single integer. We basically want to select between two different data types based on a type parameter. Using type families, this list type could be declared as follows:

\-- Declare a list-like data family
data family XList a

\-- Declare a list-like instance for Char
data instance XList Char \= XCons !Char !(XList Char) | XNil

\-- Declare a number-like instance for ()
data instance XList () \= XListUnit !Int

The right-hand sides of the two `data instance` declarations are exactly ordinary data definitions. In fact, a `data instance` declaration is nothing more than a shorthand for a `data` declaration followed by a `type instance` (see below) declaration. However, they define two instances of the same parametric data type, `XList Char` and `XList ()`, whereas ordinary data declarations define completely unrelated types. A recent [tutorial paper](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/Simonpj/Talk:FunWithTypeFuns "Simonpj/Talk:FunWithTypeFuns") has more in-depth examples of programming with type families.

[GADTs](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/GADT "GADT") bear some similarity to type families, in the sense that they allow a parametric type's constructors to depend on the type's parameters. However, all GADT constructors must be defined in one place, whereas type families can be extended. Functional dependencies are similar to type families, and many type classes that use functional dependencies can be equivalently expressed with type families. Type families provide a more functional style of type-level programming than the relational style of functional dependencies.

Requirements to use type families
---------------------------------

Type families are a GHC extension enabled with the `-XTypeFamilies` flag or the `{-# LANGUAGE TypeFamilies #-}` option. The first stable release of GHC that properly supports type families is 6.10.1. Release 6.8 includes an early partial deprecated implementation.

An associated data type example
-------------------------------

As an example, consider Ralf Hinze's [generalised tries](http://www.cs.ox.ac.uk/ralf.hinze/publications/GGTries.ps.gz), a form of generic finite maps.

### The class declaration

We define a type class whose instances are the types that we can use as keys in our generic maps:

class GMapKey k where
  data GMap k :: \* \-> \*
  empty       :: GMap k v
  lookup      :: k \-> GMap k v \-> Maybe v
  insert      :: k \-> v \-> GMap k v \-> GMap k v

The interesting part is the _associated data family_ declaration of the class. It gives a [_kind signature_](https://downloads.haskell.org/ghc/8.10.3/docs/html/users_guide/glasgow_exts.html#data-families) (here `* -> *`) for the associated data type `GMap k` - analogous to how methods receive a type signature in a class declaration.

What it is important to notice is that the first parameter of the associated type `GMap` coincides with the class parameter of `GMapKey`. This indicates that also in all instances of the class, the instances of the associated data type need to have their first argument match up with the instance type. In general, the type arguments of an associated type can be a subset of the class parameters (in a multi-parameter type class) and they can appear in any order, possibly in an order other than in the class head. The latter can be useful if the associated data type is partially applied in some contexts.

The second important point is that as `GMap k` has kind `* -> *` and `k` (implicitly) has kind `*`, the type constructor `GMap` (without an argument) has kind `* -> * -> *`. Consequently, we see that `GMap` is applied to two arguments in the signatures of the methods `empty`, `lookup`, and `insert`.

### An Int instance

To use Ints as keys into generic maps, we declare an instance that simply uses `Data.IntMap`, thusly:

instance GMapKey Int where
  data GMap Int v        \= GMapInt (Data.IntMap.IntMap v)
  empty                  \= GMapInt Data.IntMap.empty
  lookup k   (GMapInt m) \= Data.IntMap.lookup k m
  insert k v (GMapInt m) \= GMapInt (Data.IntMap.insert k v m)

The `Int` instance of the associated data type `GMap` needs to have both of its parameters, but as only the first one corresponds to a class parameter, the second needs to be a type variable (here `v`). As mentioned before, any associated type parameter that corresponds to a class parameter must be identical to the class parameter in each instance. The right hand side of the associated data declaration is like that of any other data type.

NB: At the moment, GADT syntax is not allowed for associated data types (or other indexed types). This is not a fundamental limitation, but just a shortcoming of the current implementation, which we expect to lift in the future.

As an exercise, implement an instance for `Char` that maps back to the `Int` instance using the conversion functions `Char.ord` and `Char.chr`.

### A unit instance

Generic definitions, apart from elementary types, typically cover units, products, and sums. We start here with the unit instance for `GMap`:

instance GMapKey () where
  data GMap () v           \= GMapUnit (Maybe v)
  empty                    \= GMapUnit Nothing
  lookup () (GMapUnit v)   \= v
  insert () v (GMapUnit \_) \= GMapUnit $ Just v

For unit, the map is just a `Maybe` value.

### Product and sum instances

Next, let us define the instances for pairs and sums (i.e., `Either`):

instance (GMapKey a, GMapKey b) \=> GMapKey (a, b) where
  data GMap (a, b) v            \= GMapPair (GMap a (GMap b v))
  empty		                \= GMapPair empty
  lookup (a, b) (GMapPair gm)   \= lookup a gm \>>= lookup b 
  insert (a, b) v (GMapPair gm) \= GMapPair $ case lookup a gm of
				    Nothing  \-> insert a (insert b v empty) gm
				    Just gm2 \-> insert a (insert b v gm2  ) gm

instance (GMapKey a, GMapKey b) \=> GMapKey (Either a b) where
  data GMap (Either a b) v                \= GMapEither (GMap a v) (GMap b v)
  empty                                   \= GMapEither empty empty
  lookup (Left  a) (GMapEither gm1  \_gm2) \= lookup a gm1
  lookup (Right b) (GMapEither \_gm1 gm2 ) \= lookup b gm2
  insert (Left  a) v (GMapEither gm1 gm2) \= GMapEither (insert a v gm1) gm2
  insert (Right b) v (GMapEither gm1 gm2) \= GMapEither gm1 (insert b v gm2)

If you find this code algorithmically surprising, I'd suggest to have a look at [Ralf Hinze's paper](http://www.cs.ox.ac.uk/ralf.hinze/publications/index.html#J4). The only novelty concerning associated types, in these two instances, is that the instances have a context `(GMapKey a, GMapKey b)`. Consequently, the right hand sides of the associated type declarations can use `GMap` recursively at the key types `a` and `b` - not unlike the method definitions use the class methods recursively at the types for which the class is given in the instance context.

### Using a generic map

Finally, some code building and querying a generic map:

myGMap :: GMap (Int, Either Char ()) String
myGMap \= insert (5, Left 'c') "(5, Left 'c')"    $
	 insert (4, Right ()) "(4, Right ())"    $
	 insert (5, Right ()) "This is the one!" $
	 insert (5, Right ()) "This is the two!" $
	 insert (6, Right ()) "(6, Right ())"    $
	 insert (5, Left 'a') "(5, Left 'a')"    $
	 empty
main \= putStrLn $ maybe "Couldn't find key!" id $ lookup (5, Right ()) myGMap

### Download the code

If you want to play with this example without copying it off the wiki, just download the source code[\[1\]](https://gitlab.haskell.org/ghc/ghc/-/blob/ddbdec4128f0e6760c8c7a19344f2f2a7a3314bf/testsuite/tests/indexed-types/should_run/GMapAssoc.hs) for `GMap` from GHC's test suite.

Detailed definition of data families
------------------------------------

Data families appear in two flavours: (1) they can be defined on the toplevel or (2) they can appear inside type classes (in which case they are known as associated types). The former is the more general variant, as it lacks the requirement for the type-indices to coincide with the class parameters. However, the latter can lead to more clearly structured code and compiler warnings if some type instances were - possibly accidentally - omitted. In the following, we always discuss the general toplevel form first and then cover the additional constraints placed on associated types.

### Family declarations

Indexed data families are introduced by a signature, such as

data family GMap k :: \* \-> \*

The special `family` distinguishes family from standard data declarations. The result kind annotation is optional and, as usual, defaults to `*` if omitted. An example is

Named arguments can also be given explicit kind signatures if needed. Just as with [GADT declarations](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#generalised-algebraic-data-types-gadts) named arguments are entirely optional, so that we can declare `Array` alternatively with

data family Array :: \* \-> \*

#### Associated family declarations

When a data family is declared as part of a type class, we drop the `family` keyword. The `GMap` declaration takes the following form

class GMapKey k where
  data GMap k :: \* \-> \*
  ...

In contrast to toplevel declarations, named arguments must be used for all type parameters that are to be used as type-indices. Moreover, the argument names must be class parameters. Each class parameter may only be used at most once per associated type, but some may be omitted and they may be in an order other than in the class head. In other words: **the named type parameters of the data declaration must be a permutation of a subset of the class variables**.

Example is admissible:

class C a b c where { data T c a :: \* }  \-- OK
class C a b c where { data T a a :: \* }  \-- Bad: repeated variable
class D a where { data T a x :: \* }      \-- Bad: x is not a class variable
class D a where { data T a :: \* \-> \* }   \-- OK

### Instance declarations

Instance declarations of data and newtype families are very similar to standard data and newtype declarations. The only two differences are that the keyword `data` or `newtype` is followed by `instance` and that some or all of the type arguments can be non-variable types, but may not contain forall types or type synonym families. However, data families are generally allowed in type parameters, and type synonyms are allowed as long as they are fully applied and expand to a type that is itself admissible - exactly as this is required for occurrences of type synonyms in class instance parameters. For example, the `Either` instance for `GMap` is

data instance GMap (Either a b) v \= GMapEither (GMap a v) (GMap b v)

In this example, the declaration has only one variant. In general, it can be any number.

Data and newtype instance declarations are only legit when an appropriate family declaration is in scope - just like class instances require the class declaration to be visible. Moreover, each instance declaration has to conform to the kind determined by its family declaration. This implies that the number of parameters of an instance declaration matches the arity determined by the kind of the family. Although all data families are declared with the `data` keyword, instances can be either `data` or `newtype`s, or a mix of both.

Even if type families are defined as toplevel declarations, functions that perform different computations for different family instances still need to be defined as methods of type classes. In particular, the following is not possible:

data family T a
data instance T Int  \= A
data instance T Char \= B
nonsense :: T a \-> Int
nonsense A \= 1             \-- WRONG: These two equations together...
nonsense B \= 2             \-- ...will produce a type error.

Given the functionality provided by GADTs (Generalised Algebraic Data Types), it might seem as if a definition, such as the above, should be feasible. However, type families - in contrast to GADTs - are _open_; i.e., new instances can always be added, possibly in other modules. Supporting pattern matching across different data instances would require a form of extensible case construct.

#### Associated type instances

When an associated family instance is declared within a type class instance, we drop the `instance` keyword in the family instance. So, the `Either` instance for `GMap` becomes:

instance (GMapKey a, GMapKey b) \=> GMapKey (Either a b) where
  data GMap (Either a b) v \= GMapEither (GMap a v) (GMap b v)
  ...

The most important point about associated family instances is that the type indices corresponding to class parameters must be identical to the type given in the instance head; here this is the first argument of `GMap`, namely `Either a b`, which coincides with the only class parameter. Any parameters to the family constructor that do not correspond to class parameters, need to be variables in every instance; here this is the variable `v`.

Instances for an associated family can only appear as part of instance declarations of the class in which the family was declared - just as with the equations of the methods of a class. Also in correspondence to how methods are handled, declarations of associated types can be omitted in class instances. If an associated family instance is omitted, the corresponding instance type is not inhabited; i.e., only diverging expressions, such as `undefined`, can assume the type.

#### Scoping of class parameters

In the case of multi-parameter type classes, the visibility of class parameters in the right-hand side of associated family instances depends _solely_ on the parameters of the data family. As an example, consider the simple class declaration

class C a b where
  data T a

Only one of the two class parameters is a parameter to the data family. Hence, the following instance declaration is invalid:

instance C \[c\] d where
  data T \[c\] \= MkT (c, d)    \-- WRONG!!  'd' is not in scope

Here, the right-hand side of the data instance mentions the type variable `d` that does not occur in its left-hand side. We cannot admit such data instances as they would compromise type safety.

#### Type class instances of family instances

Type class instances of instances of data families can be defined as usual, and in particular data instance declarations can have `deriving` clauses. For example, we can write

data GMap () v \= GMapUnit (Maybe v)
               deriving Show

which implicitly defines an instance of the form

instance Show v \=> Show (GMap () v) where ...

Note that class instances are always for particular _instances_ of a data family and never for an entire family as a whole. This is for essentially the same reasons that we cannot define a toplevel function that performs pattern matching on the data constructors of _different_ instances of a single type family. It would require a form of extensible case construct.

#### Overlap

The instance declarations of a data family used in a single program may not overlap at all, independent of whether they are associated or not. In contrast to type class instances, this is not only a matter of consistency, but one of type safety.

### Import and export

The association of data constructors with type families is more dynamic than that is the case with standard data and newtype declarations. In the standard case, the notation `T(..)` in an import or export list denotes the type constructor and all the data constructors introduced in its declaration. However, a family declaration never introduces any data constructors; instead, data constructors are introduced by family instances. As a result, which data constructors are associated with a type family depends on the currently visible instance declarations for that family. Consequently, an import or export item of the form `T(..)` denotes the family constructor and all currently visible data constructors - in the case of an export item, these may be either imported or defined in the current module. The treatment of import and export items that explicitly list data constructors, such as `GMap(GMapEither)`, is analogous.

#### Associated families

As expected, an import or export item of the form `C(..)` denotes all of the class' methods and associated types. However, when associated types are explicitly listed as subitems of a class, we need some new syntax, as uppercase identifiers as subitems are usually data constructors, not type constructors. To clarify that we denote types here, each associated type name needs to be prefixed by the keyword `type`. So for example, when explicitly listing the components of the `GMapKey` class, we write `GMapKey(type GMap, empty, lookup, insert)`.

#### Examples

Assuming our running `GMapKey` class example, let us look at some export lists and their meaning:

*   `module GMap (GMapKey) where...`: Exports just the class name.
*   `module GMap (GMapKey(..)) where...`: Exports the class, the associated type `GMap` and the member functions `empty`, `lookup`, and `insert`. None of the data constructors is exported.
*   `module GMap (GMapKey(..), GMap(..)) where...`: As before, but also exports all the data constructors `GMapInt`, `GMapChar`, `GMapUnit`, `GMapPair`, and `GMapEither`.
*   `module GMap (GMapKey(empty, lookup, insert), GMap(..)) where...`: As before.
*   `module GMap (GMapKey, empty, lookup, insert, GMap(..)) where...`: As before.

Finally, you can write `GMapKey(type GMap)` to denote both the class `GMapKey` as well as its associated type `GMap`. However, you cannot write `GMapKey(type GMap(..))` — i.e., sub-component specifications cannot be nested. To specify `GMap`'s data constructors, you have to list it separately.

#### Instances

Family instances are implicitly exported, just like class instances. However, this applies only to the heads of instances, not to the data constructors an instance defines.

An associated type synonym example
----------------------------------

Type synonym families are an alternative to functional dependencies, which makes functional dependency examples well suited to introduce type synonym families. In fact, type families are a more functional way to express the same as functional dependencies (despite the name!), as they replace the relational notation of functional dependencies by an expression-oriented notation; i.e., functions on types are really represented by functions and not relations.

### The `class` declaration

Here's an example from Mark Jones' seminal paper on functional dependencies:

class Collects e ce | ce \-> e where
  empty  :: ce
  insert :: e \-> ce \-> ce
  member :: e \-> ce \-> Bool
  toList :: ce \-> \[e\]

With associated type synonyms we can write this as

class Collects ce where
  type Elem ce
  empty  :: ce
  insert :: Elem ce \-> ce \-> ce
  member :: Elem ce \-> ce \-> Bool
  toList :: ce \-> \[Elem ce\]

Instead of the multi-parameter type class, we use a single parameter class, and the parameter `e` turned into an associated type synonym `Elem ce`.

### An `instance`

Instances change correspondingly. An instance of the two-parameter class

instance Eq e \=> Collects e \[e\] where
  empty           \= \[\]
  insert e l      \= (e:l)
  member e \[\]     \= False
  member e (x:xs) 
    | e \== x      \= True
    | otherwise   \= member e xs
  toList l        \= l

becomes an instance of a single-parameter class, where the dependent type parameter turns into an associated type instance declaration:

instance Eq e \=> Collects \[e\] where
  type Elem \[e\]   \= e
  empty           \= \[\]
  insert e l      \= (e:l)
  member e \[\]     \= False
  member e (x:xs) 
    | e \== x      \= True
    | otherwise   \= member e xs
  toList l        \= l

### Using generic collections

With Functional Dependencies the code would be:

sumCollects :: (Collects e c1, Collects e c2) \=> c1 \-> c2 \-> c2
sumCollects c1 c2 \= foldr insert c2 (toList c1)

In contrast, with associated type synonyms, we get:

sumCollects :: (Collects c1, Collects c2, Elem c1 ~ Elem c2) \=> c1 \-> c2 \-> c2
sumCollects c1 c2 \= foldr insert c2 (toList c1)

Detailed definition of type synonym families
--------------------------------------------

Type families appear in two flavours: (1) they can be defined on the toplevel or (2) they can appear inside type classes (in which case they are known as associated type synonyms). The former is the more general variant, as it lacks the requirement for the type-indices to coincide with the class parameters. However, the latter can lead to more clearly structured code and compiler warnings if some type instances were - possibly accidentally - omitted. In the following, we always discuss the general toplevel form first and then cover the additional constraints placed on associated types.

### Family declarations

Indexed type families are introduced by a signature, such as

The special `family` distinguishes family from standard type declarations. The result kind annotation is optional and, as usual, defaults to `*` if omitted. An example is

Parameters can also be given explicit kind signatures if needed. We call the number of parameters in a type family declaration, the family's arity, and all applications of a type family must be fully saturated w.r.t. to that arity. This requirement is unlike ordinary type synonyms and it implies that the kind of a type family is not sufficient to determine a family's arity, and hence in general, also insufficient to determine whether a type family application is well formed. As an example, consider the following declaration:

type family F a b :: \* \-> \*   \-- F's arity is 2, 
                              \-- although its overall kind is \* -> \* -> \* -> \*

Given this declaration the following are examples of well-formed and malformed types:

F Char \[Int\]       \-- OK!  Kind: \* -> \*
F Char \[Int\] Bool  \-- OK!  Kind: \*
F IO Bool          \-- WRONG: kind mismatch in the first argument
F Bool             \-- WRONG: unsaturated application

A top-level type family can be declared as open or closed. (Associated type families are always open.) A closed type family has all of its equations defined in one place and cannot be extended, whereas an open family can have instances spread across modules. The advantage of a closed family is that its equations are tried in order, similar to a term-level function definition:

type family G a where
  G Int \= Bool
  G a   \= Char

With this definition, the type `G Int` becomes `Bool` and, say, `G Double` becomes `Char`. See also [here](https://gitlab.haskell.org/ghc/ghc/-/wikis/new-axioms) for more information about closed type families.

#### Associated family declarations

When a type family is declared as part of a type class, we drop the `family` special. The `Elem` declaration takes the following form

class Collects ce where
  type Elem ce :: \*
  ...

Exactly as in the case of an associated data declaration, **the named type parameters must be a permutation of a subset of the class parameters**. Examples

class C a b c where { type T c a :: \* }   \-- OK
class D a where { type T a x :: \* }       \-- No: x is not a class parameter
class D a where { type T a :: \* \-> \* }    \-- OK

### Type instance declarations

Instance declarations of open type families are very similar to standard type synonym declarations. The only two differences are that the keyword `type` is followed by `instance` and that some or all of the type arguments can be non-variable types, but may not contain forall types or type synonym families. However, data families are generally allowed, and type synonyms are allowed as long as they are fully applied and expand to a type that is admissible - these are the exact same requirements as for data instances. For example, the `[e]` instance for `Elem` is

type instance Elem \[e\] \= e

A type family instance declaration must satisfy the following rules:

*   An appropriate family declaration is in scope - just like class instances require the class declaration to be visible.
*   The instance declaration conforms to the kind determined by its family declaration
*   The number of type parameters in an instance declaration matches the number of type parameters in the family declaration.
*   The right-hand side of a type instance must be a monotype (i.e., it may not include foralls) and after the expansion of all saturated vanilla type synonyms, no synonyms, except family synonyms may remain.

Here are some examples of admissible and illegal type instances and closed families:

type family F a :: \*
type instance F \[Int\]              \= Int         \-- OK!
type instance F String             \= Char        \-- OK!
type instance F (F a)              \= a           \-- WRONG: type parameter mentions a type family
type instance F (forall a. (a, b)) \= b           \-- WRONG: a forall type appears in a type parameter
type instance F Float              \= forall a.a  \-- WRONG: right-hand side may not be a forall type

type family F2 a where                           \-- OK!
  F2 (Maybe Int)  \= Int
  F2 (Maybe Bool) \= Bool
  F2 (Maybe a)    \= String

type family G a b :: \* \-> \*
type instance G Int            \= (,)     \-- WRONG: must be two type parameters
type instance G Int Char Float \= Double  \-- WRONG: must be two type parameters

#### Closed family simplification

Included in ghc starting 7.8.1.

When dealing with closed families, simplifying the type is harder than just finding a left-hand side that matches and replacing that with a right-hand side. GHC will select an equation to use in a given type family application (the "target") if and only if the following 2 conditions hold:

1.  There is a substitution from the variables in the equation's LHS that makes the left-hand side of the branch coincide with the target.
2.  For each previous equation in the family: either the LHS of that equation is _apart_ from the type family application, **or** the equation is _compatible_ with the chosen equation.

Now, we define _apart_ and _compatible_:

1.  Two types are _apart_ when one cannot simplify to the other, even after arbitrary type-family simplifications
2.  Two equations are _compatible_ if, either, their LHSs are apart or their LHSs unify and their RHSs are the same under the substitution induced by the unification.

Some examples are in order:

type family F a where
  F Int  \= Bool
  F Bool \= Char
  F a    \= Bool

type family And (a :: Bool) (b :: Bool) :: Bool where
  And False c     \= False
  And True  d     \= d
  And e     False \= False
  And f     True  \= f
  And g     g     \= g

In `F`, all pairs of equations are compatible except the second and third. The first two are compatible because their LHSs are apart. The first and third are compatible because the unifying substitution leads the RHSs to be the same. But, the second and third are not compatible because neither of these conditions holds. As a result, GHC will not use the third equation to simplify a target unless that target is apart from `Bool`.

In `And`, _every_ pair of equations is compatible, meaning GHC never has to make the extra apartness check during simplification.

Why do all of this? It's a matter of type safety. Consider this example:

type family J a b where
  J a a \= Int
  J a b \= Bool

Say GHC selected the second branch just because the first doesn't apply at the moment, because two type variables are distinct. The problem is that those variables might later be instantiated at the same value, and then the first branch would have applied. You can convince this sort of inconsistency to produce `unsafeCoerce`.

It gets worse. GHC has no internal notion of inequality, so it can't use previous, failed term-level GADT pattern matches to refine its type assumptions. For example:

data G :: \* \-> \* where
 GInt  :: G Int
 GBool :: G Bool

type family Foo (a :: \*) :: \* where
 Foo Int \= Char
 Foo a   \= Double

bar :: G a \-> Foo a
bar GInt  \= 'x'
bar \_     \= 3.14

The last line will fail to typecheck, because GHC doesn't know that the type variable `a` can't be `Int` here, even though it's obvious. The only general way to fix this is to have inequality evidence introduced into GHC, and that's a big deal and we don't know if we have the motivation for such a change yet.

#### Associated type instances

When an associated family instance is declared within a type class instance, we drop the `instance` keyword in the family instance. So, the `[e]` instance for `Elem` becomes:

instance (Eq (Elem \[e\])) \=> Collects (\[e\]) where
  type Elem \[e\] \= e
  ...

The most important point about associated family instances is that the type indexes corresponding to class parameters must be identical to the type given in the instance head; here this is `[e]`, which coincides with the only class parameter.

Instances for an associated family can only appear as part of instance declarations of the class in which the family was declared - just as with the equations of the methods of a class. Also in correspondence to how methods are handled, declarations of associated types can be omitted in class instances. If an associated family instance is omitted, the corresponding instance type is not inhabited; i.e., only diverging expressions, such as `undefined`, can assume the type.

#### Overlap

The instance declarations of an open type family used in a single program must be compatible, in the form defined above. This condition is independent of whether the type family is associated or not, and it is not only a matter of consistency, but one of type safety.

Here are two examples to illustrate the condition under which overlap is permitted.

type instance F (a, Int) \= \[a\]
type instance F (Int, b) \= \[b\]   \-- overlap permitted

type instance G (a, Int)  \= \[a\]
type instance G (Char, a) \= \[a\]  \-- ILLEGAL overlap, as \[Char\] /= \[Int\]

#### Decidability

In order to guarantee that type inference in the presence of type families is decidable, we need to place a number of additional restrictions on the formation of type instance declarations (c.f., Definition 5 (Relaxed Conditions) of [Type Checking with Open Type Functions](http://www.cse.unsw.edu.au/~chak/papers/SPCS08.html)). Instance declarations have the general form

type instance F t1 .. tn \= t

where we require that for every type family application `(G s1 .. sm)` in `t`,

1.  `s1 .. sm` do not contain any type family constructors,
2.  the total number of symbols (data type constructors and type variables) in `s1 .. sm` is strictly smaller than in `t1 .. tn`, and
3.  for every type variable `a`, `a` occurs in `s1 .. sm` at most as often as in `t1 .. tn`.

These restrictions are easily verified and ensure termination of type inference. However, they are not sufficient to guarantee completeness of type inference in the presence of, so called, _loopy equalities_, such as `a ~ [F a]`, where a recursive occurrence of a type variable is underneath a family application and data constructor application - see the above mentioned paper for details.

If the option \-XUndecidableInstances is passed to the compiler, the above restrictions are not enforced and it is on the programmer to ensure termination of the normalisation of type families during type inference.

### Equality constraints

Type context can include equality constraints of the form `t1 ~ t2`, which denote that the types `t1` and `t2` need to be the same. In the presence of type families, whether two types are equal cannot generally be decided locally. Hence, the contexts of function signatures may include equality constraints, as in the following example:

sumCollects :: (Collects c1, Collects c2, Elem c1 ~ Elem c2) \=> c1 \-> c2 \-> c2

where we require that the element type of `c1` and `c2` are the same. In general, the types `t1` and `t2` of an equality constraint may be arbitrary monotypes; i.e., they may not contain any quantifiers, independent of whether higher-rank types are otherwise enabled.

Equality constraints can also appear in class and instance contexts. The former enable a simple translation of programs using functional dependencies into programs using family synonyms instead. The general idea is to rewrite a class declaration of the form

to

class (F a ~ b) \=> C a b where
  type F a

That is, we represent every functional dependency (FD) `a1 .. an -> b` by an FD type family `F a1 .. an` and a superclass context equality `F a1 .. an ~ b`, essentially giving a name to the functional dependency. In class instances, we define the type instances of FD families in accordance with the class head. Method signatures are not affected by that process.

Frequently asked questions
--------------------------

### Comparing type families and functional dependencies

Functional dependencies cover some of the same territory as type families. How do the two compare? There are some articles about this question:

*   Experiences in converting functional dependencies to type families: "[Functional dependencies vs. type families](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/Functional_dependencies_vs._type_families "Functional dependencies vs. type families")"
*   [GHC trac](https://ghc.haskell.org/trac/ghc/wiki/TFvsFD) on a comparison of functional dependencies and type families

### Injectivity, type inference, and ambiguity

A common problem is if:

type family F a

f :: F a \-> F a
f \= undefined

g :: F Int \-> F Int
g x \= f x

Then compiler complains about the definition of g:

``Couldn't match expected type `F Int' against inferred type `F a1'.``

In type-checking g's right hand side GHC discovers (by instantiating f's type with a fresh type variable) that it has type F a1 -> F a1 for some as-yet-unknown type a1. Now it tries to make the inferred type match g's type signature. Well, you say, just make a1 equal to Int and you are done. True, but what if there were these instances

type instance F Int \= Bool
type instance F Char \= Bool

Then making a1 equal to Char would _also_ make the two types equal. Because there is (potentially) more than one choice, the program is rejected.

However (and confusingly) if you omit the type signature on g altogether, thus

f :: F a \-> F a
f \= undefined

g x \= f x

GHC will happily infer the type g :: F a -> F a. But you can't _write_ that type signature or, indeed, the more specific one above. (Arguably this behaviour, where GHC _infers_ a type it can't _check_, is very confusing. I suppose we could make GHC reject both programs, with and without type signatures.)

**What is the problem?** The nub of the issue is this: knowing that F t1\=F t2 does _not_ imply that t1 = t2. The difficulty is that the type function F need not be _injective_; it can map two distinct types to the same type. For an injective type constructor like Maybe, if we know that Maybe t1 = Maybe t2, then we know that t1 = t2. But not so for non-injective type functions.

The problem starts with f. Its type is _ambiguous_; even if I know the argument and result types for f, I cannot use that to find the type at which a should be instantiated. (So arguably, f should be rejected as having an ambiguous type, and probably will be in future.) The situation is well known in type classes:

bad :: (Read a, Show a) \=> String \-> String
bad x \= show (read x)

At a call of bad one cannot tell at what type a should be instantiated.

The only solution is to avoid ambiguous types. In the type signature of a function,

*   Ensure that every type variable occurs in the part after the "\=>"
*   Ensure that every type variable appears at least once outside a type function call.

Alternatively, you can use data families, which create new types and are therefore injective. The following code works:

data family F a
 
f :: F a \-> F a
f \= undefined
 
g :: F Int \-> F Int
g x \= f x

References
----------

*   [Associated Types with Class.](http://www.cse.unsw.edu.au/~chak/papers/CKPM05.html) Manuel M. T. Chakravarty, Gabriele Keller, Simon Peyton Jones, and Simon Marlow. In _Proceedings of The 32nd Annual ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages (POPL'05)_, pages 1-13, ACM Press, 2005.
*   [Associated Type Synonyms.](http://www.cse.unsw.edu.au/~chak/papers/CKP05.html) Manuel M. T. Chakravarty, Gabriele Keller, and Simon Peyton Jones. In _Proceedings of The Tenth ACM SIGPLAN International Conference on Functional Programming_, ACM Press, pages 241-253, 2005.
*   [System F with Type Equality Coercions.](http://www.cse.unsw.edu.au/~chak/papers/SCPD07.html) Martin Sulzmann, Manuel M. T. Chakravarty, Simon Peyton Jones, and Kevin Donnelly. In _Proceedings of The Third ACM SIGPLAN Workshop on Types in Language Design and Implementation_, ACM Press, 2007.
*   [Type Checking With Open Type Functions.](http://www.cse.unsw.edu.au/~chak/papers/SPCS08.html) Tom Schrijvers, Simon Peyton-Jones, Manuel M. T. Chakravarty, Martin Sulzmann. In _Proceedings of The 13th ACM SIGPLAN International Conference on Functional Programming_, ACM Press, pages 51-62, 2008.
*   [Fun with Type Functions](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/Simonpj/Talk:FunWithTypeFuns "Simonpj/Talk:FunWithTypeFuns") Oleg Kiselyov, Simon Peyton Jones, Chung-chieh Shan (the source for this paper can be found at [http://patch-tag.com/r/schoenfinkel/typefunctions/wiki](http://patch-tag.com/r/schoenfinkel/typefunctions/wiki) )


[Source](https://wiki.haskell.org/GHC/Type_families)
