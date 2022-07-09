Instance declarations and resolution[]{#instance-decls} {#instance-resolution}
=======================================================

An instance declaration has the form :

    instance (assertion1, ..., assertionn) => class type1 ... typem where ...

The part before the \"`=>`\" is the *context*, while the part after the
\"`=>`\" is the *head* of the instance declaration.

When GHC tries to resolve, say, the constraint `C Int Bool`, it tries to
match every instance declaration against the constraint, by
instantiating the head of the instance declaration. Consider these
declarations: :

    instance context1 => C Int a     where ...  -- (A)
    instance context2 => C a   Bool  where ...  -- (B)

GHC\'s default behaviour is that *exactly one instance must match the
constraint it is trying to resolve*. For example, the constraint
`C Int Bool` matches instances (A) and (B), and hence would be rejected;
while `C Int Char` matches only (A) and hence (A) is chosen.

Notice that

-   When matching, GHC takes no account of the context of the instance
    declaration (`context1` etc).
-   It is fine for there to be a *potential* of overlap (by including
    both declarations (A) and (B), say); an error is only reported if a
    particular constraint matches more than one.

See also `instance-overlap`{.interpreted-text role="ref"} for flags that
loosen the instance resolution rules.

Relaxed rules for the instance head {#flexible-instance-head}
-----------------------------------

::: {.extension shortdesc="Enable type synonyms in instance heads.
Implied by :extension:`FlexibleInstances`." since="6.8.1"}
TypeSynonymInstances

Allow definition of type class instances for type synonyms.
:::

::: {.extension shortdesc="Enable flexible instances.
Implies :extension:`TypeSynonymInstances`." implies=":extension:`TypeSynonymInstances`" since="6.8.1"}
FlexibleInstances

Allow definition of type class instances with arbitrary nested types in
the instance head.
:::

In Haskell 98 the head of an instance declaration must be of the form
`C (T a1 ... an)`, where `C` is the class, `T` is a data type
constructor, and the `a1 ... an` are distinct type variables. In the
case of multi-parameter type classes, this rule applies to each
parameter of the instance head (Arguably it should be okay if just one
has this form and the others are type variables, but that\'s the rules
at the moment).

GHC relaxes this rule in two ways:

-   With the `TypeSynonymInstances`{.interpreted-text role="extension"}
    extension, instance heads may use type synonyms. As always, using a
    type synonym is just shorthand for writing the RHS of the type
    synonym definition. For example: :

        type Point a = (a,a)
        instance C (Point a) where ...

    is legal. The instance declaration is equivalent to :

        instance C (a,a) where ...

    As always, type synonyms must be fully applied. You cannot, for
    example, write: :

        instance Monad Point where ...

-   The `FlexibleInstances`{.interpreted-text role="extension"}
    extension allows the head of the instance declaration to mention
    arbitrary nested types. For example, this becomes a legal instance
    declaration :

        instance C (Maybe Int) where ...

    See also the [rules on overlap](#instance-overlap).

    The `FlexibleInstances`{.interpreted-text role="extension"}
    extension implies `TypeSynonymInstances`{.interpreted-text
    role="extension"}.

However, the instance declaration must still conform to the rules for
instance termination: see `instance-termination`{.interpreted-text
role="ref"}.

Formal syntax for instance declaration types {#formal-instance-syntax}
--------------------------------------------

The top of an instance declaration only permits very specific forms of
types. To make more precise what forms of types are or are not
permitted, we provide a BNF-style grammar for the tops of instance
declarations below: :

    inst_top ::= 'instance' opt_forall opt_ctxt inst_head opt_where

    opt_forall ::= <empty>
                |  'forall' tv_bndrs '.'

    tv_bndrs ::= <empty>
              |  tv_bndr tv_bndrs

    tv_bndr ::= tyvar
             |  '(' tyvar '::' ctype ')'

    opt_ctxt ::= <empty>
              |  btype '=>'
              |  '(' ctxt ')' '=>'

    ctxt ::= ctype
          |  ctype ',' ctxt

    inst_head ::= '(' inst_head ')'
               |  prefix_cls_tycon arg_types
               |  arg_type infix_cls_tycon arg_type
               |  '(' arg_type infix_cls_tycon arg_type ')' arg_types

    arg_type ::= <empty>
              |  arg_type arg_types

    opt_where ::= <empty>
               |  'where'

Where:

-   `btype` is a type that is not allowed to have an outermost
    `forall`/`=>` unless it is surrounded by parentheses. For example,
    `forall a. a` and `Eq a => a` are not legal `btype`s, but
    `(forall a. a)` and `(Eq a => a)` are legal.
-   `ctype` is a `btype` that has no restrictions on an outermost
    `forall`/`=>`, so `forall a. a` and `Eq a => a` are legal `ctype`s.
-   `arg_type` is a type that is not allowed to have `forall`s or `=>`s
-   `prefix_cls_tycon` is a class type constructor written prefix (e.g.,
    `Show` or `(&&&)`), while `infix_cls_tycon` is a class type
    constructor written infix (e.g., `` `Show ``[ or
    ]{.title-ref}[&&&]{.title-ref}\`).

This is a simplified grammar that does not fully delve into all of the
implementation details of GHC\'s parser (such as the placement of
Haddock comments), but it is sufficient to attain an understanding of
what is syntactically allowed. Some further various observations about
this grammar:

-   Instance declarations are not allowed to be declared with nested
    `forall`s or `=>`s. For example, this would be rejected: :

        instance forall a. forall b. C (Either a b) where ...

    As a result, `inst_top` puts all of its quantification and
    constraints up front with `opt_forall` and `opt_context`.

-   Furthermore, instance declarations types do not permit outermost
    parentheses that surround the `opt_forall` or `opt_ctxt`, if at
    least one of them are used. For example, `instance (forall a. C a)`
    would be rejected, since GHC would treat the `forall` as being
    nested.

    Note that it is acceptable to use parentheses in a `inst_head`. For
    instance, `instance (C a)` is accepted, as is
    `instance forall a. (C a)`.

Relaxed rules for instance contexts {#instance-rules}
-----------------------------------

In Haskell 98, the class constraints in the context of the instance
declaration must be of the form `C a` where `a` is a type variable that
occurs in the head.

The `FlexibleContexts`{.interpreted-text role="extension"} extension
relaxes this rule, as well as relaxing the corresponding rule for type
signatures (see `flexible-contexts`{.interpreted-text role="ref"}).
Specifically, `FlexibleContexts`{.interpreted-text role="extension"},
allows (well-kinded) class constraints of form `(C t1 ... tn)` in the
context of an instance declaration.

Notice that the extension does not affect equality constraints in an
instance context; they are permitted by `TypeFamilies`{.interpreted-text
role="extension"} or `GADTs`{.interpreted-text role="extension"}.

However, the instance declaration must still conform to the rules for
instance termination: see `instance-termination`{.interpreted-text
role="ref"}.

Instance termination rules {#instance-termination}
--------------------------

::: {.extension shortdesc="Enable undecidable instances."}
UndecidableInstances

since

:   6.8.1

Permit definition of instances which may lead to type-checker
non-termination.
:::

Regardless of `FlexibleInstances`{.interpreted-text role="extension"}
and `FlexibleContexts`{.interpreted-text role="extension"}, instance
declarations must conform to some rules that ensure that instance
resolution will terminate. The restrictions can be lifted with
`UndecidableInstances`{.interpreted-text role="extension"} (see
`undecidable-instances`{.interpreted-text role="ref"}).

The rules are these:

1.  The Paterson Conditions: for each class constraint `(C t1 ... tn)`
    in the context
    1.  No type variable has more occurrences in the constraint than in
        the head
    2.  The constraint has fewer constructors and variables (taken
        together and counting repetitions) than the head
    3.  The constraint mentions no type functions. A type function
        application can in principle expand to a type of arbitrary size,
        and so are rejected out of hand
2.  The Coverage Condition. For each functional dependency, ⟨tvs⟩~left~
    `->` ⟨tvs⟩~right~, of the class, every type variable in
    S(⟨tvs⟩~right~) must appear in S(⟨tvs⟩~left~), where S is the
    substitution mapping each type variable in the class declaration to
    the corresponding type in the instance head.

These restrictions ensure that instance resolution terminates: each
reduction step makes the problem smaller by at least one constructor.
You can find lots of background material about the reason for these
restrictions in the paper [Understanding functional dependencies via
Constraint Handling
Rules](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/jfp06.pdf).

For example, these are okay:

    instance C Int [a]          -- Multiple parameters
    instance Eq (S [a])         -- Structured type in head

        -- Repeated type variable in head
    instance C4 a a => C4 [a] [a]
    instance Stateful (ST s) (MutVar s)

        -- Head can consist of type variables only
    instance C a
    instance (Eq a, Show b) => C2 a b

        -- Non-type variables in context
    instance Show (s a) => Show (Sized s a)
    instance C2 Int a => C3 Bool [a]
    instance C2 Int a => C3 [a] b

But these are not:

    -- Context assertion no smaller than head

> instance C a =\> C a where \...
>
> :   \-- (C b b) has more occurrences of b than the head
>
> instance C b b =\> Foo \[b\] where \...

The same restrictions apply to instances generated by `deriving`
clauses. Thus the following is accepted:

    data MinHeap h a = H a (h a)
      deriving (Show)

because the derived instance

    instance (Show a, Show (h a)) => Show (MinHeap h a)

conforms to the above rules.

A useful idiom permitted by the above rules is as follows. If one allows
overlapping instance declarations then it\'s quite convenient to have a
\"default instance\" declaration that applies if something more specific
does not:

    instance C a where
      op = ... -- Default

Undecidable instances
---------------------

::: {.index}
single: -XUndecidableInstances
:::

Sometimes even the termination rules of
`instance-termination`{.interpreted-text role="ref"} are too onerous. So
GHC allows you to experiment with more liberal rules: if you use the
experimental extension `UndecidableInstances`{.interpreted-text
role="extension"}, both the Paterson Conditions and the Coverage
Condition (described in `instance-termination`{.interpreted-text
role="ref"}) are lifted. Termination is still ensured by having a
fixed-depth recursion stack. If you exceed the stack depth you get a
sort of backtrace, and the opportunity to increase the stack depth with
`-freduction-depth=⟨n⟩`. However, if you should exceed the default
reduction depth limit, it is probably best just to disable depth
checking, with `-freduction-depth=0`. The exact depth your program
requires depends on minutiae of your code, and it may change between
minor GHC releases. The safest bet for released code \-- if you\'re sure
that it should compile in finite time \-- is just to disable the check.

For example, sometimes you might want to use the following to get the
effect of a \"class synonym\":

    class (C1 a, C2 a, C3 a) => C a where { }

    instance (C1 a, C2 a, C3 a) => C a where { }

This allows you to write shorter signatures:

    f :: C a => ...

instead of

    f :: (C1 a, C2 a, C3 a) => ...

The restrictions on functional dependencies
(`functional-dependencies`{.interpreted-text role="ref"}) are
particularly troublesome. It is tempting to introduce type variables in
the context that do not appear in the head, something that is excluded
by the normal rules. For example:

    class HasConverter a b | a -> b where
       convert :: a -> b

    data Foo a = MkFoo a

    instance (HasConverter a b,Show b) => Show (Foo a) where
       show (MkFoo value) = show (convert value)

This is dangerous territory, however. Here, for example, is a program
that would make the typechecker loop:

    class D a
    class F a b | a->b
    instance F [a] [[a]]
    instance (D c, F a c) => D [a]   -- 'c' is not mentioned in the head

Similarly, it can be tempting to lift the coverage condition:

    class Mul a b c | a b -> c where
      (.*.) :: a -> b -> c

    instance Mul Int Int Int where (.*.) = (*)
    instance Mul Int Float Float where x .*. y = fromIntegral x * y
    instance Mul a b c => Mul a [b] [c] where x .*. v = map (x.*.) v

The third instance declaration does not obey the coverage condition; and
indeed the (somewhat strange) definition:

    f = \ b x y -> if b then x .*. [y] else y

makes instance inference go into a loop, because it requires the
constraint `(Mul a [b] b)`.

The `UndecidableInstances`{.interpreted-text role="extension"} extension
is also used to lift some of the restrictions imposed on type family
instances. See `type-family-decidability`{.interpreted-text role="ref"}.

Overlapping instances {#instance-overlap}
---------------------

::: {.extension shortdesc="Enable overlapping instances."}
OverlappingInstances

Deprecated extension to weaken checks intended to ensure instance
resolution termination.
:::

::: {.extension shortdesc="Enable incoherent instances.
Implies :extension:`OverlappingInstances`." since="6.8.1"}
IncoherentInstances

Deprecated extension to weaken checks intended to ensure instance
resolution termination.
:::

In general, as discussed in `instance-resolution`{.interpreted-text
role="ref"}, *GHC requires that it be unambiguous which instance
declaration should be used to resolve a type-class constraint*. GHC also
provides a way to loosen the instance resolution, by allowing more than
one instance to match, *provided there is a most specific one*.
Moreover, it can be loosened further, by allowing more than one instance
to match irrespective of whether there is a most specific one. This
section gives the details.

To control the choice of instance, it is possible to specify the overlap
behavior for individual instances with a pragma, written immediately
after the `instance` keyword. The pragma may be one of:
`{-# OVERLAPPING #-}`, `{-# OVERLAPPABLE #-}`, `{-# OVERLAPS #-}`, or
`{-# INCOHERENT #-}`.

The matching behaviour is also influenced by two module-level language
extension flags: `OverlappingInstances`{.interpreted-text
role="extension"} and `IncoherentInstances`{.interpreted-text
role="extension"}. These extensions are now deprecated (since GHC 7.10)
in favour of the fine-grained per-instance pragmas.

A more precise specification is as follows. The willingness to be
overlapped or incoherent is a property of the *instance declaration*
itself, controlled as follows:

-   An instance is *incoherent* if: it has an `INCOHERENT` pragma; or if
    the instance has no pragma and it appears in a module compiled with
    `IncoherentInstances`{.interpreted-text role="extension"}.
-   An instance is *overlappable* if: it has an `OVERLAPPABLE` or
    `OVERLAPS` pragma; or if the instance has no pragma and it appears
    in a module compiled with `OverlappingInstances`{.interpreted-text
    role="extension"}; or if the instance is incoherent.
-   An instance is *overlapping* if: it has an `OVERLAPPING` or
    `OVERLAPS` pragma; or if the instance has no pragma and it appears
    in a module compiled with `OverlappingInstances`{.interpreted-text
    role="extension"}; or if the instance is incoherent.

Now suppose that, in some client module, we are searching for an
instance of the *target constraint* `(C ty1 .. tyn)`. The search works
like this:

-   Find all instances $I$ that *match* the target constraint; that is,
    the target constraint is a substitution instance of $I$. These
    instance declarations are the *candidates*.
-   If no candidates remain, the search fails
-   Eliminate any candidate $IX$ for which there is another candidate
    $IY$ such that both of the following hold:
    -   $IY$ is strictly more specific than $IX$. That is, $IY$ is a
        substitution instance of $IX$ but not vice versa.
    -   Either $IX$ is *overlappable*, or $IY$ is *overlapping*. (This
        \"either/or\" design, rather than a \"both/and\" design, allow a
        client to deliberately override an instance from a library,
        without requiring a change to the library.)
-   If all the remaining candidates are incoherent, the search succeeds,
    returning an arbitrary surviving candidate.
-   If more than one non-incoherent candidate remains, the search fails.
-   Otherwise there is exactly one non-incoherent candidate; call it the
    \"prime candidate\".
-   Now find all instances, or in-scope given constraints, that *unify*
    with the target constraint, but do not *match* it. Such
    non-candidate instances might match when the target constraint is
    further instantiated. If all of them are incoherent top-level
    instances, the search succeeds, returning the prime candidate.
    Otherwise the search fails.

Notice that these rules are not influenced by flag settings in the
client module, where the instances are *used*. These rules make it
possible for a library author to design a library that relies on
overlapping instances without the client having to know.

Errors are reported *lazily* (when attempting to solve a constraint),
rather than *eagerly* (when the instances themselves are defined).
Consider, for example :

    instance C Int  b where ..
    instance C a Bool where ..

These potentially overlap, but GHC will not complain about the instance
declarations themselves, regardless of flag settings. If we later try to
solve the constraint `(C Int Char)` then only the first instance
matches, and all is well. Similarly with `(C Bool Bool)`. But if we try
to solve `(C Int Bool)`, both instances match and an error is reported.

As a more substantial example of the rules in action, consider :

    instance {-# OVERLAPPABLE #-} context1 => C Int b     where ...  -- (A)
    instance {-# OVERLAPPABLE #-} context2 => C a   Bool  where ...  -- (B)
    instance {-# OVERLAPPABLE #-} context3 => C a   [b]   where ...  -- (C)
    instance {-# OVERLAPPING  #-} context4 => C Int [Int] where ...  -- (D)

Now suppose that the type inference engine needs to solve the constraint
`C Int [Int]`. This constraint matches instances (A), (C) and (D), but
the last is more specific, and hence is chosen.

If (D) did not exist then (A) and (C) would still be matched, but
neither is most specific. In that case, the program would be rejected,
unless `IncoherentInstances`{.interpreted-text role="extension"} is
enabled, in which case it would be accepted and (A) or (C) would be
chosen arbitrarily.

An instance declaration is *more specific* than another iff the head of
former is a substitution instance of the latter. For example (D) is
\"more specific\" than (C) because you can get from (C) to (D) by
substituting `a := Int`.

The final bullet (about unifying instances) makes GHC conservative about
committing to an overlapping instance. For example: :

    f :: [b] -> [b]
    f x = ...

Suppose that from the RHS of `f` we get the constraint `C b [b]`. But
GHC does not commit to instance (C), because in a particular call of
`f`, `b` might be instantiated to `Int`, in which case instance (D)
would be more specific still. So GHC rejects the program.

If, however, you enable the extension
`IncoherentInstances`{.interpreted-text role="extension"} when compiling
the module that contains (D), GHC will instead pick (C), without
complaining about the problem of subsequent instantiations.

Notice that we gave a type signature to `f`, so GHC had to *check* that
`f` has the specified type. Suppose instead we do not give a type
signature, asking GHC to *infer* it instead. In this case, GHC will
refrain from simplifying the constraint `C Int [b]` (for the same reason
as before) but, rather than rejecting the program, it will infer the
type :

    f :: C b [b] => [b] -> [b]

That postpones the question of which instance to pick to the call site
for `f` by which time more is known about the type `b`. You can write
this type signature yourself if you use the
`FlexibleContexts`{.interpreted-text role="extension"} extension.

Exactly the same situation can arise in instance declarations
themselves. Suppose we have :

    class Foo a where
       f :: a -> a
    instance Foo [b] where
       f x = ...

and, as before, the constraint `C Int [b]` arises from `f`\'s right hand
side. GHC will reject the instance, complaining as before that it does
not know how to resolve the constraint `C Int [b]`, because it matches
more than one instance declaration. The solution is to postpone the
choice by adding the constraint to the context of the instance
declaration, thus: :

    instance C Int [b] => Foo [b] where
       f x = ...

(You need `FlexibleInstances`{.interpreted-text role="extension"} to do
this.)

In the unification check in the final bullet, GHC also uses the
\"in-scope given constraints\". Consider for example :

    instance C a Int

    g :: forall b c. C b Int => blah
    g = ...needs (C c Int)...

Here GHC will not solve the constraint `(C c Int)` from the top-level
instance, because a particular call of `g` might instantiate both `b`
and `c` to the same type, which would allow the constraint to be solved
in a different way. This latter restriction is principally to make the
constraint-solver complete. (Interested folk can read
`Note [Instance and Given overlap]` in `TcInteract`.) It is easy to
avoid: in a type signature avoid a constraint that matches a top-level
instance. The flag `-Wsimplifiable-class-constraints`{.interpreted-text
role="ghc-flag"} warns about such signatures.

::: {.warning}
::: {.admonition-title}
Warning
:::

Overlapping instances must be used with care. They can give rise to
incoherence (i.e. different instance choices are made in different parts
of the program) even without `IncoherentInstances`{.interpreted-text
role="extension"}. Consider: :

    {-# LANGUAGE OverlappingInstances #-}

module Help where

> class MyShow a where myshow :: a -\> String
>
> instance MyShow a =\> MyShow \[a\] where myshow xs = concatMap myshow
> xs
>
> showHelp :: MyShow a =\> \[a\] -\> String showHelp xs = myshow xs

{-\# LANGUAGE FlexibleInstances, OverlappingInstances \#-} module Main
where import Help

> data T = MkT
>
> instance MyShow T where myshow x = \"Used generic instance\"
>
> instance MyShow \[T\] where myshow xs = \"Used more specific
> instance\"
>
> main = do { print (myshow \[MkT\]); print (showHelp \[MkT\]) }
:::

> In function `showHelp` GHC sees no overlapping instances, and so uses
> the `MyShow [a]` instance without complaint. In the call to `myshow`
> in `main`, GHC resolves the `MyShow [T]` constraint using the
> overlapping instance declaration in module `Main`. As a result, the
> program prints
>
> ``` {.sourceCode .none}
> "Used more specific instance"
> "Used generic instance"
> ```
>
> (An alternative possible behaviour, not currently implemented, would
> be to reject module `Help` on the grounds that a later instance
> declaration might overlap the local one.)

Instance signatures: type signatures in instance declarations {#instance-sigs}
-------------------------------------------------------------

::: {.extension shortdesc="Enable instance signatures."}
InstanceSigs

since

:   7.6.1

Allow type signatures for members in instance definitions.
:::

In Haskell, you can\'t write a type signature in an instance
declaration, but it is sometimes convenient to do so, and the language
extension `InstanceSigs`{.interpreted-text role="extension"} allows you
to do so. For example: :

    data T a = MkT a a
    instance Eq a => Eq (T a) where
      (==) :: T a -> T a -> Bool   -- The signature
      (==) (MkT x1 x2) (MkTy y1 y2) = x1==y1 && x2==y2

Some details

-   The type signature in the instance declaration must be more
    polymorphic than (or the same as) the one in the class declaration,
    instantiated with the instance type. For example, this is fine: :

        instance Eq a => Eq (T a) where
           (==) :: forall b. b -> b -> Bool
           (==) x y = True

    Here the signature in the instance declaration is more polymorphic
    than that required by the instantiated class method.

-   The code for the method in the instance declaration is typechecked
    against the type signature supplied in the instance declaration, as
    you would expect. So if the instance signature is more polymorphic
    than required, the code must be too.
-   One stylistic reason for wanting to write a type signature is simple
    documentation. Another is that you may want to bring scoped type
    variables into scope. For example: :

        class C a where
          foo :: b -> a -> (a, [b])

        instance C a => C (T a) where
          foo :: forall b. b -> T a -> (T a, [b])
          foo x (T y) = (T y, xs)
             where
               xs :: [b]
               xs = [x,x,x]

    Provided that you also specify
    `ScopedTypeVariables`{.interpreted-text role="extension"}
    (`scoped-type-variables`{.interpreted-text role="ref"}), the
    `forall b` scopes over the definition of `foo`, and in particular
    over the type signature for `xs`.
