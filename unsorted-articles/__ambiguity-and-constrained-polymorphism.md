# Ambiguity and constrained polymorphism

> This paper considers the problem of ambiguity in Haskell-like languages. Overloading resolution is characterized in the context of constrained polymor…

1\. Introduction
----------------

This paper considers the problem of ambiguity in the context of constrained polymorphism.

We use _constrained polymorphism_ to refer to the polymorphism originated by the combination of parametric polymorphism and context-dependent overloading.

Context-dependent overloading is characterized by the fact that overloading resolution in expressions (function calls) ee′ is based not only on the types of the function (_e_) and the argument (e′), but also on the context in which the expression (ee′) occurs. As result of this, constants can also be overloaded — for example, literals (like 1, 2 etc.) can be used to represent fixed and arbitrary precision integers as well as fractional numbers (for instance, they can be used in expressions such as 1 + 2.0) — and functions with types that differ only on the type of the result (for example, _read_ functions can be overloaded, of types _String_ → _Bool_, _String_ → _Int_ etc., each taking a string and generating the denoted value in the corresponding type). In this way, context-dependent overloading allows overloading to have a less restrictive and more prominent role in the presence of parametric polymorphism, as explored mainly in the programming language Haskell.

Ambiguity is however a major concern in context-dependent overloading. The usual meaning of an _ambiguous expression_ is, informally, an expression that has more than one meaning, or an expression that can be interpreted in two or more distinct ways.

A formalization of this, with respect to a language semantics definition by means of type derivations, defines that an expression _e_ is ambiguous if there exist two or more type derivations that give the same type and may assign distinct semantics values to _e_ (in the following, Γ⊢e:σ specifies that type _σ_ is derivable for expression _e_ in typing context Γ, using the axioms and rules of the type system; 〚Γ⊢e:σ〛 denotes the semantic value obtained by using such axioms and rules):

**Definition 1** Standard Ambiguity

An expression _e_ is called _ambiguous_ if there exist derivations Δ and Δ′ of 〚Γ⊢e:σ〛 and of 〚Γ′⊢e:σ〛, respectively, such that 〚Γ⊢e:σ〛≠〚Γ′⊢e:σ〛, where Γ and Γ′ give the same type to every _x_ free in _e_.

This is equivalent to defining that an expression _e_ is ambiguous if it prevents the definition of a coherent semantics to _e_ [\[1, page 286\]](#br0010), that is, a semantics defined by induction on the structure of expressions where the semantic value assigned to a well-typed expression is not independent of the type derivation.

Without an explicit reference to a distinct definition, ambiguous refers to the standard definition above.

Detection of ambiguity is usually done at compile-time, by the compiler type analysis phase — in Haskell, by the type inference algorithm. Unfortunately, however, detection of ambiguity can not be based on type system definitions, at least for usual definitions, that allow context-free type instantiations, that is, type instantiations that can be done independently of the context where an expression occurs. This causes a well-known incompleteness problem for usual definitions of Haskell type systems [\[2\]](#br0020), [\[3\]](#br0030), [\[4\]](#br0040). This problem is not the focus of this paper.

This paper concentrates instead on another issue related to ambiguity in Haskell, which has not received attention in the technical literature, namely the relation between ambiguity and overloading resolution in the context of constrained polymorphism, in particular the fact that the possibility of inserting new (instance) definitions disregards that an expression may be disambiguated by occurring in some context where there exists a single instance which can be used to instantiate type variables that do not occur in the simple type component of the constrained type.

Specifically, our contributions are:

•

A precise characterization of overloading resolution and ambiguity.

•

Discussion of Haskell's open-world definition of ambiguity and proposal of a new definition, called delayed-closure ambiguity, that is distinguished from overloading resolution: in the open-world approach, ambiguity is a syntactic property of a type, not distinguished from overloading resolution, whereas with delayed-closure this syntactic property (existence of unreachable variables in constraints) characterizes overloading resolution, and ambiguity is a property depending on the context where the relevant expression occurs, namely the existence of two or more instances that entail the constraint with unreachable variables. Ambiguity is tested only after overloading resolution.

In Section [2](#se0020) we present Haskell's definition of ambiguity, called open-world ambiguity. In Section [3](#se0030) we compare open-world ambiguity with the standard, semantical notion of ambiguity.

Substitutions, denoted by meta-variable _ϕ_, possibly primed or subscripted, are used throughout the paper. A substitution denotes a function from type variables to simple type expressions. _ϕ σ_ and ϕ(σ) denote the capture-free operation of substituting ϕ(α) for each free occurrence of type variable _α_ in _σ_, and analogously for the application of substitutions to constraints, sets of types and sets of constraints.

Symbol ∘ denotes function composition, and dom(ϕ)\={α|ϕ(α)≠α} and _id_ denotes the identity substitution. The restriction ϕ|V of _ϕ_ to _V_ denotes the substitution ϕ′ such that ϕ′(α)\=ϕ(α) if α∈V, otherwise _α_.

A substitution _ϕ_ is more general than another ϕ′, written ϕ≤ϕ′, if there exists ϕ1 such that ϕ\=ϕ1∘ϕ′.

Section [4](#se0040) presents an alternative definition of ambiguity, called _delayed-closure_ ambiguity, that specifies essentially that:

1.

Ambiguity should be checked when (and only when) overloading is resolved. We identify that overloading is resolved in a constraint on the type of an expression by the presence of unreachable variables in this constraint (overloading resolution is defined formally in Section [2](#se0020)). A type variable that occurs in a constraint is called reachable if it occurs in the simple type or in a constraint where another reachable type variable occurs, otherwise unreachable.

This is unlike open-world ambiguity, where the existence of any type variable that does not occur in the simple type component of a constrained type implies, in the absence of functional dependencies (see below), ambiguity. For example, type Collce⇒c of an _empty_ member of a class Collce, is considered ambiguous in Haskell, since type variable _e_ does not occur in the simple type component of the constrained type Collce⇒c (despite being reachable). In delayed-closure ambiguity, types with only reachable type variables are not checked for ambiguity, since overloading is still unresolved and may be resolved later, depending on a program context in which it occurs.

2.

Constraints with unreachable type variables may be removed if there exists only a single satisfying substitution that can be used to instantiate the unreachable type variables.

An important observation is that such constraints, removed by the existence of a single satisfying substitution, become ambiguous by the addition of further instances if a satisfying substitution exists for a removed constraint with respect to the instances that have been added.

The specification of defaults, as proposed in subsection [4.2](#se0060), allows programmers to avoid types to become ambiguous by the addition of further instances.

Section [5](#se0070) contains a description of constraint set satisfiability, focusing on issues related to decidability. Section [6](#se0100) presents a type system for a core-Haskell language that adopts delayed-closure ambiguity. Section [7](#se0110) presents a type inference algorithm for core-Haskell and discusses soundness and completeness of the type inference algorithm with respect to the type system. Section [8](#se0120) presents a standard dictionary-style semantics for core Haskell. Section [9](#se0130) discusses related work and Section [10](#se0140) summarizes our conclusions.

2\. Open-world ambiguity
------------------------

The support of overloading in Haskell is based on the definition of _type classes_. A type class declaration specifies names or symbols, called class members, and their corresponding types. Several definitions of these names can be given, each one in an _instance definition_. Each definition of a name _x_, in an instance definition, must have a type that is an instance of the type given to _x_ in the type class declaration.

Consider, for example, a declaration of type class _Eq_ that defines symbols (==) and (/=) and their types, for comparing if two values are equal or not, respectively:

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr001.gif)

1.  [Download : Download high-res image (15KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr001_lrg.gif "Download high-res image (15KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr001.gif "Download full-size image")

The class declaration of _Eq_ specifies also so-called _default_ definitions. A default definition of a name _x_ is assumed to be given in an instance definition that does not specify itself a definition for _x_.

Instances of type class _Eq_ defining equality and inequality of operations, denoted by (==) and (/=), for values of types _Int_ and _Bool_, can then be given as follows, assuming that _primEqInt_ is a primitive function for testing equality of values of type _Int_:

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr002.gif)

1.  [Download : Download high-res image (19KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr002_lrg.gif "Download high-res image (19KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr002.gif "Download full-size image")

It is well-known that it is possible to explore infinitary constrained polymorphism in Haskell, for example by defining equality for an infinite number of types of lists, as follows:

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr003.gif)

1.  [Download : Download high-res image (15KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr003_lrg.gif "Download high-res image (15KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr003.gif "Download full-size image")

As a consequence of this instance definition, every list formed by elements which can be compared for equality can itself be compared for equality.

Polymorphic functions may be defined by the use of polymorphic overloaded symbols; for example:

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr004.gif)

1.  [Download : Download high-res image (10KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr004_lrg.gif "Download high-res image (10KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr004.gif "Download full-size image")

The type of _member_ is ∀a.Eqa⇒a→\[a\]→Bool. Constraint Eqa restricts _member_ to be applied only for types that are instances of type class _Eq_.

A type class can be defined as a subclass of an existing type class. For example:

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr005.gif)

1.  [Download : Download high-res image (9KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr005_lrg.gif "Download high-res image (9KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr005.gif "Download full-size image")

defines _Ord_ as a subclass of _Eq_, which means that every type that is an instance of _Ord_ must also be an instance of _Eq_. Consider the following example:

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr006.gif)

1.  [Download : Download high-res image (16KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr006_lrg.gif "Download high-res image (16KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr006.gif "Download full-size image")

The type of _search_ is ∀a.Orda⇒a→\[a\]→Bool.

The fact that _Eq_ is a subclass of _Ord_ enables the constraint on the type of search to be _Ord a_, instead of (Orda,Eqa). Constraint Eqa needs not be explicitly included, because it is implied by the constraint Orda.

x‾ denotes the sequence x1,…,xn, where n≥0. When used in the context of a set, it denotes the corresponding set of elements in the sequence ({x1,…,xn}).

In general, in a constrained type ∀a‾.C⇒τ, _C_ is a set of constraints, that restricts the set of types to which ∀a‾.τ may be instantiated, so that every instance τ\[a‾↦τ‾\] is satisfiable in the program theory, where a‾\=a1,…,an,τ‾\=τ1,…,τn and τ\[a‾↦τ‾\] denotes the simultaneous substitution of ai by τi in _τ_, for i\=1,…,n. Notation τ\[a‾↦τ‾\] is defined similarly for quantified types _σ_ (σ\[a‾↦τ‾\]) and for constraints. Constraint-set satisfiability is discussed in Section [5](#se0070).

Ambiguity in Haskell is considered as a syntactic property on types of expressions. The definition of this property has been changing over time, since Haskell 98, which supports only single parameter type classes (it has remained the same in Haskell 2010): for single parameter type classes, ambiguity of a constrained type ∀a‾.C⇒τ is characterized simply by fv(C)⊈fv(τ) (i.e. by the fact that there is a type variable that occurs in _C_ but not in _τ_) [\[2\]](#br0020), [\[5\]](#br0050).

In the sequel we consider multi-parameter type classes (MPTCs), and Haskell as it is defined in GHC [\[6\]](#br0060) with extensions related to MPTCs (when we refer to standard Haskell, we mean Haskell 98 or Haskell 2010). MPTCs are recognized as a natural extension to Haskell, that should be incorporated into the language. This has been recognized as early as in the original paper related to type classes [\[7\]](#br0070). This has not happened, however, mainly because of problems related to ambiguity, namely that the use of overloaded symbols were thought to introduce expressions with ambiguous types.

In order to introduce support for MPTCs, the definition of ambiguity in GHC was changed so that ambiguity could be avoided. Ambiguity of a constrained type C⇒τ was changed to a definition based on the property of reachability of type variables occurring in _C_, from the set of type variables occurring in the simple type _τ_, where reachability is defined as follows:

**Definition 2**

A variable a∈fv(C) is called reachable from, or with respect to, a set of type variables _V_ if a∈V or if a∈π for some π∈C such that there exists b∈fv(π) such that _b_ is reachable. a∈fv(C) is called unreachable if it is not reachable.

The set of reachable and unreachable type variables of constraint set _C from V_ are denoted respectively by reachableVars(C,V) and unreachableVars(C,V).

The subset of constraints with reachable and of unreachable type variables of constraint set _C from V_ are denoted respectively by CVr and CVu.

We also say that type variables _W_ are reachable in constrained type C⇒τ if W⊆reachableVars(C,fv(τ)) (and similarly for unreachable type variables and if _W_ is a type variable instead of a set of type variables).

For example, for type σ\=∀c,e.Collce⇒c, variable _e_ is reachable from {c}, the set of type variables of the simple type (_c_) of _σ_; for type σ\=∀a.(Showa,Reada)⇒(String→String), variable _a_ is unreachable from the empty set of type variables of the simple type (String→String) of _σ_.

It is easy to see that, for all _C_, _V_ we have that:fv(C)\=reachableVars(C,V)∪unreachableVars(C,V) and reachableVars(C,V)∩unreachableVars(C,V)\=∅.

For example, both type variables _a_ and _b_ are reachable in constrained type (Fab,Oa)⇒b, since _b_ occurs in the simple type part (_b_), and _a_ occurs in the constraint Fab, which contains _b_.

We also use, in this paper, the following:

**Definition 3**

Overloading (of symbols that originate the constraints) in constraint set _D_ occurring in an expression with a constrained type C⇒τ is resolved if all type variables in D⊆C are unreachable from fv(τ).

For example, overloading in constraint set {Fab,Oa}, as well as in both constraints in this constraint set, of type (Fab,Oa)⇒b is yet unresolved, and overloading is resolved for any constraint set that occurs in a constraint set on a type where the simple type has no type variables, as {Showa,Reada} on ∀a.(Showa,Read a)⇒String→String.

The distinction between reachable and unreachable type variables in constraints on types of an expression is relevant because unreachable type variables can never be instantiated by unification with some other type, due to occurrence of this expression in some context.

GHC defines an expression as ambiguous by ambiguity of its type C⇒τ, which does not mean simply the existence of an unreachable variable in _C_, with respect to the set of type variables occurring in _τ_, but takes into account the use of functional dependencies [\[8\]](#br0080), [\[9\]](#br0090), [\[10\]](#br0100). Following Haskell's open-world assumption, according to which instances may be added to a well-typed program without causing a type error, ambiguity of a constrained type C⇒τ is characterized by the existence of a type variable in _C_ that is not _uniquely determined_ from the set of type variables in the simple type _τ_ [\[11\]](#br0110).

Informally, this unique determination specifies that, for each type variable _α_ that is in _C_ but not in _τ_, there must exist a functional dependency β↦α, for some _β_ in _τ_ (or a similar unique determination specified via type families, instead of functional dependencies). In this paper we use β↦α, instead of β→α, used in Haskell, to indicate a functional dependency (to avoid confusion with the notation used to denote functions).

This unique determination has been formalized in [\[8\]](#br0080), [\[10\]](#br0100), upon which the formalization of open-world ambiguity below is based.

Consider that:

1.

sequences of constraints and of types can be indexed directly by type class parameters (i.e. type variable names), taken into account that to each type class parameter there is a corresponding integer, which gives its position in the class declaration;

2.

X↦Y denotes a functional dependency from the set of type variables _X_ to the set of type variables _Y_, specifying that the values in _Y_ are determined by those in _X_;

3.

Fd(A) denotes the set of functional dependencies of type class _A_.

Then, for any constraint set _C_, there is a set of _induced functional dependencies_ of _C_, given by:IFd(C)\={fv(tX‾)↦fv(tY‾)|At‾∈C,(X↦Y)∈Fd(A)}

The transitive closure of _V_ with respect to IFd(C), denoted by VIFd(C)+, defines set of type variables in _C_ that are _uniquely determined_ from _V_.

For example, given class _F_ _a_ _b_ | b↦a where … (that specifies functional dependency b↦a), we have that {b}IFd(F)+\={a,b}.

We have:

**Definition 4** Open-world ambiguity

A type ∀a‾.C⇒τ is called _open-world ambiguous_ (abbreviated as _ow-ambiguous_) if (a‾∩fv(P))⊈fv(τ)IFd(C)+.

For example, constrained type (Fab,Oa)⇒b is ow-ambiguous. To prevent this ambiguity, programmers can use a functional dependency (b↦a) in the declaration of class _F_.

Figuring out which functional dependencies (or type functions) need to be specified for dealing with ambiguity errors can be avoided with delayed-closure ambiguity, as explained in Section [4](#se0040).

We define now the set of constraints formed by class and instance declarations that occur in a program, called a program theory (a term borrowed from [\[12\]](#br0120)), and constraint set provability (entailment), in a program theory.

**Definition 5**

A program theory _P_ is a set of axioms of first-order logic, generated from class and instance declarations occurring in the program, as follows (where C⇒π is considered syntactically equivalent to _π_ if _C_ is empty):

•

For each class declaration

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr007.gif)

1.  [Download : Download high-res image (4KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr007_lrg.gif "Download high-res image (4KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr007.gif "Download full-size image")

the program theory contains the following formula if _C_ is not empty:∀a‾.C⇒TCa‾ where a‾\=a1…an.

•

For each instance declaration

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr008.gif)

1.  [Download : Download high-res image (5KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr008_lrg.gif "Download high-res image (5KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr008.gif "Download full-size image")

the program theory contains the following formula:∀a‾.C⇒TCt1…tn where a‾\=fv(t1)∪…fv(tn)∪fv(C); if _C_ is empty, then the instance declaration is of the form

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr009.gif)

1.  [Download : Download high-res image (4KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr009_lrg.gif "Download high-res image (4KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr009.gif "Download full-size image")

and the program theory contains the formula:∀a‾.TCt1…tn

3\. Ambiguity and constrained polymorphism
------------------------------------------

Both the open-world and the standard definitions consider as ambiguous an expression _e_ that might be used in a program context where distinct instances exist for an overloaded symbol that occurs in _e_. The motivation for this is that a coherent semantics for _e_, obtained by using type derivations that derive the type obtained by considering the chosen instance types are selected for each overloaded symbol, does not exist (because distinct semantics values could be given by considering such distinct instances).

However:

1.

if the expression is effectively used in a context where overloading is resolved and there is a constraint on the expression's type for which distinct instances exist, then, and only then, a type-error, characterizing ambiguity, can be detected;

2.

the expression may be used only in contexts where overloading is resolved in such a way that there exists a single instance for the type of each overloaded symbol used in the expression.

This indicates a prematureness of ambiguity detection because of the possibility of an expression being used in a context where two distinct types exist for some used overloaded symbol. Such possibility is what both the open-world and the standard definitions of ambiguity consider, albeit in different ways, as shown in the remainder of this section.

The standard definition of ambiguity considers the existing instances but closes the world for expressions without considering whether overloading has been resolved or not. We consider an example below ([Example 2](#en0070)). The open-world definition of ambiguity disregards the existence or not of instances in the relevant context, and considers ambiguity by the possibility of inserting any instance definitions.

**Example 1**

Consider _the_ canonical ambiguous expression in Haskell (cf. e.g. [\[2\]](#br0020), [\[4\]](#br0040)): (_show_ . _read_), called e0 for further reference (where “.” denotes function composition).

This expression is considered ambiguous in Haskell, irrespective of the context in which it occurs. This is related to the fact that instances definitions are global, i.e. are always present in any scope. However, defaults could be specified (in this example, for _Show_, _Read_) in order to avoid ambiguity. In standard Haskell, defaults are restricted, in a rather ad-hoc way, for constraint sets that include a constraint on class _Num_. Subsection [4.2](#se0060) describes the use of defaults for avoiding ambiguity of constraint sets, and considers an extension for the use of defaults under delayed-closure ambiguity (Section [4](#se0040)). Under delayed-closure ambiguity, the type of e0 is _String_ → _String_ if the context has only one instance of _Show_ and _Read_; otherwise there is a type error (unsatisfiability if there is no such instance, ambiguity if there are two or more).

The fact that the simple type in the type of e0 cannot be changed by placing e0 in another context characterizes that ambiguity (and unsatisfiability) of e0 should be checked, that is, it should be verified whether there exists or not only one instance that satisfies the constraints on the type of the expression. If there is only one instance, the constraints are satisfied, and can then be removed from the type of the expression (in the example, the type of (_show_ . _read_) can be simplified from ∀a.(Showa,Reada)⇒(String→String) to _String_→ _String_.

Although both open-world and standard definitions of ambiguity both disregard whether overloading is or is not yet resolved and both anticipate the test of ambiguity, they disagree in key aspects: there are expressions that are unambiguous according to the standard definition but ow-ambiguous and vice-versa.

Examples of the first case occur both when overloading is and is not resolved. e0 is an example of when overloading is resolved: it is always ow-ambiguous, and standard ambiguity depends on the existence of two or more instances of (_Show a_, _Read a_) (in this case, the standard definition agrees with delayed-closure, presented in the next section). The following is an example of an expression for which overloading is not yet resolved (and is therefore not ambiguous according to the delayed-closure approach), that is ow-ambiguous and can be ambiguous or not according to the standard interpretation.

**Example 2**

Consider expression ((+) 1) — which in Haskell can be written as (1+) —, in a program with classes _Sum_ and _NumLit_, given in a program with the following classes, where 1 is considered to have type ∀a.NumLit a⇒a:

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr011.gif)

1.  [Download : Download high-res image (10KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr011_lrg.gif "Download high-res image (10KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr011.gif "Download full-size image")

We have that (1+) is considered ow-ambiguous, but the standard definition would consider it ambiguous only if there exist two or more instances of the type of (1+), namely ∀a,b,c.(NumLita,Sumabc)⇒b→c, in the program. For example, let P0 be a program theory that contains instances _Sum Int Float Float_, _Sum Float Float Float_, _NumLit Int_ and _NumLit Float_. Then (1+) is considered ambiguous, according to the standard definition, in P0.

Both open-world and standard ambiguity disregard that overloading is not yet resolved, and that the test of ambiguity should not be done yet, since the type of the polymorphic expression (1+) can still change depending on the context where it is used (in this case, both disagree with delayed-closure, described in the next section; remember: overloading is not yet resolved for an expression of type ∀a,b,c.(NumLita,Sumabc)⇒b→c).

4\. Delayed-closure ambiguity
-----------------------------

In this section we present an approach for dealing with ambiguity in Haskell that uses the presence of unreachable variables in a constraint for characterizing overloading resolution (or, more precisely, for characterizing that overloading should have been resolved), instead of characterizing ambiguity.

Informally, instead of issuing an ambiguity error, the presence of an unreachable type variable _a_ from the set of type variables in fv(τ), in a constrained type ∀a‾.C⇒τ, triggers a test of whether this variable can be instantiated (i.e. eliminated), because of the existence of a single instance that can be used to instantiate it. We use the following for this.

**Definition 6**

Consider constrained type C⇒τ, program theory _P_, and that type variable _a_ occurs in π∈C and is unreachable with respect to fv(τ) and consider a substitution _ϕ_, with domain restricted to unreachable type variables in _C_, such that P⊢eϕ(C). Then _ϕ_ is called a _satisfying substitution for C in P_. _A unique satisfying substitution for C in P is called an improvement substitution of C in P and_ ϕ(C) _the improved constraint_.

We can now define delayed-closure ambiguity, as follows.

**Definition 7**

Type ∀a‾.C⇒τ is _delayed-closure ambiguous_, with respect to a program theory _P_, if unreachableVars(C,fv(τ))≠∅ and there exist at least two satisfying substitutions for _C_ in _P_.

If there exists no satisfying substitution for _C_ in _P_ then _C_ is called unsatisfiable in _P_.

We call _improvement_ (cf. [\[15\]](#br0150)) the process of substituting a constrained type C⇒τ, in a given program theory _P_, by ϕ(C)⇒τ, where ϕ(C) is the improved constraint of _C_ in _P_.

**Example 3**

Consider type (_F a Bool_) ⇒ _Bool_, in a program with the following instances (forming a program theory _P_):

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr012.gif)

1.  [Download : Download high-res image (10KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr012_lrg.gif "Download high-res image (10KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr012.gif "Download full-size image")

Substitution (a↦Char) is the improvement substitution for (_F a Bool_) in _P_, and (_F Char Bool_) is the improved constraint of (_F a Bool_) in _P_.

**Example 4**

Consider a classical example of MPTCs with functional dependencies,[1](#fn0010) namely matrix multiplication.

Consider the following types:

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr013.gif)

1.  [Download : Download high-res image (11KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr013_lrg.gif "Download high-res image (11KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr013.gif "Download full-size image")

Consider also the following:

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr014.gif)

1.  [Download : Download high-res image (23KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr014_lrg.gif "Download high-res image (23KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr014.gif "Download full-size image")

The type of _x_ in the following example:

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr015.gif)

1.  [Download : Download high-res image (7KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr015_lrg.gif "Download high-res image (7KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr015.gif "Download full-size image")

is inferred to be MultMatrixMatrixa, MultaMatrixb⇒b.

Type variable _a_ is reachable from _b_, and thus, with delayed-closure ambiguity, there is no checking for ambiguity (no ambiguity arises, relieving the programmer from having to figure out if and which functional dependencies could solve the problem).

If _x_ is used in a context that type _Matrix_ is inferred for it (or of course if _x_ is declared to be of type _Matrix_), then the type:MultMatrixMatrixa,MultaMatrixMatrix⇒Matrix can be simplified to _Matrix_, if there is a single instance of (_Mult Matrix Matrix a_) in the current context; if another instance of (_Mult Matrix Matrix a_) exists in this context, then we have ambiguity (in this case, the compiler can report a helpful error message, informing that there are two or more instances of _Mult Matrix Matrix a_ in the context).

The possibility of adding further instances is possible until overloading resolution. This happens when the context cannot anymore change (instantiate) the type of the expression, because of the existence of unreachable type variables in the constraint. The programmer can then rely on the type inference algorithm to instantiate unreachable type variables (whenever there exists a single instance in the context for such instantiation), relieving the programmer from having to figure out if and which functional dependencies could solve his problem.

### 4.1. Discussion

Haskell's open-world has a notable characteristic that a well-typed program never becomes untypeable by the introduction of new instance declarations. Delayed-closure restricts this advantage to expressions for which overloading is not resolved; when overloading is resolved, the world is closed, i.e. existing definitions of overloaded names are considered, by checking ambiguity and unsatisfiability.

This seems a significant disadvantage, but let us consider further aspects. With delayed-closure ambiguity more programs become well-typed and ambiguity becomes easier to understand: under delayed-closure, ambiguity is not a syntactic property of a type, and it does not mean a possibility, of using an expression in a context where two or more instances for the type of the expression _might_ exist. It means the actual fact that there exist two or more instances, when overloading is resolved. This agrees with the usual understanding of ambiguity in a natural language, that considers ambiguity for concrete sentences, that may be interpreted in distinct ways. In our view the most important aspect is that ambiguity is distinguished from overloading resolution. Ambiguity is tested only after overloading resolution. The notion of unsatisfiability becomes a related notion, that refers to the nonexistence of instances for entailment of a constraint set. Variables in a constraint are either all reachable or all unreachable. If they are unreachable, the constraint can be removed (in the case of single entailment) or there is a type-error (ambiguity in cases of two or more, unsatisfiability in the case of no entailment). Another slight counter weight in favor of delayed-closure ambiguity is the fact that it yields a more symmetric treatment: for expressions for which overloading is resolved, removal of an instance declaration may cause unsatisfiability and insertion may cause ambiguity.

The use of delayed-closure ambiguity in Haskell would benefit by two significant changes in Haskell: the ability to control exportation and importation of instances in modules and the possibility of specifying defaults for constraint sets. A mechanism for specifying defaults is considered in the next subsection. There are several proposals for allowing the control instance scope visibility in modules (see e.g. [\[16\]](#br0160), [\[17\]](#br0170), [\[18\]](#br0180)), and a detailed discussion is left for future work.

### 4.2. Defaults

Defaults can be specified in standard Haskell but only for constraint sets where all constraints consist of classes declared in the Haskell Prelude and one of them is class _Num_. Consequences of this are that predefined classes in general and class _Num_ in particular have to be distinguished by Haskell compilers and, more significantly, an exceptional rule is created, without a strong technical reason for restricting defaults to specific classes. The motivation is to avoid some frequent uses of type annotations.

Distinct proposals related to changing the way of handling defaults in GHC can be consulted at:

> [http://ghc.haskell.org/trac/haskell-prime/wiki/Defaulting](http://ghc.haskell.org/trac/haskell-prime/wiki/Defaulting)

These include a proposal for removing the possibility of specifying defaults altogether. We basically follow the basic proposal (number 2) related to the possibility of specifying defaults for MPTCs.

A default clause should be in our view a top level declaration (like class and instance declarations) to be applied only within the module containing the declaration, and it should not be possible to either export or import defaults. The relevant issue here is to disallow a change in the behavior of a module because of a change in which modules are imported.

A default clause may specify a default for a constraint, which may be a type expression (not only a type) of any kind. For example, we can have:

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr016.gif)

1.  [Download : Download high-res image (8KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr016_lrg.gif "Download high-res image (8KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr016.gif "Download full-size image")

There must exist a single default clause for each constraint. We consider that the names given in default clauses may be subject to renaming substitutions (for example, default (_Read a_) _Int_ and default (_Read b_) _Int_ are equivalent).

Default application is only considered for constraint sets with unreachable variables, and the only result of applying defaults is the removal of constraints (since a constraint which contains an unreachable type variable can only contain unreachable type variables). Constraint set removal is done per set of unreachable variables, as follows.

**Definition 8** Constraint Set Removal via Defaulting

A constraint set _C_ can be removed from C∪D⇒τ by application of a default if and only if the following hold:

1.

fv(C)∩(fv(D)∪tv(τ))\=∅; i.e. _C_ has only unreachable variables, and they do not occur in (the subset that is not removed) _D_;

2.

there exists a satisfying substitution _ϕ_ for _C_ in the program theory such that, for each constraint Aτ‾ in _C_, there is a default clause of the form default (_A_ τ‾) ρ‾ in the current module and ϕ(τ‾)\=ρ‾.

For example, consider default clauses default (_Read a_) _Int_ and default (_Show a_) _Int_ in a given module, with a program theory _P_ such that P⊢e(ShowInt,ReadInt,ShowBool,ReadBool). Let C\={Reada,Showa}. Then _C_ can be removed in this module from constrained type C⇒τ if a∉fv(τ) ((a↦Int) is the satisfying substitution for _C_ used in the definition above).

However, if the default clauses were, for example, default (_Read a_) _Int_ and default (_Show a_) _Bool_, then _C_ cannot be removed. Substitution (a↦Int) cannot be used since there is no default clause default _Show a_ _Int_; analogously, (a↦Bool) cannot be used since there is no default clause default (_Read_ _a_) _Bool_.

5\. Satisfiability
------------------

This section contains a description of constraint set satisfiability, including a discussion of decidability, based on work already presented in [\[19\]](#br0190).

Following [\[15\]](#br0150), ⌊C⌋P is used to denote the set of satisfiable instances of constraint set _C_ with respect to program theory _P_:⌊C⌋P\={ϕ(C)|P⊢eϕ(C)}

**Example 5**

As an example, consider:P\={∀a,b.Dab⇒C\[a\]b,DBool\[Bool\]} We have that ⌊Caa⌋P\=⌊C\[Bool\]\[Bool\]⌋P. Both constraints DBool\[Bool\]⇒C\[Bool\]\[Bool\] and C\[Bool\]\[Bool\] are members of ⌊Caa⌋P and also members of ⌊C\[Bool\]\[Bool\]⌋P.

A proof that P⊢e{C\[Bool\]\[Bool\]} holds can be given from the entailment rules given in [Fig. 1](#fg0010), since this is the conclusion of rule (mp0) with premises P⊢e{DBool\[Bool\]} and P⊢e{DBool\[Bool\]⇒C\[Bool\]\[Bool\]}, and these two premises can be derived by using rule (inst0).

Equality of constraint sets is considered modulo type variable renaming. That is, constraint sets _C_, _D_ are also equal if there exists a renaming substitution _ϕ_ that can be applied to _C_ to make _ϕ_ _C_ and _D_ equal. _ϕ_ is a renaming substitution if for all α∈dom(S) we have that ϕ(α)\=β, for some type variable β∉dom(ϕ).

Constraint set satisfiability is in general an undecidable problem [\[20\]](#br0200). It is restricted in this work so that it becomes decidable, as described below.

The restriction is based on a measure of constraints, given by a so-called constraint-head-value function, based on a measure of the sizes of types in this constraint head. Essentially, the sequence of constraints that unify with a constraint axiom in recursive calls of the function that checks satisfiability or simplification of a type constraint is such that either the sizes of types of each constraint in this sequence is decreasing or there exists at least one type parameter position with decreasing size.

The definition of the constraint-head-value function is based on the use of a constraint value ν(π) that gives the number of occurrences of type variables and type constructors in _π_, defined as follows:ν(Cτ1⋯τn)\=∑i\=1nν(τi)ν(T)\=1ν(α)\=1ν(ττ′)\=ν(τ)+ν(τ′)

Consider computation of satisfiability of a given constraint set _C_ with respect to program theory _P_ and consider that, during the process of checking satisfiability of a constraint π∈C, a constraint π′ unifies with the head of constraint ∀α‾.C0⇒π0 in _P_, with unifying substitution _ϕ_. Then, for any constraint π1 that, in this process of checking satisfiability of _π_, also unifies with π0, where the corresponding unifying substitution is ϕ1, the following is required, for satisfiability of _π_ to hold:

1.

ν(ϕπ′) is less than ν(ϕ1π1) or, if ν(ϕπ′)\=ν(ϕ1π1), then ϕπ′≠π″, for all π″ that has the same constraint value as π′ and has unified with π0 in process of checking for satisfiability of _π_, or

2.

ν(ϕπ′) is greater than ν(ϕ1π1) but then there is a type argument position such that the number of type variables and constructors, in this argument position, of constraints that unify with π0 decreases.

More precisely, constrain-head-value-function Φ associates a pair (I,Π) to each constraint (∀α‾.P0⇒π0)∈P, where _I_ is a tuple of constraint values and Π is a set of constraints. Let Φ0(π0)\=(I0,∅) for each constraint axiom ∀α‾.P0⇒π0∈P, where I0 is a tuple of n+1 values equal to ∞, a large enough constraint value defined so that ∞\>ν(π) for any constraint _π_ in the program theory.

Decidability is guaranteed by defining the operation of updating Φ(π0)\=(I,Π), denoted by Φ\[π0,π\], as follows, where I\=(v0,v1,…,vn) and π\=Cτ1⋯τn:Φ\[π0,π\]\={Failif vi′\=−1 for i\=0,…,nΦ′otherwise where Φ′(π0)\=((v0′,v1′,…,vn′),Π∪{π})Φ′(x)\=Φ(x) for x≠π0v0′\={ν(π)if ν(π)<v0 orν(π)\=v0 and π∉Π−1otherwisefor i\=1,…,nvi′\={ν(τi)if ν(τi)<vi−1otherwise Let sats1(π,P,Δ) hold if

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr017.gif)

1.  [Download : Download high-res image (15KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr017_lrg.gif "Download high-res image (15KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr017.gif "Download full-size image")

where _mgu_ is the most general (least) unifier relation [\[21\]](#br0210): mgu(T,ϕ) is defined to hold between a set of pairs of simple types or constraints T and a substitution _ϕ_ if i) _ϕ_ is a unifier of every pair in T (i.e. ϕτ\=ϕτ′ for every (τ,τ′)∈T, and analogously for pairs of simple constraints (π,π′)∈T), and ii) it is the least such unifier (i.e. if ϕ′ is a unifier of all pairs in T, then ϕ≤ϕ′).

The following examples illustrate the definition of constraint set satisfiability as defined in [Fig. 2](#fg0020). Let Φ(π).I and Φ(π).Π denote the first and second components of Φ(π), respectively.

**Example 6**

Consider satisfiability of π\=Eq\[\[I\]\] in P\={EqI,∀a.Eqa⇒Eq\[a\]}, letting π0\=Eq\[a\]; we have:sats1(π,P,{(ϕ|∅,{Eq\[I\]},π0)}),ϕ\=\[a1↦\[I\]\]S0\={ϕ1∘id|ϕ1∈S1,Eq\[I\]⊢satsP,Φ1S1}π⊢satsP,Φ0S0 where Φ1\=Φ0\[π0,π\], which implies that Φ1(π0)\=((3,3),{π}), since ν(π)\=3, and a1 is a fresh type variable; then:sats1(Eq\[I\],Θ,{(ϕ′|∅,{EqI},π0)}),ϕ′\=\[a2↦I\]S1\={ϕ2∘id|ϕ2∈S2,EqI⊢satsP,Φ2S2}Eq\[I\]⊢satsP,Φ1S1 where Φ2\=Φ1\[π0,Eq\[I\]\], which implies that Φ2(π0)\=((2,2),Π2), with Π2\={π,Eq\[I\]}), since ν(Eq\[I\])\=2 is less than Φ1(π0).I.v0\=3; then:sats1(EqI,P,{(id,∅,EqI)})S2\={ϕ3∘id|ϕ3∈S3,∅⊢satsP,Φ3S3}EqI⊢satsP,Φ2S2 where Φ3\=Φ2\[EqI,Eq I\] and S3\={id} by (SEmpty1).

The following illustrates a case of satisfiability involving a constraint π′ that unifies with a constraint head π0 such that ν(π′) is greater than the upper bound associated to π0, which is the first component of Φ(π0).I.

**Example 7**

Consider satisfiability of π\=AI(T3I) in program theory P\={A(Ta)I,∀a,b.A(T2a)b⇒Aa(Tb)}. We have, where π0\=Aa(Tb):sats1(π,P,{(ϕ|∅,{π1},π0)})ϕ\=\[a1↦I,b1↦T2I\]π1\=A(T2I)(T2I)S0\={ϕ1∘id|ϕ1∈S1,π1⊢satsP,Φ1S1}π⊢satsP,Φ0S0 where Φ1\=Φ0\[π0,π\], which implies that Φ1(π0).I\=(5,1,4); then:sats1(π1,Θ,{(ϕ′|∅,{π2},π0)})ϕ′\=\[a2↦T2I,b2↦TI\]π2\=A(T4I)(TI)S1\={ϕ2∘\[a1↦T2a2\]|ϕ2∈S2,π2⊢satsP,Φ2S2}π1⊢satsP,Φ1S1 where Φ2\=Φ1\[π0,π1\]. Since ν(π1)\=6\>5\=Φ1(π0).I.v0, we have that Φ2(π0).I\=(−1,−1,3).

Again, π2 unifies with π0, with unifying substitution ϕ′\=\[a3↦T4I,b2↦I\], and updating Φ3\=Φ2\[π0,π2\] gives Φ3(π0).I\=(−1,−1,2). Satisfiability is then finally tested for π3\=A(T6I)I, that unifies with A(Ta)I, returning S3\={\[a3↦T5I\]|∅}\={id}. Constraint _π_ is thus satisfiable, with S0\={id}.

The following example illustrates a case where the information kept in the second component of Φ(π0) is relevant.

**Example 8**

Consider the satisfiability of π\=A(T2I)F in program theory P\={AI(T2F),∀a,b.Aa(Tb)⇒A(Ta)b} and let π0\=A(Ta)b. Then:sats1(π,P,{(ϕ|∅,{π1},π0)})ϕ\=\[a1↦(TI),b1↦F\]π1\=A(TI)(TF)S0\={ϕ1∘id|ϕ1∈S1,π1⊢satsP,Φ1S1}π⊢satsP,Φ0S0 where Φ1\=Φ0\[π0,π\], giving Φ1(π0)\=((4,3,1),{π}); then:sats1(π1,P,{(ϕ′|∅,{π2},π0)})ϕ′\=\[a2↦I,b2↦TF\],π2\=AI(T2F)S1\={ϕ2∘id|ϕ2∈S2,π2⊢satsP,Φ2S2}π1⊢satsP,Φ1S1 where Φ2\=Φ1\[π0,π1\]. Since ν(π1)\=4, which is equal to the first component of Φ1(π0).I, and π1 is not in Φ1(π0).Π, we obtain that S2\={id} and _π_ is thus satisfiable (since sats1(AI(T2F),P)\={(id,∅,AI(T2F)}).

Since satisfiability of type class constraints is in general undecidable [\[20\]](#br0200), there exist satisfiable instances which are considered to be unsatisfiable according to the definition of [Fig. 2](#fg0020). Examples can be constructed by encoding instances of solvable Post Correspondence problems by means of constraint set satisfiability, using G. Smith's scheme [\[20\]](#br0200).

To prove that satisfiability as defined in [Fig. 2](#fg0020) is decidable, consider that there exist finitely many constraints in program theory _P_, and that, for any constraint _π_ that unifies with π0, we have, by the definition of Φ\[π0,π\], that Φ(π0) is updated so as to include a new value in its second component (otherwise Φ\[π0,π\]\=Fail and satisfiability yields ∅ as the set of satisfying substitutions for the original constraint). The conclusion follows from the fact that Φ(π0) can have only finitely many distinct values, for any π0.

### 5.1. Improvement

In this paper, improvement filters out constraints with unreachable type variables (remember that the presence of unreachable type variables in a constraint is an indication that overloading has been resolved) from a constraint _C_, on a constrained type C⇒τ. Improvement tests satisfiability on Cfv(τ)u (the subset of constraints of _C_ with unreachable type variables) and removes Cfv(τ)u if each constraint in this subset has a single satisfying substitution.

### 5.2. Context reduction

Context reduction is a process that reduces a constraint _π_ into constraint set _D_ according to a _matching instance_ for _π_ in the relevant program theory _P_: if there exists (∀α‾.C⇒π′)∈P such that ϕ(π′)\=π, for some _ϕ_ such that ϕ(C) reduces to _D_; if there is no matching instance for _π_ or no reduction of ϕ(C) is possible, then _π_ reduces to (a constraint set containing only) itself.

As an example of a context reduction, consider an instance declaration that introduces ∀a.Eqa⇒Eq\[a\] in program theory _P_; then _Eq_\[_a_\] is reduced to _Eq a_.

Context reduction can also occur due to the presence of superclass class declarations, but we only consider the case of instance declarations in this paper, which is the more complex process. The treatment of reducing constraints due to the existence of superclasses is standard; see e.g. [\[2\]](#br0020), [\[5\]](#br0050), [\[3\]](#br0030).

Context reduction uses _matches_, defined as follows:

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr020.gif)

1.  [Download : Download high-res image (22KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr020_lrg.gif "Download high-res image (22KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr020.gif "Download full-size image")

where _mgm_ is analogous to _mgu_ but denotes the most general matching substitution, instead of the most general unifier.

The third parameter of _matches_ is either empty or a singleton set, since overlapping instances [\[22\]](#br0220) are not considered.

The least constraint value function is used as in the definition of _sats_ to guarantee that context reduction is a decidable relation.

An empty constraint set reduces to itself (red). Rule (conj) specifies that constraint set simplification works, unlike constraint set satisfiability, by performing a union of the result of simplifying separately each constraint in the constraint set. To see that a rule similar to (conj) cannot be used in the case of constraint set satisfiability, consider a simple example, of satisfiability of C\={Aa,Ba} in P\={AInt,ABool,BInt,BChar}. Satisfiability of _C_ yields a single substitution where _a_ maps to _Int_, not the union of computing satisfiability for _A a_ and _B a_ separately.

Rule (inst) specifies that if there exists a constraint axiom ∀α‾.C⇒Aτ‾, such that Aτ‾ matches with an input constraint _π_, then _π_ reduces to any constraint set _D_ that _C_ reduces to.

Rules (stop0) and (stop) deal with failure due to updating of the constraint-head-value function.

6\. Type system
---------------

In this section we present a type system for a core-Haskell language that adopts delayed-closure ambiguity.

We use a context-free syntax of core Haskell expressions, given in [Fig. 5](#fg0050), where meta-variable _x_ represents a variable. Meta-variables _x_, _y_, _z_ denote variables and _e_ an expression, possibly primed or subscripted. We call the language core Haskell (not core ML) because expressions are considered to be typed in a program theory (as defined in Section [1](#se0010)), with information about overloaded symbols generated from class and instance declarations.

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr022.gif)

1.  [Download : Download high-res image (5KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr022_lrg.gif "Download high-res image (5KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr022.gif "Download full-size image")

Fig. 5. Context-free syntax of core Haskell expressions.

A context-free syntax of constrained types is presented in [Fig. 6](#fg0060), where meta-variable usage is also indicated. For simplicity and following common practice, kinds are not considered in type expressions (and thus type expressions which are not simple types are not distinguished from simple types). Also, type expression variables are called simply type variables.

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr023.gif)

1.  [Download : Download high-res image (43KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr023_lrg.gif "Download high-res image (43KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr023.gif "Download full-size image")

Fig. 6. Types, constraints and meta-variable usage.

We assume that a program theory is part of a typing context Γ, and can be denoted by PΓ. The initial, global typing context under which program expressions are considered to be typed contain all assumptions x:σ, where _x_ is a member of a type class _A_ (declared as class C⇒Aα‾ where …_x_ :: _τ_…) and σ\=∀α‾.Aa‾⇒τ is the type obtained including in α‾ type variables in fv(τ)∪α‾∪fv(C).

Note that type ordering disregards constraint set satisfiability. Satisfiability is only important when considering whether a constraint set _C_ can be removed from a constrained type C,D⇒τ (_C_ can be removed if and only if overloading for _C_ has been resolved and there exists a single satisfying substitution for _C_; see [Fig. 8](#fg0080)).

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr025.gif)

1.  [Download : Download high-res image (4KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr025_lrg.gif "Download high-res image (4KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr025.gif "Download full-size image")

Fig. 8. Constraint set simplification.

In rule (APP), the constraints on the type of the result are those that occur in the function type plus not all constraints that occur in the type of the argument but only those that have variables reachable from the set of variables that occur in the simple type of the result or in the constraint set on the function type. This allows, for example, not including constraints on the type of the following expressions, where _o_ is any expression, with a possibly non-empty set of constraints on its type: _flip_  _const_  _o_ (where _const_ has type ∀a,b.a→b→a and _flip_ has type ∀a,b,c.(a→b→c)→b→a→c), which should denote an identity function, and _fst_ (_e_, _o_), which should have the same denotation as _e_.

C⊕VD denotes the constraint set obtained by adding to _C_ constraints from _D_ that have type variables reachable from _V_:P⊕VQ\=P∪{ψ∈Q|fv(ψ)∩reachableVars(Q,V)≠∅}

gen(ψ,σ,V) holds if σ\=∀α‾.ψ, where α‾\=fv(ψ)−V.

Relation ≫P is a simplification relation on constraints, defined as a composition of improvement and context reduction, defined respectively in subsections [5.1](#se0080) and [5.2](#se0090).

7\. Type inference
------------------

In this section we present a type inference algorithm for core-Haskell, and discuss soundness and completeness of type inference with respect to the type system.

A type inference algorithm for core Haskell is presented in [Fig. 10](#fg0100), using rules of the form Γ⊢ie:(ψ,ϕ), which means that _ψ_ is the least (principal) type of (derivable for) _e_ in typing context _ϕ_Γ, where ϕΓ≤Γ and, whenever Γ′≤Γ is such that Γ′⊢ie:(ψ′,ϕ′), we have that ϕΓ≤Γ′ and ψ′≤ϕψ. Furthermore, we have that ϕΓ⊢ie:(ψ,ϕ′) holds whenever Γ⊢ie:(ψ,ϕ) holds, where ϕ′≤ϕ (cf. [Theorem 1](#en0180) below).

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr027.gif)

1.  [Download : Download high-res image (58KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr027_lrg.gif "Download high-res image (58KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr027.gif "Download full-size image")

Fig. 10. Type inference.

**Example 9**

Consider expression _x_ and typing context Γ\={f:Int→Int,x:α}; we can derive Γ⊢ifx:(Int,ϕ), where ϕ\=\[α↦Int\]. From ϕΓ\={f:Int→Int,x:Int}, we can derive ϕΓ⊢ifx:(Int,id).

**Theorem 1**

_If_ Γ⊢ie:(ψ,ϕ) _holds then_ ϕΓ⊢ie:(ψ,ϕ′) _holds, where_ ϕ′≤ϕ_._

_Furthermore, for all typing contexts_ Γ′ _with the same quantified type assumptions as_ Γ _— i.e. for all_ Γ′ _such that_ PΓ′\=PΓ _and for which_ (x:∀α.σ)∈Γ′ _implies_ (x:∀α.σ)∈Γ _—, if_ Γ′⊢ie:(ψ′,ϕ′) _is derivable, for some_ ψ′_,_ ϕ′_, we have that_ ϕΓ≤Γ′_,_ ψ≤ψ′ _and_ ϕ′≤ϕ_._

mguI is a function that gives a most general unifier of a set of pairs of simple types (or simple constraints). mguI(τ\=τ′,ϕ) is an alternative notation for mgu({(τ,τ′)},ϕ)). We have:

**Theorem 2** Soundness

_If_ Γ⊢ie:(ψ,ϕ) _holds then_ ϕΓ⊢e:ψ _holds._

**Theorem 3** Principal type inference

_If_ Γ⊢ie:(ψ,ϕ) _holds then, for all_ ψ′ _such that_ Γ⊢e:ψ′ _holds, we have that_ ψ≤ψ′_._

A completeness theorem does not hold. For example, consider expression e0 of [Example 1](#en0060); we have that there exists Γ such that Γ⊢e0:String→String holds but there is no _ψ_, _ϕ_ such that Γ⊢e0:(ψ,ϕ) holds.

In our opinion, the greater simplicity obtained by allowing type instantiation to be done (“guessed”) in a context-independent way, does not compensate the disadvantages of allowing ambiguous expressions to be well-typed and of having several translations for expressions, one of them a principal translation. We prefer a declarative specification of type inference, that allows a unique type to be derivable for each expression, where type instantiation is restricted to be done only in a context-dependent way, given by considering functions, used in the type inference algorithm, as relations. In other words, the type inference algorithm can be obtained from a declarative specification of type inference by transforming relations used into functions; see [\[19\]](#br0190). The fact that every element is an element of a unique type is a bonus that agrees with everyday spoken language. It is straightforward to define, a posteriori, the set of types that are instances of the type of an expression.

The fact that only a single type can be derived for each expression rules out the possibility of having distinct type derivations. Thus, an error message for an expression such as (_show_ . _read_), in a context with more than one instance for _Show_ and _Read_, should be that the expression can not be given (there is no type that would allow it to have) a well-defined semantics. Distinct meanings of (_show_ . _read_) would be obtained from distinct instance types of _show_, _read_.

In the next section we give a semantics by induction on the derivation of the type of an expression by considering functions used in the type inference algorithm (mguI, _gen_, ≫P) as relations.

8\. Semantics
-------------

The semantics of core Haskell, given in [Fig. 11](#fg0110), follows a standard core Haskell semantics [\[7\]](#br0070), [\[2\]](#br0020), [\[5\]](#br0050), based on the application of (so-called) _dictionaries_ to names with constrained types. A dictionary is a tuple of denotations of definitions given in an instance declaration; in other words, denotations of _instance members_. A dictionary of a superclass contains also a pointer to a dictionary of each of its subclasses, but the treatment of superclasses is standard and is omitted in this paper (see e.g. [\[2\]](#br0020), [\[5\]](#br0050), [\[3\]](#br0030)).

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr028.gif)

1.  [Download : Download high-res image (62KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr028_lrg.gif "Download high-res image (62KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr028.gif "Download full-size image")

Fig. 11. Core Haskell semantics.

A fundamental characteristic of core Haskell is that the semantics of an expression depends on its type. The semantics is defined below by induction on the rules of a type system where each variable occurrence has its type annotated (there is no guessing of types). The type annotated for a variable in rule (VARs) is the greatest instance-type of the variable occurrence in the relevant typing and program contexts, as specified by [Definition 9](#en0210) below, where a program context C\[e\] is an expression that has _e_ as a subexpression. The type annotated for a lambda-bound variable is the type of the function parameter, and the type annotated for a let-bound variable is the type of its defining expression. Type annotations are indicated by a dot in bold face font (as in x:σ) in [Fig. 11](#fg0110).

**Definition 9**

ϕ′ψ is an _instance-type of e in_ Γ (and an _instance-type of e in_ Γ _and program context_ C\[e\]) if both Γ⊢ie:(ψ,ϕ) and Γ⊢iC\[e\]:(ψ′,ϕ′) are derivable, for some _ϕ_, ψ′.

Furthermore, _ψ_ is the greatest (most specific) instance-type for _e_ in Γ and program context e′\=C\[e\], modulo type variable renaming, denoted by git(e,Γ,e′), if _ψ_ is an instance-type of _e_ in Γ and program context C\[e\] and there is no instance-type ψ′ of _e_ in Γ and program context C′\[e′\] such that ψ′ is distinct from _ψ_ and ψ≤ψ′.

[Example 10](#en0220) below gives two distinct greatest instance-types of (==) in the same typing context and distinct program contexts (where B and C can be seen as abbreviations of _Bool_ and _Char_ respectively).

**Example 10**

Let {(==):∀a.Eqa⇒a→a→B,True:B,′⁎′:C}⊆Γ, PΓ\={EqB,EqC}, e\=((==) True,(==) '⁎'). Then Γ⊢ie:((B→B,C→B),ϕ) is derivable, where ϕ\=\[a↦B,b↦C\] and _a_, _b_ are fresh type variables.

Instance-types of (==) in program contexts (==) _True_ and (==) '⁎' are respectively B→B→B and C→C→B.

Typing formulas have the form Γ⊢ae:ψ, except that types of variables are annotated, as mentioned. A formal description of how to compute the annotated type of variables is left for further work.

For each class declaration classC⇒Aα‾ where x‾::τ‾, a selection function is generated for each overloaded name xi in x‾. Such name denotes, in the semantics, a function that merely selects the _i_\-th component of a dictionary (tuple) parameter; if n\=1, selection corresponds to the identity function. For example, class _Eq_ generates a pair of functions, with names ((==) and (/=)) that in the translation denote functions (_fst_ and _snd_) for selecting respectively the first and second components of a pair.

C‾ denotes a sequence of constraints of _C_ in a standard (lexicographical) order.

Each instance declarationinstanceC⇒πwherex‾\=e‾ generates either a dictionary or a dictionary constructor dπ, according to whether _C_ is empty or not, respectively. A dictionary constructor takes as parameter one dictionary for each constraint in the sequence C‾ and yields the dictionary of _π_. The instance declaration makes η(π) equal to dπ and η(xi,τi)\=(dπ,C). If there is no instance declaration for _π_, we assume that η(π) is undefined.

If _x_ is (not an instance member but) a let-bound variable, then η(x,τ) is made equal (d0,C), where a generalization of C⇒τ is the type of _x_; d0 is an indication that no dictionary or dictionary constructor is associated to (x,τ); d0 is used as indication that _x_ is a let-bound variable of constrained type, not an instance member.

For example, for a typing context Γ associated to a program that defines instances of classes _Eq_ for _Char_ and lists, we have, where dEqChar is a dictionary of class _Eq Char_ and dEqL is a dictionary constructor that constructs a dictionary of class _Eq_ for lists of values of type _a_ from a dictionary of values of (any) type _a_:η(EqChar)\=dEqCharη(\[aEq\]))\=dEqL where _a_ is a fresh, arbitrary type variable, and:η((==),Char→Char→Bool)\=(dEqChar,∅)η((==),\[a\]→\[a\]→Bool)\=(dEqL,{Eqa})

Let η†(C↦v‾) be equal to η\[π1↦v1,…,πn↦vn\], where C\={π1,…,πn}.

vSeq(C‾) denotes a sequence of fresh variables vi, one for each πi in the sequence C‾.

η(x,τ,Γ) gives the semantics of possibly overloaded name _x_ with type C⇒τ, for some _C_, in typing context Γ. The translation of an overloaded name _x_ that is an instance member is a selection from a dictionary (possibly constructed by passing pertinent dictionaries to a dictionary constructor). Otherwise, the translation is not a selection but a function call that passes pertinent dictionaries, one for each constraint in the constraint set on the type of _x_, to an already defined function.

η(C) denotes the sequence η(π1)…η(πn), where C‾\=π1…πn; the dictionary for constraint _π_ is given by from η(π). We have:η(x,τ,Γ)\={xif C0\=∅xv‾if d\=d0x(dv‾)otherwise where:(∀α‾.C0⇒τ0)\=Γ(x)ϕ\=mgu(τ,τ0),v‾\=η(ϕD),(d,D)\=η(x,τ)

Note that the constraint set _C_ on _x_'s type is disregarded in the semantics, which uses Γ(x) to obtain the original constraint set used in the definition of _x_; simple type _τ_ is used to instantiate the original constraint set. This is done because constraints removed from _C_ due to overloading resolution must be considered in the semantics. We have:

**Theorem 4**

_For any derivations_ Δ_,_ Δ′ _of typing formulas_ Γ⊢ae:ϕ _and_ Γ′⊢ae:ϕ_, respectively, where_ Γ _and_ Γ′ _give the same type to every x free in e, we have_〚Γ⊢ae:ϕ〛η\=〚Γ′⊢ae:ϕ〛η _where the meanings are defined using_ Δ _and_ Δ′_, respectively._

The proof is straightforward: since Γ and Γ′ give the same type to every _x_ free in _e_ and the type system rules are syntax-directed, Δ and Δ′ are the same.

Consider the following Haskell program extract:

**Example 11**

![](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr029.gif)

1.  [Download : Download high-res image (60KB)](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr029_lrg.gif "Download high-res image (60KB)")
2.  [Download : Download full-size image](https://ars.els-cdn.com/content/image/1-s2.0-S0167642316000836-gr029.gif "Download full-size image")

Several name bindings in a let-expression, the use of a variable pattern in a definition and a definition without a let-binding are considered to be syntactic sugar for, respectively, nested let-expressions, a definition of a lambda-abstraction and a definition of the form let _teqww_ \= λx.e in _teqww_.

The first occurrence of _teq_ in line (1) above is translated to teq(dTEqLv1v2), where _teq_'s translation is the identity function, dTEqL is a dictionary with a single function, say teqL, that receives the two dictionary arguments v1 and v2 passed to _teqww_ and yields teqL, the translation of function _teq_ for lists defined above. The translation is given with respect to environment _η_ such that η({TEqa,Showa})\=v‾ (where v‾ is the sequence v1v2), and η(teq,τ)\=dTEqL, where τ\=\[\[a\]\]→\[\[a\]\]→(Bool,String).

We have also that η(TEqInt) is equal to a dictionary with just one member (say, dTEqInt), and similarly for η(ShowInt). The translation of the second occurrence of _teq_ in line (1) above is equal to:teq(dTEqLdTEqIntdShowInt)

The use of dictionaries and the ensuing dictionary construction and selection of member values at run-time can be avoided by passing values that correspond to overloaded names that are in fact used. For example, an equality function for lists can receive just an equality function for list elements, instead of a dictionary containing also an unused inequality function. Passing a dictionary to perform selection at run-time is unnecessary. Full laziness and common subexpression elimination are techniques used to avoid repeated construction of dictionaries at run-time [\[2\]](#br0020), [\[3\]](#br0030), [\[27\]](#br0270), [\[28\]](#br0280), but the optimization could be avoided a priori. This and related implementation issues are however outside of the scope of this paper and are left for further work.

Note that the constraints on types of expressions are considered in the semantics only in the cases of polymorphic and constrained overloaded variables. Consider for example expression _eqStar_ given by (intended for comparison of the semantics of (==) with those of expressions (==) _x_ and (==) _x y_):leteq\=λx.λy.(==)xyineq'⁎' In a context where (==) has type Eqa⇒a→a→Bool, the translation of _eqStar_ is given by:leteq\=λv.λx.λy.(==)vxyineq dictEqChar'⁎' We have that (==) _v_ and _eq dictEqChar_ denote a primitive equality function for characters, say _primEqChar_. The translation of each occurrence of (==) passes a pertinent dictionary to (==) so that the type obtained is the expected type for an equality function on values of type _t_. Both expressions (==) _x_ and (==) _x y_ have also constrained types, but a dictionary is passed only in the case of (==). The semantics of an expression with a constrained type where the set of constraints is non-empty only considers this set of constraints if the expression is an overloaded variable; otherwise constraints are disregarded in the semantics. Furthermore, since each occurrence of an overloaded variable has a translation that is the application of pertinent dictionary values to that variable, translation of _types_ with constraints are never input or output values of the translation function.

9\. Related work
----------------

Blott [\[29\]](#br0290) and Jones [\[2\]](#br0020) have presented coherent semantics for ow-unambiguous expressions.

Sulzmann et al. [\[12\]](#br0120) consider the encoding of multi-parameter type classes with functional dependencies via constraint handling rules [\[30\]](#br0300). In their work, as in many other related works (e.g. [\[31\]](#br0310)), ambiguity means ow-ambiguity. Sulzmann et al.'s definition of ow-ambiguity is based on provability of constraints in a program theory, using constraint-handling rules (instead of being a definition that the set of type variables of a constraint set is not a subset of the set of induced functional dependencies of a simple type).

Functional dependencies, introduced in Haskell in order to allow the inference of more specific types and to avoid ambiguity errors, also allow computations at the type level, because of reductions forced by functional dependencies on the type inference algorithm. Type level programming based on functional dependencies has been explored for example in [\[32\]](#br0320), [\[33\]](#br0330) and has been used for instance to define heterogeneous collections and database access libraries for Haskell [\[34\]](#br0340), [\[35\]](#br0350). The use of delayed-closure ambiguity eliminates the need of functional dependencies to avoid ambiguity but does not allow type level programming, which relies on the fact that type specialization occurs in types that involve reachable type variables.

The delayed-closure approach described in this paper can be seen as a variation of Agda's approach to overloading [\[36\]](#br0360). Agda uses a context-independent approach where any use of an overloaded name requires overloading to be resolved. There is no support for constrained types, that allow overloading resolution to be deferred. Overloading works though as in the delayed-closure approach, being based on verifying whether there exists or not a _a single definition of a value in scope at the call site_ [\[36\]](#br0360) that is of the type of a so-called _instance argument_. If no such unique definition exists, a type error is reported.

An instance argument is similar to an implicit argument in Agda, but, instead of just requiring a dummy value to be inserted for an implicit argument, it is required that there exists a unique definition of the type of the instance argument in the current scope.

The delayed-closure approach does not have (and could be used to avoid) Agda's restriction to non-recursive resolution for instance arguments, in cases where overloading resolution requires a recursive search for verification of uniqueness of satisfiability. This can be done without the introduction of constrained types. The addition of constrained types in Agda requires further investigation.

10\. Conclusion
---------------

This paper discusses the problem of ambiguity in Haskell-like languages. A definition of ambiguity, called _delayed-closure_, is presented, where the existence of more than one instance (and more than one type derivation) for the same type of an expression is considered only when there exist unreachable variables in the constraints on the type of an expression. The presence of unreachable variables in constraints characterizes the nonexistence of a program context in which the expression could be placed that would allow instantiation of these variables and overloading resolution.

The paper describes an approach for using default declarations for avoiding ambiguity by the addition of new instance declarations, but leaves for further work a proposal for allowing the importation and exportation of type class instances.

Adopting delayed-closure ambiguity in Haskell would eliminate the need of using functional dependencies or type families for the purpose of dealing with ambiguity. It would also enable Haskell compilers to provide more helpful ambiguity-related error messages. There would be no influence on well-typed Haskell programs, but programs which currently cause ambiguity errors in Haskell could then become well-typed.

The paper presents a type system and a type inference algorithm that includes a constraint-set satisfiability function, that determines whether a given set of constraints is entailed or not in a given context, focusing on issues related to decidability, a constraint-set improvement function, for filtering out constraints for which overloading has been resolved, and a context-reduction function, for reducing constraint sets according to matching instances. A standard dictionary-style semantics for core Haskell is also presented.

As future work, we intend to investigate also the use of delayed-closure ambiguity in connection with type families.

© 2016 Elsevier B.V.


[Source](https://www.sciencedirect.com/science/article/pii/S0167642316000836#se0020)