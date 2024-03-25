# A terminal view of testing polymorphic functions

> A blog about functional programming

Posted on June 29, 2017

Parametricity restricts the behavior of a polymorphic function ϕ:∀α.Tα→Uα, so that, picking one type τ and one input value x:Tτ and inspecting the result ϕ x:Uτ often tells us about what happens on many other inputs.

To test polymorphic functions, Bernardy et al.[1](#fn1) (2010) have a monomorphization technique using initial algebras (the “initial view”). They also conjectured the existence of a dual method based on terminal coalgebras (the “terminal view”), which I will show in this post.

Initial view
------------

We assume that Tα has the form (Fα→α)×(Gα→X), for some functors F and G and a non-parametric type X (i.e., in which α does not occur), and that U (the result type) is also a functor. Then, to test a function of type ∀α.(Fα→α)×(Gα→X)→Uα, it suffices to only try inputs where the first argument is the initial F\-algebra, f0:Fμ→μ (and μ is the “least fixed point” of the functor F). The second argument may vary freely in the type Gμ→X.

The intuition is that the first argument, of type Fα→α, is used by the polymorphic function to _construct_ values of abstract type α, while the second argument provides ways of “observing” these constructed values. The initial algebra f0:Fμ→μ corresponds to an injective constructor, so that a tester can inspect values of type μ in the output Uμ to know exactly how they were constructed by ϕ. By parametricity, the polymorphic function ϕ must construct its output “uniformly” over all types, so looking at the result on one input allows us to deduce its behavior on many other inputs.

This effectively establishes an isomorphism:

(∀α.(Fα→α)×(Gα→X)→Uα)≃(Gμ→X)→Uμ

Testing a polymorphic function is equivalent to testing a corresponding monomorphic function, which is more straightforward to do.

### Definitions

Given ϕ:∀α.(Fα→α)×(Gα→α)→Uα. and the initial algebra f0:Fμ→μ, the _monomorphization_ of ϕ is defined by:

mono⁡ϕ:(Gμ→μ)→Uμmono⁡ϕ g0\=ϕ (f0,g0)

To define the inverse, we recall the definition of the initial algebra f0: every algebra f:Fα→α induces a _catamorphism_ ηf:μ→α, which is the unique function making the following square commute:

Fμ→f0μFηf↓↓ηfFα→fα

Given a monomorphic ψ:(Gμ→μ)→Uμ, its _polymorphization_ is:

poly⁡ψ:∀α.(Fα→α)×(Gα→α)→Uαpoly⁡ψ (f,g)\=Uηf (ψ (g∘Gηf))

### Monopoly theorem

**Polymorphization is the inverse of monomorphization.**

1.  poly is a right-inverse for mono:
    
    mono∘poly\=id
    
    Unfolding the definitions, we need to check the following equation:
    
    Uηf0 (ψ (g0∘Gηf0))\=ψ g0
    
    Indeed, the catamorphism ηf0 of the initial algebra must be the identity function. We may thus simplify the left hand side by functoriality of U and G.
    
2.  poly is a left-inverse for mono:
    
    poly∘mono\=id
    
    Equivalently,
    
    Uηf (ϕ (f0,g∘Gηf))\=ϕ (f,g)
    
    That is a consequence of the free theorem of ϕ, with the catamorphism ηf as a functional relation between μ and α (details omitted).
    

The form of Tα as (Fα→α)×(Gα→X) above may seem restrictive, but the paper also shows how a large class of types can actually be transformed to make that result applicable. Basically, the type Tα may be a product of types of the form Ciα→Diα where Ci,Di are functors, and Di is made of sums, products, and fixpoints.

To motivate the dual approach, let’s look at an example where that is not the case.

Example
-------

Let B be the type of booleans, with constructors 0,1:B and destructor if:∀β.B→β→β→β. Our example will be:

∀α.((α→B)×((α→B)→B))→B

Given arguments f:α→B and g:(α→B)→B, the result will be a boolean combination of applications of f and g. Since there is no α in the immediate context to apply f to, we can only apply g at first. Here is a typical inhabitant of the above type:

ϕ\=λ(f,g).g (λa.if (f a) 0 1)

The only way a polymorphic function ϕ of that type can inspect a value of type α is to use the first parameter. Hence, from the point of view of ϕ, a value of type α contains only as much information as a boolean. To test ϕ, it is actually sufficient to instantiate α with B and set the first argument to the identity function f0\=λx.x.

In fact, the above type is isomorphic to the following finite type:

((B→B)→B)→B

### Terminal view

With the initial view, we examined how a polymorphic function uses _constructors_ of abstract type α, represented by algebras Fα→α. With an initial algebra f0:Fμ→μ, the least fixed point μ was as large as the sum of ways of constructing its inhabitants using f0.

Here, we shall look at _destructors_: coalgebras α→Fα. (Above, Fα\=B.) With a terminal coalgebra f1:ν→Fν, the “greatest fixed point” ν will be as large as the “product” of observations we can make about an inhabitant using f1.

We now consider the following type, where F is a functor and we’ll see later what U can be:

∀α.(α→Fα)→Uα

What happens if we take the terminal coalgebra, f1:ν→Fν?

By definition, every coalgebra f:α→Fα induces an _anamorphism_ ϵf:α→ν, which is the unique function with the commutative square:

α→fFαϵf↓↓Fϵfν→f1Fν

Under the extra assumption that U is a _contravariant functor_, we show this isomorphism:

(∀α.(α→Fα)→Uα)≃Uν

### Definitions

The monomorphization of ϕ:∀α.(α→Fα)→Uα is defined by:

mono⁡ϕ:Uνmono⁡ϕ\=ϕ f1

The polymorphization of ψ:Uν is defined by:

poly⁡ψ:∀α.(α→Fα)→Uαpoly⁡ψ f\=Uϵf ψ

Note that the contravariant functor U lifts ϵf:α→ν to Uϵf:Uν→Uα.

### Theorem

**Polymorphization is the inverse of monomorphization.**

1.  poly is a right-inverse for mono:
    
    mono∘poly\=id
    
    Equivalently, Uϵf1 ψ\=ψ.
    
    Indeed, the anamorphism ϵf1 of the terminal coalgebra must be the identity function.
    
2.  poly is a left-inverse for mono:
    
    poly∘mono\=id
    
    Equivalently, Uϵf (ϕ f1)\=ϕ f.
    
    That is a consequence of the free theorem of ϕ, with the catamorphism ϵf as a functional relation between ν and α (details omitted).
    

### Application

This technique actually applies to our example; with α→Fα\=α→B and Uα\=((α→B)→B)→B, we obtain the same monomorphization:

ν\=Bf1\=λx.x

Notice that simple trick of pushing the extra argument type (α→B)→B into the result type Uα. This happens to work whenever the argument type is covariant in α (this includes types like α, X→α, X, where X is non-parametric).

Dually, in the initial view, we separated the algebra Fα→α from an “observation function” Gα→X; we can simplify that assumption by shoving that type (contravariant in α, since G is covariant) into the result type Uα, which remains covariant.

To summarize, we have two dual methods of monomorphizing polymorphic functions, of type ∀α.Tα→Uα, in the following situations:

*   ∀α.(Fα→α)→Uα, where F and U are covariant—in particular, Uα may be a function type whose arguments Gα→X correspond to “observation functions”;
*   ∀α.(α→Fα)→Uα, where F is covariant and U is contravariant—Uα may be a function type with “constructors” Y→Hα as arguments for example.

Overlapping views
-----------------

There are cases where both techniques apply. We should get equivalent results since monomorphizations are isomorphisms. For instance:

∀α.(X→α)→(α→Y)→Z

The initial view yields α\=X, with the first argument set to the identity function; the second argument, which may vary freely, has type X→Y.

The terminal view yields α\=Y, with the second argument set to the identity function; the first argument has type X→Y.

Here is another example:

∀α.(α→α×X)→α→Y

The coalgebra α→α×X views α as an infinite stream of X, i.e., the type Xω. We fix the first argument as the stream destructor Xω→Xω×X, and the second argument may be any stream Xω.

Before taking the initial view, we rewrite that type a bit, first by commutativity A→B→C≃B→A→C and second by distributivity of exponentials over products A→B×C≃(A→B)×(A→C).

(α→α×X)→α→Y≃α→(α→α×X)→Y≃α→(α→α)→(α→X)→Y

The algebra α×(α→α) (isomorphic to (𝟙+α)→α) views α as a natural number. With α\=N, we fix the first two arguments to the Peano constructors (zero and successor), and the last argument varies over N→X, which is isomorphic to streams Xω.

Questions
---------

The initial view can be adapted to other types ∀α.Tα→Uα when there is an embedding of Tα in some (Fα→α)×Cα for a covariant F and contravariant C.

Dually, are there interesting types to embed in (α→Fα)×Dα, for F and D both covariant?

* * *

Is there a unified view that generalizes the above?

* * *

What can we do for a type like ∀α.((α→α)→α)→α, for which neither the initial nor terminal view are applicable?

* * *

1.  [\*Testing polymorphic properties\*](http://publications.lib.chalmers.se/publication/99387-testing-polymorphic-properties). Jean-Philippe Bernardy, Patrik Jansson, Koen Claessen. ESOP 2010.[↩︎](#fnref1)


[Source](https://blog.poisson.chat/posts/2017-06-29-terminal-monomorphization.html)