# Canonical testing for polymorphic functions

> A blog about functional programming

Posted on June 7, 2017

Polymorphism is an essential aspect of reusable code: it describes programs that can operate on many types of data. Parametric polymorphism, which can be seen as a dual of encapsulation, also makes programming safer: encapsulated data can only be accessed through a clearly defined interface, and prevents accidental nonsensical operations.

If fewer mistakes are possible, then not only does the likeliness of writing a correct program increase, but hopefully it also becomes easier to check that the program is actually correct. _Testing polymorphic properties_[1](#fn1) cements that intuition: we only need to test a polymorphic function on a very small subset of its domain.

A particular aspect I would like to clarify is the optimality of the approach presented in that paper: it seems to carve out a subset of test cases that is the least redundant in a certain sense.

This blog post gives a formalization of the problem of testing and shows some examples taking advantage of parametric polymorphism to solve it efficiently.

Testing
-------

Let ∀α.Tα→Uα be a parametric function type. We will leave the quantifier implicit and just write Tα→Uα.

The goal of testing is to determine whether an implementation f:Tα→Uα matches a specification g:Tα→Uα, such that ∀x.f x\=g x; a straightforward approach is to test that property with some inputs. The formal definitions here are tailored to testing polymorphic functions but they can easily be reformulated in different settings.

> A **test input** x for Tα→Uα is a value of type TA for some type A. Alternatively, it is an inhabitant of the **domain** ∃α.Tα.

In general, we cannot try all inputs: there are infinitely many types A to instantiate α with, resulting in overwhelmingly many different potential test cases. The problem is thus to find a good set of test inputs.

> A **test set** X for Tα→Uα is a set of test inputs. Alternatively, it is a subset of ∃α.Tα.

The main property to look for is that an implementation matches a specification if and only if all tests pass. This is _completeness_.

> A test set X for Tα→Uα is **complete** when, for all functions f,g:Tα→Uα, (∀x∈X.f x\=g x)⟹f\=g.

The whole domain of Tα→Uα, i.e., ∃α.Tα defines a complete test set, but as we mentioned earlier, it is rather wasteful. Here are some examples where we can have much smaller complete test sets.

Examples
--------

α×α→α

f can return either its first or second argument, independently of their values. To test it, we only need to apply f to a pair of distinguishable elements.

X\={(0,1):N×N}

* * *

α×(α→α)→α

f can only iterate its second argument on the first a fixed number of times. A function that counts these iterations does the job.

X\={(0,λn.n+1):N×(N→N)}

* * *

Let α⋆ be the type of lists of α.

α⋆→α⋆

f can drop, duplicate, and move elements from the input list, depending only on its length, but it can not invent new elements. For every given length, we only need to apply f to a single list to fully determine its behavior for all lists of that length.

X\={\[ \],\[0\],\[0,1\],\[0,1,2\],\[0,1,2,3\],⋯:N⋆}

* * *

α→α

Only the identity function has that type. There’s no need to test it.

X\=∅

* * *

Let B be the type of booleans.

α→B

There’s no way for that function to inspect its argument, so it must be a constant function. We only need to apply it to one arbitrary argument, say the unit ():𝟙, to obtain that constant.

X\={():𝟙}

* * *

α×(α→B)→B

f can only observe the first argument through the second one. That type is isomorphic to B→B. We need two inputs to test f.

X\={((),(λ().true)),((),(λ().false)):𝟙×(𝟙→B)}

* * *

Let 𝟘 be the empty type.

α+(α→𝟘)→B

f can only see whether its argument is a left or right. This example shows that test inputs may not instantiate the type variable α identically.

X\={left ():𝟙+(𝟙→𝟘)right (λz.z):𝟘+(𝟘→𝟘)}

* * *

α×(α→α+α)→α

This type is isomorphic to

α×(α→(B×α))→αα×(α→α)×(α→B)→α

Using α and α→α, we generate some values of type α, and with α→B we assign one boolean to each value.

X\={(0,λn.n+1,p):N×(N→N)×(N→B)|p:N→B}

* * *

Some more examples to think about. For the first two, the presence of multiple type variables does not seem to cause any major issue.

(α+β)×(α→β)→β(β→α)×(α→β)×α→β The last one contrasts with previous examples: it also has the shape Tα→Uα, but U is no longer covariant; this also arises by currying examples whose domain is a product, but the output sum type wrapping functions looks even more puzzling; I haven’t given much thought about this situation.

α→(α→α)+((α→B)→B)

* * *

Test sets
---------

There seem to be some common ideas in these examples. For instance, we want to generate test cases which are just “big enough” to not forget operations done by the function under test; this corresponds to the concept of initial algebras, which is involved in _Testing Polymorphic Properties_.

Another idea, which I want to make more precise here, is that the test set should be as small as possible.

We may ask that the complete test set X should be minimal for set inclusion, but this isn’t quite sufficient to characterize a good test set. Indeed, consider the following type again:

α⋆→α⋆

Here is another complete test set, it differs from X above in lists of length 3:

Y\={\[ \],\[0\],\[0,1\],\[0,0,1\],\[0,1,1\],\[0,1,2,3\],⋯:N⋆}

None of the proper subsets of Y is complete, so it is minimal. Yet, the test set X shown earlier seems more “efficient” since it uses only one list of length 3, as opposed to two lists for Y.

### Subsumption

The problem of testing as presented at the beginning of this post consists in distinguishing functions of a given type. To compare the effectiveness of test inputs, we define **subsumption**. The tests \[0,0,1\] and \[0,1,1\] are _subsumed_ by \[0,1,2\], meaning that the latter one discriminates polymorphic functions on lists at least as well as the former two:

> A test input x **subsumes** y with respect to Tα→Uα if for all f,g:Tα→Uα, f x\=g x⟹f y\=g y.

Exercise: x:TA subsumes y:TB if and only if f x determines f y, i.e., there is a function ιx,y:UA→UB such that for all f:Tα→Uα, we have f y\=ιx,y (f x).

As a more general example[2](#fn2), x\=\[0,…,n−1\]:(N<n)⋆ subsumes all lists of length n with respect to α⋆→α⋆. Indeed, thanks to the free theorem for that type, for any list y:A⋆ of length n, we have:

f y\=ιx,y (f \[0,…,n−1\])

where ιx,y:(N<n)⋆→A⋆ maps every index in a list to the corresponding element in y; in Haskell we can define it as:

    iota_x :: [a] -> [Int] -> [a]
    iota_x y = fmap (y !!)

Of course, we can lift subsumption into a relation on sets. Above, X strongly subsumes Y:

> A test set X **strongly subsumes** Y (with respect to Tα→Uα) if every element of Y is subsumed by an element of X.

On a closer look, it is a bit weird: for instance, Y does not strongly subsume the domain ∃α.α⋆, even though it looks much “smaller”. This is why this subsumption is “strong”.

A somewhat more natural but weaker notion of subsumption generalizes the original definition (between single test inputs) directly:

> A test set X (**weakly**) **subsumes** Y (with respect to Tα→Uα) if, for all f,g:Tα→Uα, we have that ∀x∈X.f x\=g x implies ∀y∈Y.f y\=g y.

Complete test sets weakly subsume each other, so it is not a really useful notion for our purposes. We can at least factor it out of the definition of completeness:

> A _complete test set_ for Tα→Uα is a test set which subsumes the domain ∃α.Tα.

When X subsumes Y, every test input y∈Y is “covered” by inputs in X: the value a function takes at y is determined by the values at inputs in X, but not all of them. Indeed, we can witness subsumption in a more fine-grained way.

> A **subsumption** S of Y by X (with respect to Tα→Uα) consists of a subset Sy⊆X for each y∈Y, such that Sy subsumes {y}. We denote subsumptions by S:Y≺X.

Subsumptions subsume the previous definitions of subsumption.

1.  A subsumption of Y by X exists if and only if X subsumes Y. (If X subsumes Y, then there is a trivial subsumption, Sy\=X.)
2.  X strongly subsumes Y if and only if there exists a subsumption S:Y≺X where Sy is a singleton for all y∈Y, i.e., S is actually a function Y→X.

Subsumptions with respect to a given function type form a category. Let R:Y≺X and S:Z≺Y be two subsumptions involving three test sets X,Y,Z. Their composition RS:Z≺Y is given by:

RSz\={x|y∈Sz,x∈Ry}

Exercise: the composition of subsumptions is a subsumption; composition is associative, and has identities.[3](#fn3)

#### A word about emptiness

For α→α, the empty set is complete because there is only one value of that type.

> A type Tα→Uα is **trivial** if it has at most one inhabitant, up to observational equivalence.

A related notion is that of test inputs which provide no information at all.

> A test input x is **isotropic** with respect to Tα→Uα if the empty set subsumes {x}, i.e., ∀f,g:Tα→Uα.f x\=g x.

(0,0) is isotropic with respect to α×α→α. Trivial types are those whose inputs are all isotropic.

### Canonical test sets

Assume Tα→Uα is not trivial.

Ideally, a test set should have no redundancies. Here is one formulation, which may need some improvement:

> A test set X is a **canonical test set** for Tα→Uα if every element of the domain ∃α.Tα is either isotropic or subsumed by only one element of X.

Clearly enough, a canonical test set strongly subsumes every test set. In the examples above, X is a canonical test set. The test set Y for the list example is not canonical: \[0,1,2\] is subsumed by neither of \[0,0,1\] or \[0,1,1\] individually, although the singleton {\[0,1,2\]} is subsumed by the pair {\[0,0,1\],\[0,1,1\]}.

I would like to find a characterization (or generalization) of canonical test sets, justifying their “optimality” categorically. The idea of initial/terminal objects seems relevant, however in the category described above, subsumptions are far from unique: in the case of lists, we can easily construct two subsumptions S,S′:Y≺X, for example with S\[0,0,1\]\={\[0,1,2\]} and S\[0,0,1\]′\={\[0,1,2\],\[0,1\]}.

Going forward, it appears that we can also compare subsumptions between two test sets, and this might give us a 2-category to work with.

Future plans
------------

Two questions:

1.  Does _Testing Polymorphic Properties_ define canonical test sets? Most likely, yes.
2.  What is the relation between “canonical test sets” and “canonical forms”[4](#fn4)? At a high level, they are similar in that both seek to avoid redundancy, which is expressed in one case by subsumption, in the other by observational equivalence. Two consequences we expect to come from this work are: a more general algorithm to test polymorphic properties and a description of canonical forms in System F.

* * *

1.  [\*Testing polymorphic properties\*](http://publications.lib.chalmers.se/publication/99387-testing-polymorphic-properties). Jean-Philippe Bernardy, Patrik Jansson, Koen Claessen. ESOP 2010.[↩︎](#fnref1)
    
2.  The restriction on the type of x, with elements in N<n, is so that ιx,y can be total.[↩︎](#fnref2)
    
3.  Viewing subsumptions S:Y≺X as functions S:Y→PX, that is the Kleisli composition of the power set monad P (a common representation of non-determinism). Thus we have a subcategory of the Kleisli category for the power set monad.[↩︎](#fnref3)
    
4.  [\*Deciding equivalence with sums and the empty type\*](https://arxiv.org/abs/1610.01213). Gabriel Scherer. POPL 2017.[↩︎](#fnref4)


[Source](https://blog.poisson.chat/posts/2017-06-07-canonical-testing.html)