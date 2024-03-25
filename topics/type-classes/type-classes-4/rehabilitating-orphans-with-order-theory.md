---
downloaded:       2021-12-01
author:           
page-url:         https://gitlab.haskell.org/ghc/ghc/-/wikis/Rehabilitating-Orphans-with-Order-theory
page-title:       Rehabilitating Orphans with Order theory · Wiki · Glasgow Haskell Compiler / GHC · GitLab
article-title:    Rehabilitating Orphans with Order theory · Wiki · Glasgow Haskell Compiler / GHC
article-length:   9416
article-created:  {Date-Creation-yyyymmdd}
article-modified: {Date-Revision-yyyymmdd}
desc:             The Glorious Glasgow Haskell Compiler.
---
# Rehabilitating Orphans with Order theory · Wiki · Glasgow Haskell Compiler / GHC · GitLab

The Glorious Glasgow Haskell Compiler.
## Motivation

Without orphans, the dependency graph suffers from linearization: the richer class hierarchy we have, the more we approach a total order where every package is either imported or imports every other. This is catastrophic for productivity.

The great thing about open source, and open source with Haskell in particular, is how much the product of our labor --- the code --- itself synchronizes the laborers --- us. The type systems allows information that humans could never communicate in a "game of telephone" to losslessly flow from package to package, all throughout the dependency tree. But rather than forcing all releases to be tightly coordinated, types and version constraint solving frees us to release libraries fairly independently.

How do we in fact get more asynchronicity out of types, the great synchronizer? I think the answer lies with Order theory. The PVP induces a relation on versions that indicates compatibility. That relation is a partial order. The module `import` and package `build-depends` relations are also partial orders, with a nice isomorphism from the former to the latter. All this allows the ecosystem at large---hackage, let's say---to be seen as a giant CRDT.

From that vantage point, the linearization of the dependency graph directly inhibits the ability of the ecosystem to evolve concurrently, and thus for work to be parallelized.

> TODO some theoretical investigation of:
> 
> -   How breaking changes bubble up
> -   Expected chain length from dependency to a given "goal packages"
> -   How linearization relates to dependency bloat

### `base`

Perhaps the best illustration of these linearization woes is `base`.

Base is a mix of many different things: pure highly portable abstractions, basic concrete types, and highly GHC-specific implementations of things like numbers and IO. None of this really belongs together, and the fact that the unstable GHC-specific portions necessary necessitate breaking changes and major version bumps each release is a huge source of churn and busywork.

But untangling base is nigh impossible because of the very high density of instances precludes this. Not only are the imports linearized, but a satisfactory linear "meandering" through the modules is actually rather difficult to come by. So this unfortunate situation is not only bad for consumers of base, it is also bad for maintainers of base.

Beyond avoiding version number churn, there are also issues with making breaking changes in `base`. Stuff like the Applicative-Monad proposal took years, because each GHC only can work with a single version of base. Other controversies like `Text` vs `String` go unresolved. It is @Ericson214's view that there is no single right answer, because the needs of legacy code and new users are fundamentally irreconcilable.

With orphans made safe to use, all these problems can be solved.

-   `base` can be broken up into many libraries, with the vast majority of packages only needing to depend on the stabler parts.
-   Even those unstable parts are free to make breaking changes faster, especially if we have the bandwidth to maintain multiple release families for a transition period.
-   Dubious decisions like `String`, `head`, and `tail`, etc. can be cornered off.
-   Future versions of `base` can be a thin reexport of these new packages for sake of legacy code without updated `build-depends`

This is perhaps the most dramatic example, but the rest of the ecosystem should benefit from similar untanglings too.

## Background

### World semantics

GHC's focus on checking consistency instance by instance has good performance characteristics, but obscures the theory of the task.

The "world semantics" from Chapter 1, section 4 of \[1\] clarify the situation immensely. \[TODO summarize key points, but really, just go read it.\]

The only quibble I might add is that the multi-param type class example of non-orphans gone wrong can be construed as a semi-orphan: because it only occurs when the type class parameters have instance heads that *wouldn't* pass the orphan checks were they the sole parameter. A stronger orphan check would have required a local-type-guarded argument for every parameter for a non-local type class, and that, while super restrictive, would solve the problem.

Why isn't this approache used in the implementation already? The challenge is that a naive implementation has rather bad asymptotic complexity, as every pair of instances across every pair of imports needs to be checked. Even with the more obvious optimization of only comparing modules that have not been previously compared, that's still O(n^2) edges of the complete bipartite graph.

### Rust

In \[2\], Aaron Turon, one of the core rust designers (as in the ones with the PhDs signing off the theory) makes the connection between modal logic and type classes more specific. For a really quick summary:

-   In both Rust and Haskell today, while the instances themselves are monotonic, the consistency checks aren't and cannot be.
-   Rust prohibits orphans completely, unlike Haskell, but has much more complicated subtle orphan checks, sometimes observing whether the upstream instances (cover as much as they could), to compensate.

I am not advocating for the Rust solution, but I do want to credit them for exploring this path. It's precisely the non-monotonicity of the consistency checks that opens the door to a vast and rich design space, and and as far as I know they recognized it first.

## Solution

### Meet module

The proposed solution is to continue leaning into order theoretic-solutions, letting what's been the basis for the overall good properties of the ecosystem also solve this problem.

Ignoring the minutiae of judging instance heads, the general idea of an instance concerning a type class from module `A` and a type from module `B`, *not* being an orphan is that:

-   Either it's in module `A`, and module `A` imports module `B`
-   Or it's in module `B`, and module `B` imports module `A`

This bares a strong resemblance to properties a property of a lattice meet:

> x ≤ y => (x ∧ y = x, y ∧ x = x)

Now, the notion of meets applies to order elements that are incomparable --- indeed that is it's whole point! --- so the next question to ask is, what does such a meet look like in the Haskell context and what does it mean for coherence?

The results are good: If A ∧ B is exists, and isn't A or B, it is a third module which:

-   imports A and B
-   is imported by ever other module also (transitively) importing A and B

Since neither of A and B imports the other, we know only variable-head non-orphan "flexible" instances could overlap with our prospective instance in A ∧ B. That's a much more limited set of things to check for. Then, since every other module which could also write this instance imports A ∧ B, we can just use our existing per-instance check to ensure they don't!

### Module Meet canonicity?

But how do we know which module is A ∧ B? Checking the laws after the fact is:

-   Non-local, and impossible even in the open-world context packages are written
-   Inefficient even if we bound the check somehow

Instead, we can *pre-declare* meets.

Now, this does sound rather compositional. Have we just transformed a type class consistency problem into some other consistency problem? Well yes, but at least we've boiled down the rather complex question of consistency checks into something much simpler.

And indeed there is precedent for such an approach: rather than writing down complicated propositions about who provides what module, module interfaces, etc., we just use a black box version number.

This is an apt comparison, because meet declarations should live in Cabal files. Any module purporting to be the meet of some other modules should be declared as such in the cabal file. We may also want to also have the coarser notion of "meet packages", and have the meet structure preserved by the coarsening isomorphism:

> module A in package a, module B in package b => module (A ∧ B) in package (a ∧ b)

In practice, [@Ericson2314][1] doesn't anticipate contention over who implements what. Rather than today, where packages begrudgingly and shamefully implement the orphans they need, I expect packages whose *sole purpose* is implementing orphans to be created, and people to collaborate on those packages rather than getting into packaging wars over who implements what instance.

### Incremental verification

What about checks? The more obvious ones are

-   The assignments need to be unique
-   packages should only declare modules they provide

As to verifying downstream imports, the situation gets trickier. Certainly the assignments in upstream packages need to be respected by downstream ones. But in the simplest formation that not only means:

> Don't provide instances you aren't allowed to

But also:

> Don't forget to (transitively) import any declared meets amongst ones imports.

Maybe it's fine, but perhaps that last restriction is a little bit onerous? Say I want to to use `ClassA TypeC`, `ClassB TypeD`, but don't care about `ClassB TypeC` and `ClassA TypeD`? *Open Question*: Perhaps we can prune the module import relation to just concern modules that provide instances, or even just "potentially-overlapping" instances.

## References

\[1\]: Non-Reformist Reform for Haskell Modularity. PhD thesis, Saarland University, 2019. [https://people.mpi-sws.org/~skilpat/papers/kilpatrick-thesis-nov-2019-publication.pdf][2]

\[2\]: Negative reasoning in Chalk, Aaron Turon, 2017. [https://aturon.github.io/tech/2017/04/24/negative-chalk/][3]

[1]: https://gitlab.haskell.org/Ericson2314 "John Ericson"
[2]: https://people.mpi-sws.org/~skilpat/papers/kilpatrick-thesis-nov-2019-publication.pdf
[3]: https://aturon.github.io/tech/2017/04/24/negative-chalk/
