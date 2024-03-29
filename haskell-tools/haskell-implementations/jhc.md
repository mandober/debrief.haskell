# JHC

https://web.archive.org/web/20160625205953/http://repetae.net/computer/jhc/jhc.shtml

**JHC** is a compiler for Haskell that aims to produce very efficient code as well as explore novel compilation techniques in an attempt to make them practical.

One thing JHC does not aim to be is a toy or proof-of-concept compiler. A lot of the techniques have already had proof-of-concept implementations and JHC aims to determine how to bring them to a full-scale Haskell compiler.

Although JHC is not ready for general use, some of its ideas or code might be useful.

## JHC features

* Full support for Haskell 98, FFI and some extensions (modulo some bugs being worked on and some libraries that need to be filled out)
* Produces 100% portable ISO C. The same C file can compile on machines of different byte order or bit-width without trouble.
* No pre-written runtime; other than 20 lines of boilerplate all code is generated from the *Grin intermediate code* and subject to all code simplifying and dead code elimination transformations. As a result, JHC currently produces the smallest binaries of any Haskell compiler (`main = putStrLn "Hello, World!"` compiles to 6.5 KB vs 177 KB for GHC 6.4)
* No garbage collector. A variant of *Region Inference* is in the works.
* Compilation by transformation with two general intermediate languages.
  - First Intermediate language based on *Henk*, *Pure Type Systems* and the *Lambda cube*. This is similar enough to GHCs core that many optimizations may be implemented in the same way.
  - Second Intermediate language is based on *Boquist's graph reduction language*. This allows all unknown jumps to be compiled out leaving standard case statements and function calls as the only form of flow control. Combined with JHC's use of region inference, this means JHC can compile to most any standard imperative architecture (or language or virtual machine) directly, without special support for a stack or tail-calls.
* *Novel type class implementation not based on dictionary passing* with many attractive properties. This implementation is possible due to the *whole-program analysis* phase and the use of the `lambda-cube` rather than `System F` as the base for the functional intermediate language.
* Intermediate language and back-end suitable for directly compiling any language that can be embedded in the full lambda cube, making things like a compiler for `Cayenne` much more direct. There is no *type erasure* phase, types are erased for the simple reason that values don't depend on them via the standard *dead-code elimination* pass.
* A very modern design, using *rank-n polymorphism*, *monad transformers*, *generic programing*, and *existential types* to make the code very concise and clear and improve code reuseability.
* All *indirect jumps* are transformed away, JHC's final code is very similar to hand-written imperative code, using only branches and static function calls. A simple *basic-blocks analysis* is enough to transform tail-calls into loops.
* Full transparent support for *mutually recursive modules*.

## Type Classes

One of the most novel things about JHC is its implementation of type classes which differs significantly from the standard *dictionary passing* approach used by other Haskell compilers.

JHC's unique approach is made possible by two other design choices:
* the use of a *pure type system with no distinction between types and values*
* the use of *whole-program analysis*

The basic idea is that **instead of passing a dictionary, a case statement directly scrutinizes the type parameter of a function and calls the appropriate overloaded routine directly**.

This has a number of interesting properties
* The number of extra hidden parameters is the number of free type variables in a functions signature rather than the number of class constraints. For example, `(Ord a, Show a, Num a) => a -> a` will only pass a single extra parameter for the type of `a` rather than 3 dictionaries.
* the two indirections (the first one to look up the dictionary, the second one to call the function pointed to in the dictionary) are replaced by a single case of an algebraic data type and calls to statically known functions. This is exactly the transformation that the `GRIN` points-to analysis does, but much sooner and with much better optimization potential. Calls to statically known functions are much more efficient.

Standard *case coalescing optimizations* have a dramatically enhanced effect when dealing with overloaded functions. Consider this code snipped:

    (x * (y + z) / z) :: a

Each of the calls to the polymorphic functions `*`, `+`, and `/` will expand to a case statement on `a`, since all case statements are trivially examining the same value, they are coalesced into a single one.

With dictionary passing, we would have to look up the appropriate entry in the `Num` hidden parameter, the `Floating` hidden parameter, then look up each of `*`, `+` and `/` individually. Under JHC's scheme all of that is statically transformed into a single case on a normal algebraic type. This optimization is a huge win.

JHC's ability to do this comes from the fact that it is statically evident from the case statement that *the type fully determines every polymorphic function on that type*, a property that is lost in the dictionary passing approach since, as far as the compiler is concerned arbitrary functions may be passed in the dictionaries, it doesn't know that they come in specific correlated sets.

*Functional dependencies* actually lead to run-time savings. Each functional dependency transforms into a case statement which may be omitted.

Although a *whole-world analysis* is needed to generate full versions of type class methods, this is actually rarely needed in practice, as it is often the case the compiler is able to statically prove only a certain subset of types are needed at any given point and is able to generate specialized versions on the spot. This is implemented in a manner very similar to *GHC's rules*.

Advanced compile-time and run-time specializations are possible via pragmas.

## E

`E` is a *pure type system* based on `Henk` and the *lambda-cube*. An important property of `E` is that there is *no distinction between types and values*, which is important for JHC's implementation of type classes.

## Grin

`Grin` is basically a *first-order monadic functional language*. It is very similar to the *Graph Reduction Intermediate Language* as defined by Boquist but has a few notable changes:
- it is typed.
- it has multiple return values as a primitive (unboxed tuples)

My target is higher level C or C-- rather than RISC code, so some transformations are less important as the C compiler can be assumed to take care of them.

The terminology and syntax borrows from Haskell's current implementation of monads and do-notation.

Most of the transformations mentioned in Boquist's thesis have been implemented, however certain intermediate states in Boquist's scheme are actually invalid in the strongly typed `Grin` of JHC, so need to be combined or modified.

A whole lot can be learned from the Grin data type and Grin is fully defined by the following:

```hs
infixr 1 :->, :>>=

data Lam = Val :-> Exp deriving (Eq, Ord, Show)

data Exp
    = Exp :>>= !Lam
    | App Atom [Val]      -- ^ handles applications of functions and builtins
    | Prim Primitive [Val]
    | Case Val [Lam]
    | Return { expValue :: Val }
    | Store  { expValue :: Val }
    | Fetch  { expAddress :: Val }
    | Update { expAddress :: Val, expValue :: Val }
    | Error String Ty -- ^ abort with an error message, unrecoverably
    | Cast Val Ty     -- ^ reinterpret Val as a diff ty, also (un)box lifted ty
    deriving (Eq, Show, Ord)

data Val
    = NodeC !Tag [Val]
    | NodeV !Var [Val]
    | Tag !Tag
    | Const Val -- ^ pointer to const data, only Lit, Tag, NodeC may be children
    | Lit !Number Ty
    | Var !Var Ty
    | Tup [Val]
    | ValPrim APrim
    deriving (Eq, Show, Ord)

data Ty
    = TyTag         -- ^ a lone tag
    | TyPtr Ty      -- ^ pointer to a heap location which contains its arg
    | TyNode        -- ^ a whole tagged node
    | Ty Atom       -- ^ a basic type
    | TyTup [Ty]    -- ^ unboxed list of values
    | TyUnknown     -- ^ unknown (undefined) ty, eliminated by code generation
    deriving (Eq, Ord)
```

## Extensions

Jhc implements several standard and new extensions to Haskell 98

* The *FFI* is almost fully supported except for calling Haskell code from C.
* *Hierarchical module names* are supported as described in the addendum. The search algorithm is somewhat different than GHC though, `Control.Monad.Identity` will be searched for as `Control/Monad/Identity.hs`, `Control/Monad.Identity.hs` and `Control.Monad.Identity.hs`, giving you a bit more freedom in laying out your directory structure.
* *Empty data declarations* with no constructors are supported
* *Liberalized type synonyms* are supported (type synonyms may appear anywhere a type may appear)
* `INLINE` and `SPECIALIZE` pragmas
* `unsafePerformIO` and `unsafeInterleaveIO` are supported

New Extensions include
* *foreign primitive*: all primitives are brought into scope with a foreign primitive declaration; these can also be used to gain access to C constants, obviating much of the need for a preprocessor such as `hsc2hs` and allowing portable C code to be generated by JHC.
* `SRCLOC_ANNOTATE` pragma. This is a *generalization of GHCs assert magic*. A function which is given an SRCLOC_ANNOTATE pragma will be replaced with an alternate version that takes the functions use site as an argument. This allows error messages to be in terms of where said function was used. The alternate function is named `[function]_srclocann_` and must be in the same module as the original function. JHC does no checking to ensure both functions have the same effect, it is up to the user to ensure that. An example is:

```hs
head :: [a] -> a
head (x:xs) = x
head [] = error "head: empty list"
{-# SRCLOC_ANNOTATE head #-}

head_srcloc_ann__ :: String -> [a] -> a
head_srcloc_ann__ pos (x:xs) = x
head_srcloc_ann__ pos [] = error $ pos ++ ": head: empty list"

-- Now, calling head on an empty list produces an error message like:
-- "Main.hs:23: head: empty list"
```

* `SUPERSPECIALIZE` pragma. This pragma has the same affect as the `SPECIALIZE` pragma, but in addition to doing compile-time specialization, SUPERSPECIALIZE performs run-time specialization.

A *superspecialized function* will perform a single check against the type it is called with, and depending on that test, it will call a specialized version of the function.

This can be a huge win when working with overloaded numeric types. Consider a matrix-multiply routine: if the type cannot be determined at compile-time, normally, we would be forced to fall back to generic version which may have hundreds of additions and multiplications, each of which must test what type its args are. If we SUPERSPECIALIZE the multiply routine instead, a single run-time test will be performed and a more efficient specialized routine will be used, even if it could not be proven at compile time.

* `MULTISPECIALIZE` pragma. This is equivalent to calling SPECIALIZE against every possible type. It's main cost is compile time and memory usage. It should only be used sparingly as it can lead to quadratic rule explosion in the total number of types in the transitive closure of all imported modules in the worst case.

* `MULTISUPERSPECIALIZE` pragma. This is equivalent to calling SUPERSPECIALIZE against every possible type. If not careful, this can result in massive code bloat but might be a big win in certain cases.


## The story of JHC

When I first started to learn Haskell in 1999, I decided I needed a project. Haskell was my first (modern) functional language and I was seduced by its robust strong type system and efficiency gains. After writing a toy ray-tracer (my usual first project in a new language) it was clear I needed to try something somewhat more challenging and JHC was born.

My reasoning was simple: by writing a Haskell compiler in Haskell I will double my language learning speed since I will not only have to learn how to program in it, by forcing myself to complete a non-trivial project, but also its subtle semantics and dark corners since I actually needed to implement it correctly.

Writing a compiler is also doubly-efficient to begin with, since if you self-compile improvements not only give you a better optimizer, but also speed up your self-compiled compiler. All in all, I figure I was making very good use of my time. For some reason, when I explain my reasoning to other people they look at me like I am crazy, but I can detect no flaw in my logic.

In any case, I worked on JHC on and off for a while, and the project got boosts a few times, such as when `hatchet` was released and I used it to replace my front end.

Recently, with my purchase of a faster machine, actually beefy enough to run JHC, and the realization I was getting good optimizations from my implementation of type classes, combined with the small binary size of produced files, I decided to make a push for JHC to become a usable compiler.

There are still substantial issues which need to be overcome before JHC can be used for general Haskell programing.
- It doesn't scale. Basically since JHC compiles the entire standard library along with your code, even moderately complex programs can be beyond its grasp.
- It takes ridiculous amounts of memory and CPU. A gigabyte of RAM usage is not unheard of.
- It leaks memory. The Region inference algorithm is still in the tweaking stage and programs are known to leak memory. for short running programs, this does not seem to be an issue, but anything expected to perform a lot of reductions will probably run out of heap.
- Arrays are very slow at the moment.
- only about 70% of `nofib` compiles at the moment.
- Horrible error messages. A few programmer errors (and some non-errors) cause the compiler to quit with an 'error' or pattern match failure.

That said, I am releasing it because people might find the ideas interesting or be able to learn from or borrow of the code.

## References

Boquist Thesis
http://www.cs.chalmers.se/~boquist/phd/index.html

Henk paper
http://research.microsoft.com/~simonpj/Papers/henk.ps.gz

Pure Type Systems type checking paper: Secrets of the GHC Inliner
http://www.research.microsoft.com/~simonpj/Papers/inlining/index.htm
CPR and strictness analysis with Horn clauses

Typing Haskell in Haskell
http://www.cse.ogi.edu/~mpj/thih/

Hatchet
http://www.cs.mu.oz.au/~bjpop/hatchet.html

JHC
http://repetae.net/john/computer/jhc
