# Implementing and Understanding Type Classes

Implementing and Understanding Type Classes
http://okmij.org/ftp/Computation/typeclass.html

- Introduction
- Dictionary passing
- Type classes as macros
- Intensional type analysis
- The pioneering work of Stefan Kaes
- Brief bibliography
- [Stretching Type Classes](http://okmij.org/ftp/Haskell/TypeClass.html): Unusual applications and arguments about type classes in Haskell


## Introduction

*Computational abstractions*, like higher-order functions, continuations, modules, processes, automatic memory management, have made programs much faster to write, easier to prove correct and to reuse, and yet there is, often subconscious, resistance to abstractions; they appear ritualistic, formal, too abstract.

One gets the feeling of getting lost. To overcome the mistrust for an abstraction it may help to look at its realization, to see what is being abstracted away. The awareness of low-level implementation details brings the appreciation of an abstraction and the intuitive explanation for it.

This is a behind-the-scenes look at the abstraction of *parametric overloading*, aka *bounded polymorphism*, or just *type classes*.

Seeing the implementation makes type classes appear simpler, friendlier, more comfortable to use. The types and type class definitions are no longer incantations to memorize: they suddenly make sense. Knowing what tedious job GHC is doing for us, makes us appreciate the convenience of type classes.

*Dictionary passing*, although best known, is not the only compilation strategy to implement type classes. Historically, the first were *static specialization* and *run-time resolution* (intensional type analysis), both introduced in the pioneering work by Stefan Kaes, the father of parametric overloading. He presented the type system and proved its soundness, described the type inference algorithm, and proved the soundness and consistency of the two implementations of what is now known as type classes. It's a shame that his name is almost forgotten, even though his strategies are still in wide use. *Local type classes and instances* introduced in his paper still await recognition.

Dictionary passing makes it easier, in retrospect, to understand the other two implementation strategies. Therefore, we describe it first and in detail. We explain by example, juxtaposing Haskell code with the corresponding code in OCaml, a language without type classes. The implementation language can be any higher-order language, including *GHC Core*. For the sake of explanation, we restrict ourselves to a single-parameter, non-constructor type classes, such as `Num`, `Eq`, `Show`. The other two implementation strategies are presented next, illustrating the algorithms from the Kaes' paper, in modern terms. We conclude with the brief bibliography of papers that discuss implementing and understanding type classes.

## Dictionary passing

The most well-known technique of implementing type classes is the so-called dictionary passing. Although it was historically the second technic introduced for implementing classes, it is best to study it first as that helps understand the other implementations. This is an exposure of the compilation strategy, explaining what happens with type classes as GHC translates the source code to GHC Core.

```hs
class Show a where
    show :: a -> String

instance Show Bool where
    show True  = "True"
    show False = "False"

instance Show Int where
    show x = Prelude.show x -- internal

-- The first parametrically overloaded function
print :: Show a => a -> IO ()
print x = putStrLn $ show x

-- and its instantiation
test_print :: IO ()
test_print = print True
```

```ocaml
type 'a show = { show: 'a -> String }

let show_bool : bool show =
    { show = function
    | true  -> "True"
    | false -> "False"
    }

let show_int : int show =
    {show = string_of_int}

(* The first parametrically overloaded function *)
let print : 'a show -> 'a -> unit =
    fun {show=show} x -> print_endline (show x)

(* and its instantiation *)
let test_print : unit =
    print show_bool true
```

The type-class declaration `Show a` in Haskell translates to the data type declaration for the record `'a show` (a dictionary) in OCaml. The name of a type class method becomes a label in the dictionary. Comparing the types of the `print` function, in Haskell and OCaml:

    print :: Show a =>  a -> IO ()
    print : 'a show -> 'a -> unit

There are other differences, but the shapes of the arrow stands out the most: `Show a =>` vs `'a show ->`. In both Haskell and OCaml, the function `print` is **bounded polymorphic**: it applies to values of any type, as long as the type is showable. In Haskell, as long as there is an evidence that the type is a member of the `Show` class. In OCaml, it's up to the programer to procure that evidence (i.e. the dictionary), and to explicitly pass it into the `print` function. In contrast, Haskell builds that evidence (i.e. the dictionary) by itself and passes it implicitly around.

OCaml's `print` signature reveals the nature of bounded polymorphism. An *unbounded polymorphic* function such as `id : 'a -> 'a` corresponds to the *universally quantified* proposition `forall a. a -> a`. The `print` witnesses the proposition `a -> unit` quantified only over a part of the domain. 
Then the predicate `show` decides the membership in that part:   
we assert `a -> unit` only when `show(a)`.   
The proposition thus reads: `forall a. show(a) -> (a -> unit)`,   
which is the type of `print` modulo stylistic differences.


## Overloading

Overloading is the ability of a single symbol to have different meaning, as determined by the context in which it appears. Standard examples of overloading include the arithmetic operations that work correctly regardless of the undelying number type. The same symbol is used for addition of integers and for addition of floating-point numbers, although the procedures involved are considerably different. In each case, the intended meaning of the overloaded operator is determined from the types of operands.

If it were not for overloading, we'd have to come up with different names for each type (e.g. addInts, addFloats, etc.). This is exactly what the compiler does because overloading is a compile-time construct, at which time it gets completely resolved. The identifiers that are overloaded the most are function names, which the compiler resolves by consistently renaming them in the so-called *mangling process*. The reverse process of deriving their original names from mangled ones is called *demangling*. The mangling procedure usully consists of prepending a random-looking prefix to a name. Additionally, this prefix may carry additional info about the function identified by that name. Languages that support FFI, which is incompatible with name manglin, usually have some mechanism to turn it off, something akin to the *no-mangle* pragma.

One common approach is to completely resolve overloading at compile time. The compiler installs type specific meanings for all overloaded symbols, based either on type information attached to operands. A significant drawback to this approach is that overloaded operations cannot be abstracted while retaining their overloaded nature.

A more dynamic approach to overloading, which preserves the ability to abstract overloaded definitions, is found in OO languages where the resolution occurs at run-time. There are two particular problems to be dealt with:
* There are many situations when the appropriate interpretation of an overloaded symbol cannot be determined at compile-time; when there is no way to fix a single interpretation of an overloaded symbol.
* Ensuring that only suitable vals/types are used with an overloaded operator.


Standard ML uses two different approaches to overloading:
* The type of each arithmetic operator must be uniquely determined from the context, possibly by explicit type annotation. ML's, compile-time resolution of overloading, is not able to preserve the full applicability of an overloaded operator - one specific implementation of the operator needs to be chosen.
* Standard ML also has a notion of *equality types* to aid with the typing of the equality function. However, they expect a structural definition of equality, which may be undesirable[^1] (the equality of representations vs the equality of the represented thing per se, akin to the equality of numerals vs the equality of numbers).

An alternative approach to the treatment of overloading was introduced by *S.Wadler* and *S.Blott* [^11] based on the notion of a type class and is intended to provide a uniform and general framework for solving exactly these kinds of problems. *Type classes* were first implemented in Haskell [^6] where they are used for dealing with overloading, but also for have other purposes, like helping to produce more clear and modular programs [^7]. Any language, functional or not, based on the Hindley-Milner-Damas type system may be extended to support type classes.

## Type class terms

Considering an operator like the equality, `==`, we note its properties:
* *polymorphic*: use is not restricted to values of any single type
* *overloaded*: interpretation is determined by the types of its args
* *extensible*: the definition can be extended to include new datatypes


**method** 
is a function declared in a class, e.g. `fmap` may be called a method.

**class**
need not have any methods declared, in which case it can still be used to tag types that have certain properties. In this case it is a *behavior-labelling class* (BLC). More commonly, a class does declare (signature only) a few methods, optionally with their overridable definitions (full implementation). In this case it is called the *behavior-enforcing class* (BEC).

**data type**
classes use the same sort of data types used by the ML type system. A type constructor names a data type in the type language while data constructors create values in the expression language.

**instance**
binds a data type to operations which implement the methods of a specified class for that type.



A type parameter like `a` stands for any type at all, so the `id` function is truly *polymorphic*, `id :: a -> a`; it works for all and any type. In opposition to this are the *monomorphic types*, such as `f :: [Int] -> Int`. Type classes are a way to have the middle ground between the two extremes.

The basic idea is to group types according to a shared functionality or behavior. Actually, a type class defines a set of abilities and any type with those abilities can apply for class membership.

There are already many predefined and pre-populated type classes built into Haskell, but users are allowed to define their own.


## Open world assumption

By default, type classes in Haskell use the **open world assumption** in that it is possible to add instances for more types later (not just, e.g. when the class is being declared, or in the same module as the class').

> Sometimes you need a type class with a restricted set of instances, that is, a *closed world assumption*. If the set of instances is restricted, then you can do a complete case analysis of all types that inhabit the class. The reverse is also true: if you can do a complete case analysis on all instance types then you have a *restricted set of instances*. This is the key for declaring such a class. We only declare one method, namely the method for doing a complete case analysis. https://wiki.haskell.org/Closed_world_instances


## Eq class

As an example class, we consider the `Eq` class. Declaring it and populating it, is like having a special set, that is inhabited by `Eq` types only. Its members are only those types for which a suitable definition (that respects the signature) of equality has been implemented, using an instance declaration.

The declaration of the `Eq` class:

```hs
class Eq a where
    (==) :: a -> a -> Bool
```

The header introduces a name for the class and indicates that the type parameter `a` will stand for an arbitrary instance in the signatures of the methods. The indented part then specifies the signatures of the methods associated with the class.

In the general case, we use an expression of the form `C t` to represent the assertion that the type `t` is an instance of the class `C`.

A class may also declare one or more superordinate classes it depends on, called **superclasses**. A superclass dependency does not makes thing particularly more complicated, if anything, it allows for simplification of signatures. For example, the `Eq` is always the superclass to the `Ord` class; therefore the signatures of functions that have an `Ord` constraint, automatically have the implicit `Eq` constraint as well; this also means that such a function automatically has the operators `==` or `/=` at its disposal.

In a class declaration, like for Eq above, the Eq constraint is evident, so it is implicit in the signature of the equals operator. However, (outside the class) the full signature of the two methods (they have the same signature) is actually:

```hs
-- with explicit class constraints
(==), (/=) :: Eq a => a -> a -> Bool

-- and explicit quantification
(==), (/=) :: ∀a. Eq a => a -> a -> Bool
```

By convention, all *free parameters* in a type expression are implicitly bound by a *universal quantifier* (forall, `∀`) at the outermost level. Therefore, `==` is indeed polymorphic in `a`, but then the choice of types for `a` is restricted to instances of `Eq` ("you can choose any color as long as it's black"). Type class constraints like this are often described as the *context* part of a type.

When a class is declared, we can use its methods, directly or indirectly, to define other values, before we have even defined a single instance of the class.

For example, the restriction of types to only the instances of Eq class is reflected in the signature as the context (or constraint).

```hs
member :: Eq a => a -> [a] -> Bool
member x [] = False
member x (y:ys) = x==y || member x ys
```

The user types which are members of a class are defined by a collection of **instance declarations** which may be distributed throughout the program, typically in different modules, when new datatypes are introduced. And for built-in types, the instance declarations are already provided using the primitive functions. For example, the equality of machine integers is already defined in the primitive function `primEqInt`, so the instance declaration of Eq for integers can just reuse that definition/function:

```hs
instance Eq Int where
    (==) = primEqInt
```

More generally, we can define instances of the class Eq for any type, as in the
following definition of equality on lists (even though it's already defined):

```hs
instance Eq a => Eq [a] where
    []     == []     = True
    (x:xs) == (y:ys) = x == y && xs == ys
    _      == _      = False
```

The built-in types already come as instances of all the appropriate built-in classes, and we can make our custom types instances of either those predefined classes or of our own custom classes. Also, we can make builtin types instances of our custom classes.

The set of types defined by a finite collection of instance declarations may be infinite (but recursively enumerable). For example, the definitions given above describe the equality operator for integers, lists of integers, lists of lists of integers and so forth.


## Implementing Overloading

One standard technique used in the implementation of runtime overloading is to attach some kind of *tag* to the concrete representation of each object. Overloaded functions such as the equality operator can be implemented by inspecting the tags of their arguments and dispatching the appropriate function based on the tag value.

Many schemes exist for the encoding of tags to make the tag dispatch effcient. One of the benefits of static type checking is that it provides a compile-time check which ensures that the the equality function will never be applied to an object for which there is no corresponding definition of equality.

Unfortunately, the use of tags has a number of drawbacks. It can complicate data representation and may not be well suited to the underlying hardware. Perhaps more significantly, there are some forms of overloading that cannot be implemented using this approach. In particular, it is not possible to implement functions where the overloading is defined by the returned type. A simple example of this is the `read` function used in Haskell to parse a string as a value of any type that is an instance of the `Text` class, the set of readable (printable) types.

An elegant way to avoid these problems is to separate objects from their tags, treating tags as data objects in their own right. For example, we can implement `read` as a function that takes an extra argument which gives the tag of the result value. This amounts to passing type information around at run-time but this is only necessary when overloaded functions are actually involved. This is potentially more efficient than uniformly tagging every data object regardless how it will be used.

Using this approach, the `member` function in the previous section might be implemented by parameterizing it with the appropriate definition of equality. The tag in this case is the equality function itself, here given as the first param.

```hs
-- Eq constraint
member :: Eq a => a -> [a] -> Bool
member x [] = False
member x (y:ys) = x==y || member x ys

-- appropriate predicate instead of the Eq constraint
member' :: (a -> a -> Bool) -> a -> [a] -> Bool
member' eq x []     = False
member' eq x (y:ys) = eq x y || member' eq x ys

-- We could now evaluate `member 2 [1,2,3]` by rewriting it as
member' primEqInt 2 [1,2,3]
```

For a more interesting example, if `xs` is a list of lists of integers, then we could evaluate: `member [1] [[1],[2],[3]]` in a similar way, rewriting it as:

```hs
member' (eqList primEqInt) [1] xs
    where
        eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
        eqList eq [] []         = True
        eqList eq (x:xs) (y:ys) = eq x y && eqList eq xs ys
        eqList eq _ _           = False
```

The definition of eqList can be obtained directly from the instance declaration on lists in Section 2 in much the same way as the definition of member' was obtained from that of member.

Type classes do not require a particular definition of equality for a data type; any function of the appropriate type can by supplied by the user to check equality.

As a convenience, Haskell allows the programmer to use derived instances for some of the standard classes like Eq, automatically generating appropriate instance definitions.

Note that this feature is not itself part of the underlying type system. One of our goals in the remainder of this paper is to describe how these translations can be obtained automatically as part of the type checking process.


## Static Analysis





















[^1]: A.W. Appel. `A critique of Standard ML`. Princeton University CS-TR-364-92, February 1992.
[^2]: A.W. Appel. `Compiling with continuations`. Cambridge University Press, 1992.
[^3]: L. Augustsson. `Implementing Haskell overloading`. To appear in Conference on Functional Programming Languages and Computer Architecture, Copenhagen, Denmark, June 1993.
[^4]: L. Damas and R. Milner. `Principal type schemes for functional programs`. In 8th Annual ACM Symposium on Principles of Programming languages, 1982.
[^5]: K. Hammond and S. Blott. `Implementing Haskell type classes`. Proceedings of the 1989 Glasgow Workshop on Functional Programming, Fraserburgh, Scotland. Workshops in computing series, Springer Verlag.
[^6]: P. Hudak, S.L. Peyton Jones and P. Wadler (eds.). `Report on the programming language Haskell, version 1.2`. ACM SIGPLAN notices, 27, 5, May 1992.
[^7]: M.P. Jones. `Computing with lattices: An application of type classes`. Journal of Functional Programming, Volume 2, Part 4, October 1992.
[^8]: M.P. Jones. `Qualified types: Theory and Practice`. D. Phil. Thesis. Programming Research Group, Oxford University Computing Laboratory. July 1992.
[^9]: S.L. Peyton Jones and D. Lester. `A modular fully-lazy lambda lifter in Haskell`. Software. Practice and Experience, 21(5), May 1991.
[^10]: S.L. Peyton Jones and P. Wadler. `A static semantics for Haskell (draft)`. Manuscript, Department of Computing Science, University of Glasgow, February 1992.
[^11]: P. Wadler and S. Blott. `How to make ad-hoc polymorphism less ad-hoc`. In ACM Principles of Programming Languages, 1989.
