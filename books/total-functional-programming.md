# Total functional programming

`Total functional programming` - paper by David A. Turner, 2004

## Abstract

The driving idea of FP is to make programming more closely related to math. A program in a FPL like Haskell consists of equations which are both computation rules and a basis for simple algebraic reasoning about the functions and data structures they define. The existing model of FP, although elegant and powerful is compromised by the presence of partial functions. The discipline of total functional programming excludes the possibility of non-termination. Among other things, it also requires a type distinction between data, which is finite, and codata, which is potentially infinite.

## 1. Introduction

The concept of function is usually defined as:

>A function `f` with domain `A` and codomain `B` assigns to each element of `x` of `A` a unique element `f x` of `B`.

A function isn't defined unless its domain and codomain can be inferred, either explicitly or implicitly. A function may be defined as a set of equations, and its args are typically scrutinized by pattern-matching (case analysis).

The `fib` computes the nth Fibonacci number (assuming Peano `Nat` numbers)

```hs
fib :: Nat -> Nat
fib 0 = 0
fib 1 = 1
fib 2 = 1
fib 3 = 2
fib 4 = 3
fib 5 = 5
fib 6 = 8
fib n = fib (n - 1) + fib n
-- in Miranda
fib (n + 2) = fib (n + 1) + fib (n + 2)
```

These equations uniquely define the assignment of values, `fib x` for each `x`. From these equations we can prove various theorems about fibonacci numbers, by using algebraic reasoning and induction. Among such theorems are the values of `fib x` for specific `x`. For example, `fib 20 = 6765`, by using the equations as reduction rules. Thus this fn is thus both a mathematical definition of fib and at the same time an algorithm for computing it. One of the enduring myths about FPLs is that they are somehow non-algorithmic. On the contrary, the idea of FP is to present algorithms in a more transparent form, uncluttered by housekeeping details.

However, unless we optimize it (with e.g. memoization), the above is not an efficient algorithm as it takes time exponential in `n` to compute `fib n`. A more efficient function is a tail-recursive one.

```hs
fib2 :: Nat -> Nat
fib2 n = fibaux n 0 1
  where
  fibaux :: Nat -> Nat -> Nat -> Nat
  fibaux 0 a _ = a
  fibaux n a b = fibaux (n - 1) b (a + b)
```

We would like to confirm that 
`∀ n ∈ ℕ. fib2 n = fib n`. 
The key to this is to prove, that for arbitrary `p`, the theorem 
`∀ n ∈ ℕ. fibaux n (fib p) (fib (p + 1)) = fib (p + n)` holds. 
The proof by induction on `n` is straightforward by using the 
program equations for `fibaux` and `fib` and an induction step.

The seemingly close fit between the code and mathematical reasoning accounts for a large part of the appeal of FP. But there's trouble in the pardise…

```hs
loop :: Int -> Int
loop n = 1 + loop n

-- loop 0 = 1 + loop 0       subtract 'loop 0'
--      x = 1 + x            subtract 'x'
--  x - x = 1                simplify
--      0 = 1
```

We get `0 = 1` from which we can infer anything (ex falsum quidlibet). What giveth? Despite being of type `Int`, as the signature claims, the value of `loop 0` is in fact `⊥` (bottom). Because we allowed unrestricted recursion, we are programming with partial and not mathematical functions.

## 2. Total Functional Programming

In Partial Functional Programming (pFP), each type `T` is lifted `T' = T ⋃ {⊥}` i.e. each type contains an extra element `⊥` that denotes non-termination (errors, errors, exceptions, divergence, etc.). In Total Functional Programming (tFP), `⊥` does not exist.

The data types are those of discrete mathematics, which has the advantages of
- simpler proof theory
- simpler language design
- flexibility of implementation


### Simpler proof theory

We say it's easy to prove things in FP because there are no side effects. But Haskell lacks the rules of standard mathematics. For example, if `e : ℕ`, we cannot assume `e - e = 0` because `e` might be `⊥`. Similarly, we cannot rely on the principle of induction, without taking precautions to deal with the case `n = ⊥`.

```js
P(0)   ∀n. P(n) → P(S n)
------------------------- Nat-Ind
        ∀n. P(n)
```

These problems arise, in different ways, in both strict and lazy languages. In tFP these problems go away because there is no `⊥` to worry about.


### Simpler language design

In pFP programming, we have a fundamental language design choice forced on us at an early stage: 
>(1) Whether to make functional application strict in the argument. 
That is, is it a rule of the language that for any function f

`f ⊥ = ⊥`

SML says yes to this, as does Scheme, while Miranda and Haskell embrace nonstrictness and thus lazy evaluation as the norm, leading to far-reaching differences in programming style.

There are more decisions to make due to `⊥`: 
>(2) Should the product space `A × B` be lifted or non-lifted? 
In Haskell, *the product is lifted*, while in Miranda it is not. 
This affects the behaviour of pattern-matching.

In Miranda, the pattern match 

    f (x, y) = …

is irrefutable because at type `A × B` we have

    (⟘ᴀ, ⟘ʙ) = ⟘ᴀ×ʙ

In Haskell, the product type has an extra `⊥` below `(⊥, ⊥)` and thus, for it, the pattern `(x, y)` doesn't match `⊥` (unless we mark it with a lazy-pattern-match annotation, `~`).

```hs
-- ignores the arg
ignore :: (a, b) -> Int
ignore (x, y) = 5

ignore (1, 2)                  -- 5
ignore (1, undefined)          -- 5
ignore (undefined, undefined)  -- 5
ignore undefined               -- !!EXCEPTION!! ✘

-- irrefutable (lazy) match:
-- The match succeeds immediately, only when
-- the arg is needed the consequences befall
-- (but here it is not ever needed)
lazignore :: (a, b) -> Int
lazignore ~(x, y) = 5

lazignore (1, 2)                  -- 5
lazignore (1, undefined)          -- 5
lazignore (undefined, undefined)  -- 5
lazignore undefined               -- 5 ✔
```


These seemingly trivial decisions can interact in unexpected ways and cause innocent looking programs which work in one system to fail in another (even moving between two languages which are both lazy).

For another example, consider
>(3) Should the conjunction operator consider all cases?
Take for example the `&&` operation on Bool:

```hs
(&&) :: Bool -> Bool -> Bool
True  && True  = True
True  && False = False
False && True  = False
False && False = False

-- but there are more cases to be defined:
⊥ && y = ???
x && ⊥ = ???
```

Considering the possible values for these (which are constrained by monotonicity) gives us a total of 4 different possible versions of `&&`:
1. doubly strict
2. left-strict
3. right-strict
4. doubly non-strict (parallel)

Most PLs opt for the *left-strict* (left to right) version, but this is completely arbitrary and breaks the symmetry which `∧` has in logic.

In tFP these semantic choices go away. There is only one possible definition of the product type `A × B`, and only one `&&` operation exists, defined by its actions on `True` and `False` alone. We no longer have a split between strict and non-strict languages.

>In tFP, every exp of a well-typed program has a proper value, and the choice between normal and applicative order evaluation cannot affect the result.


### Flexibility of implementation

>In TFP, reduction is strongly Church-Rosser.

Note the distinction between the *Church-Rosser property* and the *strong Church-Rosser property*.

#### Church-Rosser property

>The Church-Rosser property: If a redex can be reduced in two different ways, and they both produce normal forms, these NFs will be the same.

More precisely, 
the Church-Rosser property 
(aka the Diamond property) 
of a reduction relation `-->⃰` 
(reduction in any number of steps) 
says 
if `A -->⃰ M` ⋀ `A -->⃰ W`, 
then there exists some `B` 
so that `M -->⃰ B` ⋀ `W -->⃰ B`.

```
    A
   / \
  /   \
 /     \
M       W
 \     /
  \   /
   \ /
    B
```

With the Church-Rosser property, *normal forms are unique but they need not exist*; and when they do exist, not every reduction sequence will reach them.

#### Strong Church-Rosser property

>The strong Church-Rosser property: Every reduction sequence leads to a *unique* normal form.

With strong Church-Rosser property, *normal forms are unique*, plus we have strong normalization: *normal forms always exist*, and redexes may be evaluated in any order. ⚜

The choice of evaluation order becomes a matter for the implementor - it cannot affect the semantics of the language. This gives much greater freedom to implementors to choose the efficient strategy, perhaps, to improve space complexity or to achive better parallelization.

An alternative name for TFP, inspired by the Strong Church-Rosser property, is *Strong Functional Programming*, by contrast with the conventional *Weak FP*.


### Disadvantages of tFP

There are some issues with tFP
- A tFP language is no longer Turing complete
- If all programs terminate, how do we write, e.g. an operating system

We will return to the first point later, but the resolution of the second point is codata. We'll need codata, as well as data. However, unlike in weak FP, they will be kept separate: finite data and infinite codata (but no partial data - tertium non datur).

There already is a powerful theory of tFP which has been extensively studied: *Constructive Type Theory* by Per Martin Lof 
(of which there are several different versions).

CTT (or MLTT) includes
- dependent types (types indexed over values; types can depend on values)
- second-order types
- propositions-as-types (an isomorphism between types and propositions that enables programs to express proof information)


The MLTT was developed as a foundational language for constructive mathematics. And it is possible to program in it:

[Nordst90]: "Programming in Martin-Lof's Type Theory: An Introduction", B. Nordstrom, K. Petersson, J. M. Smith, 1990


## 3. Elementary TFP

This paper proposes something much more modest than CTT, namely an elementary discipline of tFP.

The term "elementary" here means:

* Type structure no more complicated than Hindley-Milner, or one of its simpler variants. So we have types like `Nat → Nat`, and polymorphic types like `α → α`, but nothing beyond that.

* Programs and proofs will be kept separate, as in conventional programming. What we are looking for is essentially a strongly terminating subset of Haskell (or for that matter SML, since the difference between strict and lazy disappears in strong FP).


### 3.1 Rules for Elementary Total FP

- means to define data types
- convenience syntax sugar
- built-in primitive types and operations for efficiency
- functions defined by equations
- use of pattern matching to scrutinize args
- use of guards
- descending recursion


First, we must be able to define data types

```hs
data Bool     = False | True
data Nat      = Z     | S Nat
data List a   = Nil   | Cons a (List a)
data Tree     = Leaf  | Node Nat Tree Tree
data Array a  = Bounds Nat Nat (Nat -> a)
-- etc.
```

As usually, some types like `Nat` and `List` will be built in, with special syntax for convenience (so `3` instead of `S (S (S Z))`) and correspondingly, some primitive operations such as (+), (−) and (>) on `Nat` will be built-in for efficiency, although they could easily be defined. Functions are defined by the usual style of equational definition using pattern matching over data types.


>There are 3 essential restrictions to maintain *totality*
1. Case exhaustion (coverage)
2. Covariant type recursion (positivity)
3. Recursion variable gets smaller


1. All case analysis must be complete (case exhaustion). 
When a function is defined by pattern matching, every data ctor of the arg data type must be covered. Also, the set of guarded alternatives must contain the terminating `otherwise` case. In the same spirit, any builtin function must be total. Totality requirement may involve making some non-standard decisions, like forcing that `0 / 0 = 0`.

- case exhaustion: case analysis in pattern matching must be complete
- guarded alternatives must contain the terminating 'otherwise' clause
- dealing with partial functions (turning them into total functions)
  - undefined values of math functions, e.g. division by zero
  - using ℕ when appropriate (not always ℤ)
  - partiality of selector functions on sum types
- recursion
  - restriction to well-founded recursion
  - restriction to structural recursion
  - restriction to primitive recursion
  - restriction on recursive function definitions
  - recursive var must keep on getting smaller
  - covariant only type recursion
  - descending recursion


For example, `head` is a partial function that can be made total in various manners, all of which are artificial, except dependent types which simply outlaw the empty list case. Another solution would be to somehow modify the type system to admit subtypes - such as non-empty-List, on which `head` is well-defined.


2. **Type recursion must be covariant**

That is, type recursion through the lhs of `→` is not permitted.

Contravariant types like `Spin` allow `⊥` to sneak back in, and are therefore banned. We show how the damage arises:

```hs
data Spin a = Spun (Spin a -> a) -- must be disallowed

bad :: Spin a -> a
bad (Spun f) = f (Spun f)

foo :: a -- ⟘
foo = bad (Spun bad)
```

We have obtained a value, `foo`, of type `a`, with no normal form - using the equation for `bad` to rewrite `foo` gets back the same term. This will work for any type `a`, like `Nat` - so we'll have an expression of type `Nat` which does not reduce to a numeral.

The restrictions on recursion which we introduce next (rule 3) will not prevent this, since the definitions of `bad` and `foo` above are not recursive. A modification of the above scheme gives a *fixpoint operator*, equivalent to having general recursion.

Finally, it should be clear that we also need some restriction on recursive function definitions. Allowing unrestricted general recursion would bring back `⊥`. To avoid non-termination, we must restrict ourselves to *well-founded recursion*. 

How should we do this? If we were to allow arbitrary well-founded recursion, we would have to submit a **proof that each recursive call descends on some well-founded ordering**, which the compiler would have to check. We might also have to supply a proof that the ordering in question really is well-founded, if it is not a standard one.

But this contradicts our requirement for an elementary language, in which programs and proofs can be kept separate. We need a purely syntactic criterion, by which the compiler can enforce well-foundedness.

3. Each recursive function call must be on a syntactic subcomponent of its formal parameter.

This form of recursion, often called *structural recursion*, sits naturally with function definition by pattern matching.

In the case of a function of multiple arguments we also permit "nested" structural recursion as in Ackermann's function

```hs
ack :: Nat -> Nat -> Nat
ack 0 n = n + 1
ack m 0 = ack (m - 1) 1
ack m n = ack (m - 1) (ack (m + 1) (n - 1))
```

the extension to multiple args adds no power, because what it does can be desugared using higher order functions, but is syntactically convenient.

The rule to allow recursion only by syntactic descent on data constructors effectively restricts us to *primitive recursion*, which is guaranteed to terminate.

The stand that primitive recursion is too weak (e.g. that Ackermann function cannot be expressed using primitive recursive) is false; it is actually a first order result, and it doesn't apply to a language with higher-order functions.

⚜

We are working in a higher-order language, so what we actually have are the *primitive recursive functionals of finite type*, as studied by Godel in his `System T`.

These are known to include every recursive function whose totality can be proved in a first order logic (starting from the usual axioms for the elementary data types, e.g. Peano axioms for Nat).

So Ackermann is there, and much, much else. Indeed, we have more than system T, because we can define data structures with functional components, giving us infinitarily branching trees.

Depending on the exact rules for typechecking polymorphic functions, it is possible to enlarge the set of definable functions to all those which can be proved total in second-order arithmetic.

Thus, it seems the restriction to primitive recursion does not deprive us of any functions that we need, but we may have to code things in an unfamiliar way.

It is an open question whether it gives us all the algorithms we need - this is a different issue, as it relates to complexity and not just computability.

⚜

#### Example: Quicksort

Quicksort is not primitive recursive. However Treesort is primitive recursive (we descend on the subtrees) and for each version of Quicksort there is a Treesort which performs exactly the same comparisons and has the same complexity, so we haven't lost anything.


#### Example: fast exponentiation

```hs
pow :: Nat -> Nat -> Nat
pow x n | n == 0 = 1
        | odd n = x * pow (x * x) (n / 2)
        | otherwise = pow (x * x) (n / 2)
```

This definition is not primitive recursive - it descends from `n` to `n / 2`. Primitive recursion on nats descends from `succ n` to `n`. However, we can recode by introducing an intermediate data type `List Bit`, and assuming a built in function that gives us access to the binary representation of a number.

```hs
data Bit = On | Off

bits :: Nat -> [Bit] -- assumed

pow :: Nat -> Nat -> Nat
pow x n = pow1 x (bits n)
  where
  pow1 x Nil = 1
  pow1 x (Cons On y) = x * pow1 (x * x) y
  pow1 x (Cons Off y) = pow1 (x * x) y
```

⚜

Summary of programming situation:

* *Expressive power* - we can write any function which can be proved total in the first order theory of the (relevant) data types. (fact, due to Goedel)

* *Efficiency* - it is a readily observed that three quarters or more of the algorithms we ordinarily write are already primitive recursive. Many of the others can be reexpressed as primitive recursive, with same computational complexity, by introducing an intermediate data structure.


I believe it would not be at all difficult to learn to program in this discipline, but you do have to make some changes to your programming style. And it is sometimes quite inconvenient - for example Euclid's algorithm for gcd is difficult to express in a natural way).

>There is also a sledge-hammer approach that can be used to rewrite as primitive recursive any algorithm for which we can compute a prinitive recursive upper bound on its complexity. We add an additional parameter, which is a natural number initialised to the complexity bound, and count down on that argument while recursing. This wins no prizes for elegance, but it is an existence proof.

The problem of writing a *decision procedure to recognise structural recursion in a typed lambda calculus with case-expressions and recursive, sum and product types* is solved in the thesis of Andreas Abel [Abel 1999].

Adapting it to cope with a richer type system and a more equational style of function definition would be non-trivial but probably no harder than things that FPL compilers already do.


### 3.2 Proofs

Proving things about programs written in this discipline is straightforward. Equational reasoning, starting from the program equations as axioms about the functions they define. For each data type we have a **principle of structural induction**, which can be read off from the type definition. For example, `data Nat = Zero | Suc Nat`, gives us, for any property `P` over a Nat `n`:

```js
P(Z)
∀n.P(n) -> P(S n)
-----------------
∀n.P(n)
```

We have no `⊥` and no domain theory to worry about. We are in standard (set theoretic) mathematics.

## 4. Codata

An operating system can be considered as a function from a stream of requests to a stream of responses. To program things like this functionally we need infinite lists - or something equivalent to infinite lists.

In making everything well-founded and terminating we have seemingly removed the possibility of defining infinite data structures. To get them back we introduce **codata type definitions**:

```hs
data List a = Nil | a : List a

codata Colist a = Conil | a <> Colist a

data Stream a = a :> Stream a
```

>Codata definitions are equations over types that produce final algebras, instead of the initial algebras we get for data definitions.

So the type `Colist` contains all the infinite lists (co-lists) as well as finite ones (lists); to get the infinite lists alone we would omit the `Conil` alternative. The infix `<>` is the co-constructor for co-lists.

### 4.1 Programming with Codata

The rule for *primitive corecursion on codata* is the dual to that of primitive recursion on data: **instead of descending on the arg, we ascend on the result**

```hs
f :: something -> Colist Nat
f args = RHS (f args')
```

where the leading operator of the context `RHS(..)` must be a co-constructor, with the corecursive call to `f` as one of its arg. There is no constraint on the form of `args'`.

Notice that corecursion creates (potentially infinite) codata, whereas ordinary recursion analyses (necessarily finite) data. Ordinary recursion is not legal over codata, because it might not terminate. Conversely, corecursion is not legal if the result type is data, because it is finite.

Now we can define infinite structures, such as

```hs
ones :: Colist Nat
ones = 1 <> ones

fibs :: Colist Nat
fibs = f 0 1
  where
  f a b = a <> f b (a + b)
```

Note that all our infinite structures are total.

As in the case of primitive recursion over data, the rule for *coprimitive corecursion over codata* requires us to rewrite some of our algorithms, to adhere to the discipline of total functional programming. This is sometimes quite hard - for example rewriting the well known sieve of Eratosthenes program in this discipline involves coding in some bound on the distance from one prime to the next.

There is a principle of coinduction, which we use to prove infinite structures equal. It can be read off from the definition of the codata type. 

Does the introduction of codata destroy strong normalisability? No! But you have to have the right definition of normal form. Every expression whose principle operator is a co-constructor is in normal form.

To get confluence as well as strong normalisability requires a little more care.*Each corecursive definition is translated into a closed term and an explicit unwind operation introduced* - see [Telford and Turner 1997] for details.

The scheme in [Wadler et al. 1998] for translating lazy definitions into a strict language is also relevant.

### 4.2 Coinduction

First we give the definition of *bisimilarity* (on colists).

We can characterise the bisimilarity relation (`≈`, here `~`) as follows:

`x ~ y  -->  head x = head y  ⋀  tail x ~ tail y`

Actually this statement is itself a corecursive definition!

To avoid infinite regress, we say that anything obeying this statement is a bisimulation, and by bisimilarity we mean the largest such relation. For a fuller discussion see [Pitts 1994].

Assuming the understanding of how to avoid logical regress, we say that

>In general, two pieces of codata are bisimilar if 
>their finite parts are equal, and 
>their infinite parts are bisimilar

The *principle of coinduction* states that bisimilar objects are equal. One way to understand this principle is to take it as the *definition of equality on infinite objects*.

We can package the definition of bisimilarity and the principle that bisimilar objects are equal, in this method of proof:

>In proving the equality of two infinite structures, we may assume (coinductive hypothesis) the equality of recursive substructures of the same form.

For colists: to prove

`g x₁ … xₙ = h x₁ … xₙ`

it is sufficient to show

`g x₁ … xₙ = e <> g a₁ … aₙ`    
`h x₁ … xₙ = e <> h a₁ … aₙ`

There is a similar rule for each codata type.


#### An example of a proof by coinduction

The following theorem about the standard functions `map` and `iterate` is from[Bird and Wadler 1988]. We have changed the name of `map` to `comap` because for us it is a different function when it acts on colists.

```hs
iterate f x = x <> iterate f (f x)

comap f (x <> xs) = f x <> comap f xs

Theorem:
  iterate f (f x) = comap f (iterate f x)

Proof by coinduction:
  iterate f (f x)
  = f x <> iterate f (f (f x))          {iterate}
  = f x <> comap f (iterate f (f x))    {hypothesis}
  = comap f (x <> iterate f (f x))      {comap}
  = comap f (iterate f x)               {iterate}
  QED
```

The proof given in Bird and Wadler uses the "take-lemma", and it is longer than this one, requiring an auxiliary construction, involving the application of the `take` function to both sides of the equation, and an induction on the length of the `take`.

The absence of a base case in this form of induction is at first sight puzzling. It is important to note that

>Coinduction is valid only for the proof of equations over infinite structures, not of arbitrary properties of the data structure as with ordinary induction.

The "strong coinduction" principle illustrated here seems to give shorter proofs of equations over infinite lists than either of the proof methods for this which have been developed in the theory of weak functional programming - namely *partial object induction* [Turner 1982] and *the take-lemma* [Bird and Wadler 1988].

The framework seems simpler than previous accounts of coinduction - see for example [Pitts 1994], because we are not working with domain theory and partial objects, but with the simpler world of total objects.

Moral: Getting rid of partial objects seems an unmitigated blessing - not only when reasoning about finite data, but perhaps even more so in the case of infinite data.

## 5. Beyond structural recursion

The restriction to structural recursion is sometimes frustrating. If the compiler can understand that `n − 1` is smaller than `n` (for positive integer `n`) why can it not see that `n / 2` also descends from `n` (again for positive `n`)? This would enable the straightforward definition of fast exponentiation to be accepted, without our having to introduce the intermediate data type (List Bit). A similar consideration applies to partitioning a list into two non-empty parts as in Quicksort.

A significant result in this area is the paper [Arkoudas and McAllester 1996] defining a decision procedure for *Walther recursion*, a generalisation of primitive recursion.

By a *reducer-conserver analysis* of the program the properties of descending in size from its argument and conserving the size of its arg (in a sense of "size" appropriate to the data type) is transmitted from one function to another.

For example from knowing that `a − b` descends from `a` (for positive integers `a` and `b`, and `−` as natural subtraction) and examining the definition of integer division as repeated subtraction it is inferred that `n / 2` descends.

Arkoudos & McAllester argue that Walther recursion adds no power because the recursions it accepts can be translated into primitive recursion - but it adds convenience! Their system will recognise as well-founded Quicksort, gcd by Euclid's algorithm and many similar examples. The programs are expressed in a first order, monomorphic, functional language (essentially a simple subset of LISP).

Their system permits (and requires) the identification of simple subtypes, such as non-empty list or non-zero natural number, that are relevant to the analysis. 

For example natural subtraction is a conserver over natural numbers but a reducer over positive numbers and (perhaps rather inconveniently) has to be defined separately for these two types.

Generalising the definition and decision procedure for Walther recursion to a higher order polymorphic language is an important and unsolved challenge. This would certainly make elementary tFP a more attractive proposition by admitting a wider and more natural class of recursive definitions.

A method of abstract interpretation due to Alastair Telford captures much of the same ground as Walther recursion for a simple higher order (but still monomorphic) programming language, see [Telford and Turner 2000].

There is a version of Telford's *abstract interpretation scheme for codata*.

Interestingly, this recognises a class of valid corecursive definitions which includes primitive corecursion but also allows other examples such as

    evens = 2 <> comap (add 2) evens

which fails to be primitive corecursion because of the intervening call to `comap`. The definition is neverthless productive because of a conservative property of `comap`. See [Telford and Turner 1997].

>Corecursion may be *productive*, which is the dual concept to well-founded recursion. Productivity, like well-foundation, is undecidable in general.

This suggests there is a notion of Walther corecursion which works analogously to the way in which Walther recursion extends the scope of primitive recursion.

## 6. Observations and Concluding Remarks

I have outlined an elementary discipline of total functional programming, in which we have finite data and possibly-infinite codata, which we keep separate from each other by a minor variant of the Hindley-Milner type discipline.

There are syntactic restrictions on recursion and corecursion to ensure well-foundation for the former and productivity for the latter and simple proof rules for both data and codata.

Although the syntactic discipline proposed may be found too restrictive in the forms of recursion and corecursion it allows, I would argue that the istinction between data and codata is very helpful to a clean system for functional programming and is in fact necessary within a framework ensuring totality.

The attraction of an elementary total language is primarily pedagogical. In the presence of dependent types the expressive power of structural recursion is greatly enhanced, see for example [McBride 2003] for an illustration.

A question we postponed from section 2 is whether we ought to be willing to give up Turing completeness. Anyone who has taken a course in theory of computation will be familiar with the following result, which is a *corollary of the Halting Theorem*:

>Theorem: For any language in which all programs terminate, there are always- terminating programs which cannot be written in it - among these are the interpreter for the language itself.

So if we call our proposed language for total functional programming, `L`, an interpreter for `L` in `L` cannot be written. Does this really matter? I have two observations which suggest this might in fact be something to which we could accommodate ourselves quite easily.

1. We can have a hierarchy of languages, of ascending power, each of which can express the interpreters of those below it. For example if our language L has a first order type system, we can add some second order features to get a language L2, in which we can write the interpreter for L, and so on up. Constructive type theory, with its hierarchy of universes, is like this.

2. We can draw an analogy with the (closely related) issue of compile-time type systems. If we consider a complete computing system written in a typed high level language, including operating system, compilers, editors, loaders and so on, it seems that there will always be at least one place - in the loader for example - where we are obliged to escape from the typing discipline. Nevertheless many of us are happy to do almost all of our programming in languages with compile time type systems. And on the rare occasion we can use an escape hatch, such as Haskell's `UnsafePerformIO`.


There is a dichotomy in language design, because of the halting problem. 
For our programming discipline we are forced to choose between
- Security: a language in which all programs are known to terminate
- Universality: a language in which we can write
  - all terminating programs
  - silly programs which fail to terminate

and, given an arbitrary program we cannot in general say if it is one or the other. Five decades ago, at the beginning of electronic computing, we chose universality. But if today we can write all programs, bar a few special situations, it may be time to reconsider this decision.


## References

[McBrid03]: 2003 First Order Unification by Structural Recursion - Conor McBride
[TelTur00]: 2000 Ensuring Termination in ESFP - David Turner, A. J. Telford
[Wadler98]: 1998 How to add laziness to a strict language without even being odd - Philip Wadler, Walid Taha, David McQueen
[AnAbel99]: 1999 A Semantic Analysis of Structural Recursion - Andreas Abel
[BirWad88]: 1998 Introduction to Functional Programming - R. S. Bird, P. Wadler
[TelTur97]: 1997 Ensuring Streams Flow - David Turner, A. J. Telford
[ArkAll96]: 1996 Walther Recursion - Kostas Arkoudas, David McAllester 
[Turner95]: 1995 Elementary Strong Functional Programming - David Turner
[PittsA94]: 1994 A Co-induction Principle for Recursively Defined Domains - A. M. Pitts
[Nordst90]: 1990 Programming in Martin-Lof's Type Theory: An Introduction - B. Nordstrom, K. Petersson, J. M. Smith
[Runcim89]: 1989 What about the Natural Numbers - Colin Runciman
[Turner86]: 1986 Turner "An Overview of Miranda - David Turner
[Turner82]: 1982 Functional Programming and Proofs of Program Correctness - David Turner
[GodelK58]: 1958 On a hitherto unutilized extension of the finitary standpoint - Kurt Godel
