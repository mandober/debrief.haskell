# Lightweight Static Guarantees

> The programming style for static assurances, using types to carry capabilities, which are mere references to specifications rather than actual specifications

We have identified (more than a decade ago, in fact) a disciplined programming style that uses _existing_ type systems in practical, mature languages (such as OCaml, Scala, Haskell, etc., and to an extent, Java and C++) to _statically_ assure a wide range of safety properties:

*   never dereferencing a null pointer or taking the head of an empty list;
*   always sanitizing user input;
*   using only in-bounds indices to access (dynamically allocated) arrays of the statically unknown size.

The technique is compatible with modular development, separate compilation, imperative code, native mutable arrays, indirect indexing, and general recursion. The resulting program is not just more reliable but also more efficient, because fewer run-time checks are needed. The technique does not obviate the foundational, formal methods approach; rather, it complements and structures it. Formal methods may still be used to prove the (typically small and simple) security kernel correct. Our technique extends the static guarantees from the kernel through the whole program.

There are two surprises: (i) what we enumerate is even possible; (ii) that it is so old (tracing back to Morris' 1973 paper), so simple, and yet so rarely used.

*   [Introduction](#introduction)
*   [User-input sanitation and SQL injection](#dirty-string)
*   [Abstract Types -- the language protection layer](#ADT)
*   [Eliminating array bound checks](#safe-array-access)
*   [Eliminating array bound checking in multiple arrays](#multiple)
*   [Knuth-Morris-Pratt string search with safe array access](#KMP)
*   [Rights amplification](#amplification)
*   [Lightweight static capabilities](#lsc)
*   [Formalization of the abstract type protection](#formalization)
*   [The question of verification](#verification)
*   [Lightweight guarantees and static analyses](#range-analysis)
*   [Lightweight guarantees and dependent types](#deptype)
*   [Conclusions](#conclusion)

* * *

Introduction
------------

The memory of Heartbleed, a flaw in the OpenSSL library, is beginning to fade. After all, exploitable security flaws are regular occurrence. Heartbleed is still remarkable in how much effort was spent to patch it, and how trivial it was.

Like many other flaws of that nature, it was introduced without much ado. As the developer of the Heartbeat SSL feature explained, \`\`I was working on improving OpenSSL and submitted numerous bug fixes and added new features. In one of the new features, unfortunately, I missed validating a variable containing a length.'' A reviewer \`\`apparently also didn’t notice the missing validation,'' the developer said, \`\`so the error made its way from the development branch into the released version.'' The error remained (publicly) undetected for two years.

The OpenSSL patch that finally fixes the error shows how indeed trivial it was: just one statement

    memcpy(bp, pl, payload);

that copies the payload data of the size `payload` from the input packet (starting at the pointer `pl`) to the output packet buffer, starting at the location `bp`. The value of `payload` has been previously read from the input packet. The problem comes when the attacker sends a packet with the maximum value of the payload size, but with no actual payload data. In that case `memcpy`, instead of the data from the received packet (which is already ended), copies the large amount of garbage from the openSSL input buffer. That \`garbage' is actually left-over data, with often sensitive information including passwords.

What is also disturbing about Heartbleed is how trivially the error could have been avoided, if the low-level functions like `memcpy` could not be invoked directly, but only through wrappers, which, for example, sanity check that `pl + payload` is still within the input packet (the boundaries of input packet are readily available). The invocation restriction could be effected in any language with module/namespace abstraction facilities (C++ and beyond) -- or even in C, when test-compiling against appropriately set up `.h` files that omit restricted functions. The performance need not be sacrificed: the wrapper could be an inline function (or a C macro), and the length sanity check has to be done anyway.

Abstraction is the key word: Abstraction over internal data and functions forces the end programmer to use public APIs with safety checks. By preventing tampering with the internal state, abstraction also ensures that whatever invariant was verified by the safety checks remains valid. Therefore, the safety checks do not have to be done over and over again -- or even at all, if the needed invariant follows from the checks the algorithm had to do anyway. Hence our slogan: **Safe and Efficient, Now**.

The basic idea behind our programming style is ubiquitous, going back to the dawn of computing: the hardware-enforced access restriction to memory and devices. This hardware protection layer separates the computing system into the (trusted) kernel (which operates in the privileged mode and may execute low-level operations) and \`user-level' programs, which may access devices only through public kernel APIs (system calls), which do sanity and access checks. A user program may not write onto a disk at will. Rather, it has to do an \`open' system call, which, after authorization and other checks, returns an opaque token, the file descriptor. The descriptor acts as a _capability_ to do the prescribed set of operations. It also represents the fact of the successful authorization, so further operations do not have to repeat it. James Morris' 1973 paper \`Protection in Programming Languages' was the first to apply to software the ideas from operating systems such as memory protection, authentication, capabilities, scope control. The paper demonstrated how the software/language protection layer helps us reason about programs locally, modularly. The abstraction facilities of programming languages became more extensive since 1973. It is high time we took the full advantage of Morris' insights.

We can do it right now, in existing, mature programming languages. Making sure that we:

*   dereference only non-null pointers;
*   divide by only non-zero denominators;
*   take the head or tail of only non-empty linked lists;
*   index into a static buffer with only in-range indices; and
*   execute SQL commands containing no unsanitized strings from external input.

can be done already in C++ and Java. We show examples below.

We can also do more. The facilities for generic programming (parametric polymorphism) or advanced module systems in modern languages let us assure more safety properties, such as safe indexing into an array whose size is known only at run-rime -- without imposing a bound check on each access. We even show the assuredly buffer-overflow--free KMP string search, which is a rather complex imperative algorithm with indirect indexing.

Morris' old ideas do indeed work. Why don't we use them?

**References**

\`\`Costly error -- Heartbleed developer explains OpenSSL mistake that put Web at risk. \`Trivial' coding error in open source project wasn't intentional, report says''  
Jon Brodkin. Ars Technica, Apr 11, 2014 2:45 pm UTC  
<[http://arstechnica.com/information-technology/2014/04/heartbleed-developer-explains-openssl-mistake-that-put-web-at-risk/](http://arstechnica.com/information-technology/2014/04/heartbleed-developer-explains-openssl-mistake-that-put-web-at-risk/)\>  
(Surprisingly many of the \`explanations' of the problem one may find on the internet are wrong, falsely blaming `OPENSSL_malloc`)

The openSSL patch that fixes the Heartbleed (moving from OpenSSL revision 1.0.1f to 1.0.1g)  
<[https://github.com/openssl/openssl/commit/96db9023b881d7cd9f379b0c154650d6c108e9a3#diff-2](https://github.com/openssl/openssl/commit/96db9023b881d7cd9f379b0c154650d6c108e9a3#diff-2)\>

<[https://xkcd.com/1354/](https://xkcd.com/1354/)\> How the Heartbleed Bug Works

User-input sanitation and SQL injection
---------------------------------------

Heartbleed was just one instance of neglecting to validate/sanitize data received from the user or network. Another, unfortunately all-too-common instance is the input injection attacks -- in particular, SQL injection, \`Bobby Tables'--like attacks. I have implemented the static SQL injection prevention in a web application server used in production. The implementation turned out straightforward and instructive.

I have written a web application server, which has been constantly running in production for at least eight years. The server makes SQL queries using the data from user-submitted requests. SQL injection and input validation had to be taken seriously. About half-way through the development I decided to try the \`software protection layer': make the type-checker alert me to a missing input validation.

The idea was easy to try: the simple library below is essentially it. (The shown code is OCaml; it could have been C++, Java, Scala, or many other languages.)

    module DirtyString : sig       (\* interface \*)
      type dirty\_string               (\* abstract \*)
      val dirty       : string -> dirty\_string
      val escape      : dirty\_string -> string
      val read\_int    : dirty\_string -> int option
      val read\_ncname : dirty\_string -> (string \* dirty\_string) option
    end = struct                   (\* implementation \*)
      type dirty\_string = string
      let dirty = %identity
      let read\_int = int\_of\_string\_opt
      ...
    end

The method `dirty` taints a given string, turning it into a value of the _abstract_ `dirty_string` type. Once the string is dirty, there is not much one can do with it: only apply `escape`, `read_int`, and `read_ncname` methods. The latter attempts to read a sequence of alphanumeric, \`safe' characters, returning them as a string, paired with the remainder of the input dirty string. An `ncname` string can be used in building SQL queries worry-free, and hence is returned as the ordinary, \`clean' string.

In the `DirtyString` implementation, within the \`kernel', `dirty_string` is just the ordinary string. Likewise, the `read_int` method is nothing but the corresponding function from the standard library. When the module is inlined, there is really no overhead. There is still protection: to the users of the library, `dirty_string` and `string` are different, not interchangeable types.

After I wrote the `DirtyString` library, I `dirty`\-ed the result of network reading functions, and started the recompilation watching for type errors. Every place in the code that was using user input was flagged by the type checker: a `dirty_string` cannot be used as an ordinary string. Indeed, some sort of validation/sanitation is required. Fixing the errors was simple, because some sort of validation was already in place. I merely had to rename the extant `read_ncname` into `DirtyString.read_ncname`, etc. I clearly remember one place, however, which was flagged by the type checker but did not have any validation checking. After thinking for about ten minutes, I decided that there should have been a validation check at that place after all.

The experience proved quite encouraging. I spent in total less than an hour implementing the `DirtyString` and adjusting the code correspondingly, and I found a genuine problem. There was also no run-time overhead as I could see.

**References**

<[https://www.xkcd.com/327/](https://www.xkcd.com/327/)\> Exploits of a Mom (aka, \`Bobby Tables')

[Non-empty lists](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/Haskell/dependent-types.html#non-empty-list)  
The similar \`tainting' technique ensures the absence of \`head/tail of empty list' errors (the equivalent of \`NullPointerException')

[reverse.ml](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/ML/reverse.ml) \[2K\]  
List reversal with safe list access: The OCaml version of the Non-empty list code

Abstract Types -- the language protection layer
-----------------------------------------------

Let us explain, on a series of simple but already useful examples, how exactly abstraction acts as a protection layer, which invariants it enforces, and what good it does us.

Suppose we have several (assumed constant) integer arrays `a1...an`, to be repeatedly searched for occurrences of some elements. It may make sense to create the sorted versions of the arrays `a1'...an'` and search those, with the efficient binary search. There are a couple of problems however. First of all, how to ensure that an `ai'` is still the sorted version of the original `ai`? Arrays are mutable by nature. Whenever we pass `ai'` as an argument to some function, we cannot be sure it remains unmodified (unless we carefully examine the code of that function, if it is even available). Mainly, how does the binary search function make certain its argument array is actually sorted? Examining the array for sortedness is out of the question since it takes more time than the actual search.

Protection -- access restriction -- provides the sortedness guarantee. Consider the following simple library realized as an OCaml module.

    module ArraySearch : sig    (\* the interface \*)
       type t                      (\* abstract \*)
       val sort   : int array -> t
       val search : int -> t -> bool
    end = struct           (\* the implementation: the \`kernel' \*)
       type t = int array
       let sort : int array -> t = fun a -> 
         let a' = Array.copy a in
         Array.sort compare a'; a'
               (\* binary search in the sorted array arr \*)
       let search x arr = ...
    end

One can write such code in almost any language with a module system. (Later on we show another implementation, using closures/objects.) The library provides only two operations: `sort` and `search`. The former takes an integer array and returns the value of type `t`. Nothing is known about the type `t` to the users of library. In particular, none of the array access and mutation operations apply to `t` values: if we try, the type checker will complain that `t` is not an array. In fact, `t` is not equal to any other type known to the type checker. Hence we may give a `t` value only to polymorphic functions (which merely pass it around), or to the operation `search` of the library. Here is a usage example (for the case of two arrays `a1` and `a2` in our original set-up):

    let a1' = ArraySearch.sort a1        (\* First, sort the arrays \*)
    let a2' = ArraySearch.sort a2
    ...   (\* some code \*)
    let v1 = ArraySearch.search 1 a1' && ArraySearch.search 2 a2'
    ...   (\* more code \*)
    let f x = ArraySearch.search x a1' in
    f 1 && f 2

The prefix `ArraySearch` may be omitted if we `open ArraySearch` first.

Looking into the implementation of the library (the `ArraySearch` structure) shows that the type `t` is actually `int array` -- but this is known only within the implementation. The implementation may, therefore, treat `t` values as arrays, with no complaints from the type checker. The operation `sort` makes the copy of its input array and sorts the result; `search` does not modify the argument array. We thus may conclude that a value of type `t` represents a sorted array. It is made sorted by `sort`, it is not aliased to outside, and the operations on `t` preserve sortedness. We made this determination by looking _only_ at the implementation of the module, rather than through any of the code that may use it. The users of the library cannot know what `t` is, and cannot apply any operations to it except for `ArraySearch.search`. Hence the sortedness -- the property, or the invariant that may be associated with the type `t` -- indeed holds. Thus the merit of type abstraction, of type-checker--reinforced access restrictions: invariants established by _local_ reasoning are preserved _globally_. In our case, the sortedness invariant entails that the `search` operation does not need to check if its argument is a sorted array. It always is. The type abstraction hence let us use the faster algorithm and be sure it is correctly applied.

Before we turn to the second example, let us see another realization of the sorted-array--search library, using a different language protection mechanism.

    let sort : int array -> (int -> bool) = fun a ->
      let a' = Array.copy a in
      let () = Array.sort compare a' in
               (\* binary search in the sorted array arr \*)
      let search x arr = ... in
      fun x -> search x a'

Here, `sort` takes an `int array` and returns an operation to (efficiently) search in that array. The abstraction mechanism now is first-class functions: returning functions as results. Crucially, the returned function is actually a _closure_ `fun x -> search x a'` that contains a reference to the internal array `a'`. Since closures are opaque and cannot be deconstructed, the embedded `a'` reference cannot be used, or seen, outside the closure. We notice no other reference to `a'` available to library users; therefore, once sorted, `a'` remains constant and sorted. Again we see local reasoning (examining only the implementation but not the uses of the library) and the guarantee due to the protection that local invariants remain valid globally.

Incidentally, the closure implementation is similar to the one presented already in Morris 1973 paper (in the context of a different example -- although Morris has also mentioned the array sortedness and the binary search). With closures, there is an overhead of creating and using them. With the `ArraySearch` module, there is no run-time overhead at all: internally a value of the type `t` is an `int array`, with no wrappers or indirections.

As the next example, to be used later, consider the following module (library) with four operations:

    module Interv : sig    (\* the interface \*)
       type t                  (\* abstract \*)
       val lwb : t
       val upb : t
       val mid : t -> t -> t
       val to\_int : t -> int
    end = struct           (\* the implementation: the \`kernel' \*)
       type t = int
       let lwb = 0
       let upb = 5
       let mid x y = (x + y) / 2   (\* no overflow, for our range \*)
       let to\_int x = x
    end

The library is presented in the form of an OCaml module. The similar code can also be easily written in any Object-Oriented language, as a class with a protected field.

Examining the implementation lets us conclude that we may attribute to the abstract type `Interv.t` the invariant that it represents an integer from 0 through 5, inclusive. Indeed, `lwb` returns 0, which respects the invariant. Likewise, `upb` returns 5, the number within the range. The operation `mid` is both the consumer of `t` values and the producer of them. As a consumer, it can assume `t`'s invariant: it is a number from 0 through 5. Upon this assumption, the result of `mid` is also a number within the same range. Hence the invariant is established. As the consequence, the result of `to_int` operation will surely be an integer from 0 though 5, incl. Such an assurance is behind safe array indexing, described next.

To conclude, the language protection layer helps ensure that the invariants established by _local_ reasoning are preserved _globally_. Specifically, type abstraction lets us attribute an invariant (a proposition) to the values of an abstract type: e.g., the value represents a number within certain range, or a sorted array. The idea of abstract data type's statically representing and enforcing sophisticated properties of run-time values was Robin Milner's main insight behind the design of the Edinburgh LCF theorem prover back in the early 1970s.

The local reasoning conducted so far has been informal. It could be made formal, to any desired degree. In fact, the protection layer helps formal reasoning by making it local and modular. It is much easier to formally analyze the implementation of the, say, `Interv` interface, than the whole program using that interface.

It should be pointed out that guarantees of abstract types come from the lack of reflection, extension, and introspection of abstract data types. OCaml, incidentally, is weaker in this respect because polymorphic equality can break some of the invariants. So, reflection, extension and introspection facilities of a language can be viewed as vices rather than virtues.

**References**

James H. Morris Jr.: Protection in Programming Languages  
Comm. of the ACM, 1973, V16, N1, pp. 15-21  
Abstract: Linguistic mechanisms which can be used to protect one subprogram from another's malfunctioning are described. Function-producing functions and various type-tagging schemes are considered. An attempt is made to distinguish between access limitation and authentication.

[Formalization of the abstract type protection](#formalization)

[Lightweight static capabilities](#lsc)

Eliminating array bound checks
------------------------------

We now describe in detail how the language protection layer helps ensure that all array indexing operations are within array bounds. Not only do we get the confidence that an out-of-bound access (aka, buffer overflow) never occurs in any run of the program. We also eliminate dynamic array bound checks and hence improve the program performance. Let us see how the slogan -- safe and efficient, now -- works for realistic, interesting programs. This article describes binary search in a sorted array -- the example in the famous Xi and Pfenning's PLDI 1998 paper. More involved examples appear later on this web page. This article uses OCaml, although our binary search code was first presented (in 2004) in Haskell. It is easy to reimplement it in Scala or even Java; the simpler versions have been coded in C++ (see the references at the end).

Let's consider an extended version of the `Interv` interface from our earlier example:

    module type Index = sig
      type index  = private int       (\* i:index implies lwb <= i <= upb \*)
      type indexL = private int       (\* i:indexL implies lwb <= i \*)
      type indexH = private int       (\* i:indexH implies i <= upb \*)
      val lwb   : indexL
      val upb   : indexH
      val bsucc : index -> indexL
      val bpred : index -> indexH
      val bmid  : index -> index -> index
      val bcmp  : indexL -> indexH -> (index \* index) option
    end

The `Index` interface shows off another abstraction facility of OCaml: so-called private types. A value of the type `private int` can always be cast into `int` and later used as an ordinary integer. In fact, at run-time a `private int` is an `int` -- moreover, the optimizer can see that and optimize accordingly. On the other hand, `int` _cannot_ be cast to `private int`; using an `int` value where a `private int` is expected is a type error. The only way to create `private int` values is to use the operations of the interface.

`Index` has three abstract types, all distinct: for example, `bsucc lwb` is a type error. The code comments show the propositions we would like to attribute to the values of these types. That is, if `i` is a value of the type `index`, it is an integer within the closed range `[lwb,upb]` (the value of the type `index` hence also witnesses the fact that `lwb <= upb`). In contrast, an `indexL` value is only bounded from below, by `lwb`, and an `indexH` value is only bounded from above. The operation `bsucc` is meant to be the index increment. Its result may exceed `upb` but surely stays above the lower bound; therefore the result type is `indexL` rather than `index`. The operation `pred` is the predecessor (which certainly preserves the upper bound); `bmid` averages two indices, leaving the result in range -- which is reflected in its type. Finally, `bcmp` compares an integer `i:indexL` bounded from below by `lwb`, with an integer `j:indexH` bounded from above by `upb`. If `i<=j`, then both `i` and `j` are in fact within `[lwb,upb]`; they should be returned as the values of the type `index` this time. The comparison result is hence not just a mere `true` or `false`: a successful comparison improves our knowledge of values and, correspondingly, entitles to use more precise types.

The following is the straightforward implementation, parameterized by `upb`, which could be an arbitrary integer. The lower bound is assumed zero. It is easy to see the implementation respects the invariants of the interface we have just described.

    module IndexF(S:sig val upb:int end) : Index = struct
      type index  = int          
      type indexL = int          
      type indexH = int          
      let lwb : indexL = 0
      let upb : indexH = S.upb
      let bsucc : index -> indexL = Stdlib.succ
      let bpred : index -> indexH = Stdlib.pred
      let bmid  : index -> index -> index = fun x y -> x + (y - x)/2 
      let\[@inline\] bcmp  : indexL -> indexH -> (index \* index) option = fun i j ->
        if i <= j then Some (i,j) else None
    end

Within the implementation, `index`, `indexL` and `indexH` types are no longer private, so we may create their values. Ascribing the signature `Index` adds the private qualification, and the corresponding access restrictions. The curious `[@inline]` is an inlining annotation similar to the `inline` keyword in C/C++.

The invariant that a value of the `index` type is an integer within `[lwb,upb]` guarantees safe access to an array whose index range is the same `[lwb,upb]`. Because the safety is assured, the run-time bounds check can be elided:

    module BArray(S: sig type el val arr : el array end) = struct
      include IndexF(struct let upb = Array.length S.arr - 1 end)
      let\[@inline\] bget : index -> S.el = 
        fun i -> Array.unsafe\_get S.arr (i :> int)
    end

If `a1` is an integer array then `BArray(struct type el = int let arr = a1 end)` is a module that implements the `Index` interface with one extra operation: `bget` for indexing within the array `a1`. (The operation `(i :> int)` is the coercion of an `index` to an `int`, which is always possible and is, operationally, the identity.) The instantiation of `IndexF` makes `lwb` be zero and `upb` be the length of `a1` minus one -- which is the index range of `a1`, if it is not empty, thus justifying the use of `unsafe_get`. (If `a1` is an empty array, `unsafe_get` is still justified, because the `index` type has no values.)

We have implemented the trusted \`security kernel', providing the `Index` API with the extra `bget` method. We can now write the binary search itself. It takes the comparison operation `cmp`, a `key` and an array and returns either `None` if the `key` does not occur in the array, or `Some (i,key)` where `i` is the index at which the `key` does occur. The array is assumed sorted, according to `cmp`. We took this `bsearch` interface from Xi and Pfenning's PLDI 1998 paper.

    let bsearch (type a) : (a\*a -> int) -> a -> a array -> (int \* a) option =
      fun cmp key arr ->
        let module M = BArray(struct type el = a let arr = arr end) in
        let open M in
        let rec look (lo:indexL) (hi:indexH) = (\* type annotations are for clarity\*)
         match bcmp lo hi with
         | None -> None
         | Some (lo',hi') ->  (\* lo' and hi' are of the type index now \*)
             let m = bmid lo' hi' in
             let x = bget m in
             let cmp\_r  = cmp (key,x) in
             if cmp\_r < 0 then look lo (bpred m)
             else if cmp\_r = 0 then Some ((m :> int), x)
             else look (bsucc m) hi
       in
       look lwb upb

The implementation is textbook, and also closely matches Xi and Pfenning's code: only theirs was in Dependent ML and ours is ordinary OCaml. The key part is `bcmp lo hi` that compares `lo` (bounded from below by zero) with `hi` (bounded from above by `upb`, the largest index value within the array). If `lo` does not exceed `hi`, then `[lo,hi]` interval is non-empty, and is contained within `[0,upb]`, the safe index range.

The `bsearch` code is not part of a trusted security kernel: rather, it is \`user-level', so to speak. It is written using the interface of the (instantiated) `BArray` module and benefits from its invariants: the guaranteed safe indexing within the input array. If we compile the `bsearch` code with `ocamlopt -O3` and look at the generated assembly, we see all array access and index calculations inlined, with no bounds checks and no calls to error functions. Safe and efficient, indeed.

There are many variations of the `bsearch` code; the references below show several. It is worth noting one variation point, related to the feature of the `BArray` module that we did not stress.

`BArray` assures safe indexing within an array whose length is not known until run-time. The more one thinks about it, the harder it gets to believe. How is it possible to use types -- which are checked statically, before the program runs -- to ensure in-bounds indexing when the bounds are not known until the run-time? The following example explains. After the set-up:

    let a1 = \[|1;3|\] and a2 = \[|1;3|\]
    module M1 = BArray(struct type el=int let arr=a1 end)
    module M2 = BArray(struct type el=int let arr=a2 end)

Evaluating

    (M1.lwb :>int);;
    - : M1.indexL = 0
    M1.bget M1.lwb;;
            ^^^^^^
    Error: This expression has type M1.indexL
           but an expression was expected of type M1.index

gives a type error. One may be puzzled: how come we cannot index an array at index 0? Because it is not always safe: the array may be empty. Therefore, in the `Index` interface, `M1.lwb` has the type `indexL`, rather than `index` expected by `bget`. The only way to get the `index` zero value is through the comparison `lwb` with `upb`, which amounts to the non-emptiness check. The types force us to do this check:

    match (M1.bcmp M1.lwb M1.upb, M2.bcmp M2.lwb M2.upb) with
    (Some (l1,u1), Some (l2,u2)) -> (M1.bget l1, M2.bget l2);;
    - : int \* int = (1, 1)

However, evaluating

    match (M1.bcmp M1.lwb M1.upb, M2.bcmp M2.lwb M2.upb) with
    (Some (l1,u1), Some (l2,u2)) -> ((l1:>int),(l2:>int));;
    - : int \* int = (0, 0)
    
    match (M1.bcmp M1.lwb M1.upb, M2.bcmp M2.lwb M2.upb) with
    (Some (l1,u1), Some (l2,u2)) -> M1.bget l2;;
                                            ^^
    Error: This expression has type M2.index
           but an expression was expected of type M1.index

is again the type error -- even though we checked that both arrays are non-empty and `l1` may indeed index within `M1`'s array. Although both `l1` and `l2` are of the type `index` and both represent the integer 0, only `l1` may index within `M1`'s array. This is because the types of `l1` and `l2` are actually different: `M1.index` and `M2.index`, resp. Each instantiation of `BArray` with a different array creates a new, fresh \`version' of the `index` type -- to be used for indexing only within that instance of `BArray`. One may say, a `BArray` instance makes a distinct \`brand' of its abstract types, usable only with the operations of the same brand. (The code of the operations need not be duplicated since the optimizer knows that `index` of any brand is a mere integer.) Technically, `M1.index` and `M2.index` are considered distinct by the type checker because they have different \`paths', or the provenance, which can be easily checked statically. This facility of OCaml is akin to path-dependent types in Scala.

The branding, albeit not a \`simple type' facility, is not exotic: it can be accomplished in any language with existential (or, \`abstract package' types), or universal types. OCaml has both existential and universal types, which gives two other ways to write the safe `bsearch`. The code referenced below shows one such alternative, using universal types, in OCaml and Haskell.

**Version**

The current version is -- original (Haskell): August 2004; improved and explained OCaml: August 2019

**References**

Hongwei Xi and Frank Pfenning: Eliminating Array Bound Checking Through Dependent Types. PLDI'98  
The famous paper introducing a practical dependent type system as a dialect of SML. We faithfully re-implement the `bsearch` example from that paper, in several languages.

[eliminating-array-bound-checks.ml](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/lightweight-guarantees/eliminating-array-bound-checks.ml) \[15K\]  
OCaml code explained in this message, plus the older implementation of branding based on universal types

[eliminating-array-bound-check-literally.hs](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/lightweight-guarantees/eliminating-array-bound-check-literally.hs) \[8K\]  
The Haskell version of `bsearch`. The code is written in Haskell98 with the sole extension for higher-ranked types.

[eliminating-array-bound-check.lhs](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/lightweight-guarantees/eliminating-array-bound-check.lhs) \[9K\]  
The literate Haskell code with explanations and the examples  
The first version of the code was originally posted as Eliminating Array Bound Checking through Non-dependent types on the Haskell mailing list on Thu, 5 Aug 2004 19:31:36 -0700. The current version corrects the problem pointed out by Conor T. McBride in the discussion thread.

[bsearch-static.cc](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/lightweight-guarantees/bsearch-static.cc) \[2K\]  
[bsearch-template.cc](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/lightweight-guarantees/bsearch-template.cc) \[2K\]  
[bsearch-static.s](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/lightweight-guarantees/bsearch-static.s) \[3K\]  
[bsearch-template.s](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/lightweight-guarantees/bsearch-template.s) \[2K\]  
Binary search in an array with the statically known bounds: C++ code (with and without templates) and the generated assembly code (gcc -O2). The assembly code shows no run-time overhead of the abstractions used to ensure the safety of array access.

[Lightweight guarantees and static analyses](#range-analysis)  
Another explanation of the branding technique, on a simpler example

Lightweight static capabilities
-------------------------------

We describe a modular programming style that harnesses modern type systems to verify safety conditions in practical systems. This style has three ingredients: (i) A compact kernel of trust that is specific to the problem domain; (ii) Unique names, _capabilities_, that confer rights and certify properties, so as to extend the trust from the kernel to the rest of the application; (iii) Static (type) proxies for dynamic values. We illustrate our approach using examples from the dependent-type literature, but our programs are written in Haskell and OCaml today, so our techniques are compatible with imperative code, native mutable arrays, and general recursion. The three ingredients of this programming style call for (1) an expressive core language, (2) higher-rank polymorphism, and (3) phantom types.

This paper demonstrates a lightweight notion of _static capabilities_ that brings together increasingly expressive type systems and increasingly accessible program verification. Like many programmers, we want to assure safety conditions: array indices remain within bounds; modular arithmetic operates on numbers with the same modulus; a file or database handle is used only while open; and so on. The safety conditions protect objects such as arrays, modular numbers, and files. Our overarching view is that a static capability authorizes access to a protected object and simultaneously certifies that a safety condition holds. Rather than proposing a new language or system, our contribution is to substantiate the slogan that types are capabilities, today: we use concrete and straightforward code in Haskell and OCaml to illustrate that a programming language with an appropriately expressive type system is a static capability language.

This is a joint work with Chung-chieh Shan.

**References**

[lightweight-static-capabilities.pdf](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/lightweight-guarantees/lightweight-static-capabilities.pdf) \[334K\]  
The paper published in Electr. Notes Theor. Comput. Sci, 174(7), pp. 79-104, 2007

[lightweight-guarantees-u07-poster.pdf](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/lightweight-guarantees/lightweight-guarantees-u07-poster.pdf) \[81K\]  
Poster \`Lightweight Static Guarantees' presented at the Poster Session of the 2007 USENIX Annual Technical Conference. June 20, 2007. Santa Clara, CA

[Lightweight static resources,](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/Haskell/number-parameterized-types.html#ls-resources) for safe embedded and systems programming  
This follow-up paper describes further applications, for safe embedded and systems programming, ensuring, among other properties, proper alignment when accessing raw memory areas. The paper also introduces kind-level static capabilities to enforce invariants of type-level programming.

Formalization of the abstract type protection
---------------------------------------------

How do we hope to prove that the protection layer indeed enforces invariants and extends locally established properties through the whole program?

The [Lightweight static capabilities](#lsc) paper (joint work with Chung-chieh Shan) introduced the so-called \`strict' type system with the dependent-type flavor. The safety invariants are enforced and propagated because they are part of types. The paper then introduced a relaxation of the strict system to the Hindley-Milner system with type abstraction (higher-ranked types), and demonstrated how the safety invariants are still maintained.

[safety.elf](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/formalizations/safety.elf) \[17K\]  
Twelf code with proofs of soundness of the \`strict' type system for the language based on System F with lists and non-empty lists. The progress theorem assures that a well-typed program shall not attempt to take the head or tail of an empty list.

[safety-array.elf](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/formalizations/safety-array.elf) \[32K\]  
Twelf code verifying manual proofs of soundness of the \`strict' type system for the language made of System F plus \`arrays' whose type reflects their size. The progress theorem states the safety property: in a well-typed program array access is always in-bounds.

Eliminating array bound checking in multiple arrays
---------------------------------------------------

This message gives another non-trivial example of writing code with non-trivial static guarantees in present-day functional languages. The example involves native Haskell arrays, index computations, and general recursion. All array indexing operations are statically guaranteed to be safe -- hence we can safely use the efficient `unsafeAt` primitive. Furthermore, the static assurances in the main loop cost us no run-time overhead. The example uses only Haskell98 + higher-ranked types. No new type classes are introduced. The safety is based on: Haskell type system, quantified type variables, and a compact general-purpose trusted kernel. I thank Daniel Yokomizo for the challenge.

Our example is folding over multiple, _variously-sized_ arrays. This is like a fold over an array -- generalized to an arbitrary number of arrays, whose index ranges do not have to be the same (and do not have to overlap). Typing this example in a genuinely dependent type system is probably going to be quite challenging.

Our goal is to implement a function

    marray\_fold :: (Ix i, Integral i) =>
   	         (seed -> \[e\] -> seed) -> seed -> \[Array i e\] -> seed

Its third argument is a list of arrays; the arrays have all the same element and index types; the actual sizes (that is, the lower and upper bounds) may differ. Some arrays in the list may even be empty (with the lower bound higher than the upper one). The function `marray_fold`, like left fold, applies its left argument to the values extracted from the corresponding arrays. Because arrays may have different sizes and bounds, `marray_fold` operates over the range of indices that is the intersection of the ranges of the argument arrays.

For example:

    dot a1 a2 = marray\_fold (\\seed l -> product l + seed) 0 \[a1,a2\]

computes the dot products of two arrays.

**Version**

The current version is February 2006

**References**

[eliminating-mult-array-bound-check.lhs](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/lightweight-guarantees/eliminating-mult-array-bound-check.lhs) \[13K\]  
The literate Haskell98 plus higher-order types  
The code was posted as Eliminating Multiple-Array Bound Checking through Non-dependent types on the Haskell mailing list on Fri, 10 Feb 2006 22:05:04 -0800 (PST)

Knuth-Morris-Pratt string search with safe array access
-------------------------------------------------------

The largest example in the Xi and Pfenning's PLDI'98 paper is Knuth-Morris-Pratt (KMP) string search. It is an imperative algorithm with complicated control flow, mutable arrays and indirect indexing within the pattern string. It also uses a deliberately out-of-bounds index value (-1) as a special indicator in the indirect indexing. The goal is to statically assure safety of all array and string access operations, and so to eliminate run-time array-bound check without introducing other overhead into the main loops of the algorithm.

We re-implement Xi's KMP code in Haskell and OCaml, maintaining the same safety guarantees and efficiency.

**Version**

The current version is May 2006

**References**

[KMP-DML.ml](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/lightweight-guarantees/KMP-DML.ml) \[4K\]  
The KMP code in Dependent ML, written by Hongwei Xi and published in Xi and Pfenning, PLDI'98

[KMP-deptype.hs](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/lightweight-guarantees/KMP-deptype.hs) \[14K\]  
The complete Haskell code: Haskell98 with higher-ranked types

[KMP.ml](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/lightweight-guarantees/KMP.ml) \[11K\]  
The corresponding OCaml code

The question of verification
----------------------------

The lightweight approaches depend on a trusted kernel. How to make sure the trusted library deserves our trust? The same question exists for any other dependent-type system: how we make sure that our Oracle is correct, the typing rules are correct, and, mainly, that the implementation of those rules in a _compiler_ is correct?

I have heard a similar question asked of J. Strother Moore and J. Harrison. J. Strother Moore said that most of ACL2 is built by bootstrapping, from lemmas and strategies that ACL2 itself has proven. However, the core of ACL2 just has to be trusted. ACL2 has been used for quite a while and so there is a confidence in its soundness. NSA and NIST found this argument persuasive when they accepted proofs by ACL2 as evidence of high assurance, in awarding Orange book A1 and IFIPS 140-1 ratings -- the highest security ratings -- to some products.

In general, verification is a rather complex issue, far beyond the mere checking of the derivations of formal propositions: see the references below. Even in Mathematics, it is not at all resolved what exactly constitutes a mathematical proof and how much trust, including personal trust, is involved.

**References**

Randy Pollack: How to believe a machine-checked proof

Toby Murray, P.C. van Oorschot. BP: Formal Proofs, the Fine Print and Side Effects  
Awarded \`the best paper' at IEEE SecDev 2018  
\`\`We consider what value proofs about software systems deliver to end-users (e.g., in terms of net assurance benefits), and at what cost in terms of side effects (such as changes made to software to facilitate the proofs, and assumption-related deployment restrictions imposed on software if these proofs are to remain valid in operation).'' In short, this is the paper on how to believe in a formal (security) proof and what value it actually offers.

Arthur Jaffe, Frank Quinn: \`\`Theoretical mathematics'': Toward a cultural synthesis of mathematics and theoretical physics  
Bull.Am.Math.Soc. 29 (1993) 1-13 <[http://arxiv.org/abs/math.HO/9307227](http://arxiv.org/abs/math.HO/9307227)\>  

William P. Thurston: On Proof And Progress In Mathematics  
Bull.Am.Math.Soc. 30 (1994) 167-177 <[http://arxiv.org/abs/math.HO/9404236](http://arxiv.org/abs/math.HO/9404236)\>  

Michael Atiyah, Armand Borel, G. J. Chaitin, Daniel Friedan, James Glimm, Jeremy J. Gray, Morris W. Hirsch, Saunder MacLane, Benoit B. Mandelbrot, David Ruelle, Albert Schwarz, Karen Uhlenbeck, Rene' Thom, Edward Witten, Christopher Zeeman: Responses to \`\`Theoretical Mathematics: Toward a cultural synthesis of mathematics and theoretical physics'', by A. Jaffe and F. Quinn  
Bull.Am.Math.Soc. 30 (1994) 178-207. <[http://arxiv.org/abs/math.HO/9404229](http://arxiv.org/abs/math.HO/9404229)\>

Arthur Jaffe, Frank Quinn: Response To Comments On \`\`Theoretical Mathematics''  
Bull.Am.Math.Soc. 30 (1994) 208-211. <[http://arxiv.org/abs/math/9404231](http://arxiv.org/abs/math/9404231)\>

Lightweight guarantees and static analyses
------------------------------------------

When the code for the safe and efficient binary search was first posted on the Haskell mailing list in August 2004, a lively discussion followed. In particular, Bjoern Lisper asked about the relation to classical range analyses known for a long time for imperative languages.

First of all, within the divide \`\`making sense of already written programs v. writing only those programs that make sense'', range analysis, as other static verification, belongs to the first group -- whereas types, model-driven development, refinement and lightweight guarantees belong to the second.

Mainly, lightweight guarantees, or language protection layer, lets us implement the classical range analyses in a program itself -- and be sure of their outcome. In contrast, analyses in the compiler cannot be easily seen or controlled, and their outcome is often hard to detect and explain.

Let's take an example: locating the first element of an array satisfying a given predicate and returning its index.

    let findarr : type a. (a -> bool) -> a array -> int option = fun p arr ->
      let n = Array.length arr in
      let rec loop i = 
        if i < n then
          if p (arr.(i)) then Some i
          else loop (succ i)
        else None
      in loop 0

The classical range analysis will see that `i` starts at the lower bound of `arr`, i.e., zero, and is incremented afterwards. When the analysis sees the test `i<n` it infers that in the \`then' branch of that test `i` does not exceed the upper bound of the array. Therefore, the indexing operation `arr.(i)` is safe and the run-time range check may be elided.

In the lightweight guarantees framework the code looks as follows (see the reference to the complete code below):

    let findarr' : type a. (a -> bool) -> a array -> int option = fun p arr ->
      let module M = LenF(struct type el=a let arr=arr end) in
      let open M in
      let rec loop i = 
        match cmp i length with
        | Some i' -> if p (get M.arr i') then Some (i' :> int)
          else loop (Nat.succ i)
        | \_       -> None
      in loop Nat.zero

The programmer gives the array, array length and `i` more precise types: `M.arr` has the type `a array M.len`, `length` has the type `int M.len`, and `i:nat`. Here `M.len` is an \`annotation' (erased at run-time) that an object has the length `len`. We should stress that the comparison of `i` with the array length no longer returns a mere boolean. The type of `cmp` is

    nat -> int M.len -> (nat M.len) option

If the comparison `cmp i length` succeeds, the result is `M.len`\-annotated `i`, which is in bounds of the array `M.arr`. The successful comparison \`improves `i`'s type', so to speak, making it more precise. Thus the logical implication that was implicit in the range checker is made explicit to the type checker here.

Bjoern Lisper further wrote \`\`A program analysis like range analysis is not exact, of course: it must make safe approximations sometimes and will sometimes say that an array index might be out of bounds when it actually won't. In your framework, this seems to correspond to the fact that you must verify your propositions about index expressions.''

True, just as the range analysis must verify the rules of the analysis. The difference is that the conventional range analyzer is part of a compiler, typically hidden from view (of a regular programmer). Here, the analyzer is part of a library.

The need for approximations may also arise in our framework. Suppose that in the original `findarr` code, the line

    if p (arr.(i)) then Some i ...

had been replaced with

    let j = very\_complex\_function i in
    if p (arr.(j)) then Some j ...

Although the analysis may know that `i` is within array bounds, it may be very difficult to ascertain if `j` is. The classical analysis may give up and insert a run-time check (often without any indications it is done so). In our framework, we have to

    let j = very\_complex\_function (i':>int) in
    match range\_check j with
    | Some j' -> if p (arr.(j')) then Some (j':>int) ...
    | None -> on\_out\_of\_range

That is, we intentionally forget the more precise typing of `i'`, do the complex index transformation, followed by a run-time witnessing to recover the precise typing. We now have to handle the situation if the result of `very_complex_function (i':>int)` is out of range. If we somehow know that the `very_complex_function` keeps the result within the range, but do not have the time to verify or prove it, we can replace `on_out_of_range` with `assert false`. In any case, the fact that we gave up on the precise analysis is very explicit, and so is the dynamic check we had to insert.

Incidentally, if we can prove that `very_complex_function` leaves the index in range, then we can give the function a more precise type: `nat M.len -> nat M.len` and put into the trusted kernel, after the appropriate rigorous verification.

**References**

[range-check-expl.ml](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/lightweight-guarantees/range-check-expl.ml) \[3K\]  
The complete OCaml code for the example

Thread Re: \[Haskell\] Eliminating Array Bound Checking through Non-dependent types on the Haskell mailing, August 4-8 2004

Lightweight guarantees and dependent types
------------------------------------------

We have seen that type abstraction lets attribute an invariant (a proposition) to the values of an abstract type -- e.g., all values of the type represent integers within a certain range. There is a clear similarity with dependent types.

A dependent type expresses the proposition about the values of the type directly (using the language of logic), whereas an abstract type merely _refers_ to the proposition. The difference is akin to that between a golden coin and paper money. The coin has its worth in the gold it carries. It is relatively easy to compare and exchange coins of different coinages, based on the weight and purity of their gold. Paper money merely refers to worth, and requires trusted institutions (state, banks) to operate. Comparing paper money of different issuers is non-trivial. Still, we know from history that an economy based on paper money is viable.

In the discussion thread following the first presentation of the lightweight guarantees approach in August 2004, Conor McBride has made an excellent summary of this approach and its relation to genuine dependent types: \`\`The abstract `brand` is just a type-level proxy for the bounding interval, and the library of operations provides interval-respecting operations on indices. This is a very neat solution in Haskell, but it goes round an extra corner which isn't necessary with dependent types, where you can just talk about the interval directly. The library-writer would develop and verify the same convenient operations for working with intervals and indices; the proofs would be independently recheckable terms in type theory.''

**References**

Extensive discussion with Conor McBride, with his many insights and explanations of dependent-type programming. Haskell mailing list, August 6-9, 2004. Thread: Re: Eliminating Array Bound Checking through Non-dependent types

Rights amplification
--------------------

A capability, mentioned several times earlier, is a token that authorizes an access to a resource or service -- something like a train ticket, which gives the right to board a train. Capabilities originated in secure operating systems; see the ode-capabilities article referenced below for history. Continuing the ticket analogy, to board a plane we also need a passport. Passport is an authorizing token as well, giving us, e.g., the right to enter and remain in the country. It does not, by itself, give the right to board a plane -- and neither does the ticket. We need both these tokens, and they have to match (have the same name). The right conferred by the matching combination of the passport and the ticket is more that the sum of the rights of these tokens by themselves. This is rights amplification.

We have already met rights amplification in the [Eliminating array bound checks](#safe-array-access) example: to access an array value we have to possess the branded array as well as the branded index of the same \`brand', which stands for array bounds.

This article describes another classical example of rights amplification: \`simple money', from the ode-capabilities: \`\`The function, makeMint, makes mints. Each mint defines a separate currency that isn't directly convertible with other currencies -- although, of course, money changers could trade one for the other, providing indirect convertibility. A mint can make purses that hold new units of its currency, thereby inflating that currency. A purse can report its balance and make \[\`sprout'\] a new empty purse of the same currency. Given two purses of the same currency, you can deposit money into one from the other.'' Thus a purse, by itself, supports only balance reporting and sprouting. It is only when holding two purses of the same currency that an additional operation becomes possible: deposit. \`\`It is a wonderful small example of the directness and simplicity with which capabilities allow the expression of arrangements in which mutually suspicious parties can cooperate safely,'' that article says. The example also demonstrates local reasoning to verify global security properties: nobody can double-spend or steal money.

The ode-capabilities article implements the simple money in the \`capability-oriented' language E, using so-called \`sealer-unsealer pairs' provided by E as primitive. We re-implement the example in plain OCaml, with no special primitives. We also provide money changers, which were only hinted at in the original example. The OCaml implementation maintains the same security guarantees about money, and can ascertain them via static local reasoning.

Before we start, let us make a special type `Balance.t` for money balances, ensured to be non-negative by construction.

    module Balance : sig
      type t = private int                  (\* At run-time, identical to int \*)
      val zero   : t
      val (+)    : t -> t -> t              (\* NO run-time checks or overhead \*)
      val of\_int : int -> t option          (\* run-time positivity check \*)
      val (-)    : t -> t -> t option
     end = struct
      type t = int
      let zero   = 0
      let (+)    = Stdlib.(+)               (\* no run-time checks or overhead \*)
      let of\_int : int -> t option = 
        fun x -> if x >= 0 then Some x else None
      let (-)    : t -> t -> t option = 
        fun x y -> if x >= y then Some (x-y) else None
    end

This module is almost the same as `Index` in [Eliminating array bound checks](#safe-array-access), which see for more explanations.

The simple money interface is described by the following signature.

    module type simple\_money = sig
      type 'currency mint
    
      (\* heterogeneous equality for mints \*)
      val mint\_eq : 'c1 mint -> 'c2 mint -> ('c1,'c2) eq option 
    
      type mint\_holder = Mint : 'currency mint -> mint\_holder (\* existential \*)
    
      val make\_mint : string -> mint\_holder
      val name : \_ mint -> string
    
      type 'currency purse
      val make\_purse : 'currency mint -> Balance.t -> 'currency purse
      val string\_of\_purse : \_ purse -> string (\* for documentation/debugging \*)
      val balance : \_ purse -> Balance.t
      val sprout  : 'currency purse -> 'currency purse
      val deposit : Balance.t -> 
        from:'currency purse -> 
        into:'currency purse -> unit   (\* could also throw an exception \*)
    
          (\* check if two purses are of the same currency \*)
      val purse\_eq : 'c1 purse -> 'c2 purse -> ('c1,'c2) eq option 
    end

We see the already introduced operations to make and query mints and purses and perform deposits. The operations `name` and `string_of_purse` are added for the sake of presentation. The heterogeneous equality comparison of mints and purses will be explained when we come to money changers.

There are already static assurances that follow from the signature itself. First of all, the types `mint` and `purse` are abstract. Therefore, the only way to create mints and purses is by using the operations of the interface. The types are parameterized by the type of `currency`. A mint, therefore, inflates (that is, builds purses of) its own currency only: see the type of `make_purse`. A mint can only be created by invoking `make_mint` and then \`opening' the returned existential -- which gives a `currency mint` value with the fresh type `currency` distinct from any other type. Therefore, two purses whose types have the same `'currency` parameter must be \`siblings': must have eventually come from the same mint. Deposit is only possible among sibling purses: see the type of `deposit`. We thus realize rights amplification by so-called \`sibling communication'. The communication is enforced solely by types: it has no run-time overhead.

We may already write the main example from ode-capabilities, of Alice paying Bob 10 coins: \`\`First, playing Alice, we would sprout a new purse from our main purse, and then transfer 10 coins into it. Then, we send a ... request to Bob, providing the purse containing 10 coins as payment'':

    let play\_alice alice\_purse bob =
      let paymentForBob = sprout alice\_purse in
      deposit tencoins ~from:alice\_purse ~into:paymentForBob;
      bob paymentForBob;
      Printf.printf "Alice: paid bob; the balance %s\\n" (string\_of\_purse alice\_purse)
    
        val play\_alice : 'a purse -> ('a purse -> 'b) -> unit = <fun>

\`\`Playing Bob, deposit the payment into Bob's purse'':

    let play\_bob bob\_purse payment\_recd = 
      deposit tencoins ~into:bob\_purse ~from:payment\_recd;
      Printf.printf "Bob: got paid; %s\\n" (string\_of\_purse bob\_purse)
    
        val play\_bob : 'a purse -> 'a purse -> unit = <fun>

The point of the example is to assure Bob that if `deposit` succeeded, then 10 coins will indeed be transferred from Alice's payment into his purse -- even though Bob cannot see how exactly Alice made the `payment_rcd` purse. What if it is \`fake'? What is there to prevent Alice from double-spending? Bob gets the assurances by examining the signature `simple_money` and its implementation, to be shown next. Bob does not need to see Alice's code.

Just for completeness, here is how we run the example: mint the main purses for Alice and Bob, and let them perform the transaction:

    let \_ =
      let Mint carol\_mint = make\_mint "Carol" in
      let aliceMainPurse = make\_purse carol\_mint (bal 1000) in
      let bobMainPurse   = make\_purse carol\_mint Balance.zero in
    
      Printf.printf "Alice's purse: %s\\n" (string\_of\_purse aliceMainPurse);
      Printf.printf "Bob's purse: %s\\n" (string\_of\_purse bobMainPurse);
      
      let alice = play\_alice aliceMainPurse in
      let bob = play\_bob bobMainPurse in
      alice bob

Here is an implementation of `simple_money` (abbreviated; see the full code for all details):

    module Money : simple\_money = struct
      module type mint = sig
        type curr
        val name : string
        (\* ... elided \*)
      end
      type 'currency mint = (module mint with type curr = 'currency)
    
      type mint\_holder = Mint : 'currency mint -> mint\_holder (\* existential \*)
    
      let make\_mint : string -> mint\_holder = fun name -> 
        let module NewMint = struct
          type curr                         (\* fresh type \*)
          let name = name
          (\* ... elided \*)
        end
        in Mint (module NewMint)
    
      type 'currency purse = {mint: 'currency mint; bal: Balance.t ref}
    
      let make\_purse : 'currency mint -> Balance.t -> 'currency purse = 
        fun mint b -> let bal = ref b in {bal;mint}
    
      let balance : \_ purse -> Balance.t = fun {bal} -> !bal
      let sprout  : 'currency purse -> 'currency purse = fun {mint} -> make\_purse mint Balance.zero
    
          (\* could also throw an exception \*)
      let deposit : Balance.t -> from:'currency purse -> into:'currency purse -> unit = 
        fun b ~from ~into ->
          let rem = Balance.(!(from.bal) - b) |> Option.get in
          from.bal := rem;
          into.bal := Balance.(!(into.bal) + b)
    end

It has the same security properties as the E code in the original example:

1.  Only someone with the mint of a given currency can violate conservation of that currency (that is, created new non-empty purses of that currency).
2.  The mint can only inflate its own currency.
3.  No one can affect the balance of a purse they don't have.
4.  With two purses of the same currency, one can transfer money between them.
5.  Balances are always non-negative integers.
6.  A reported successful deposit can be trusted as much as one trusts the purse one is depositing into.

We already verified items 2 and 4, when looking at the signature `simple_money`. Item 5 is guaranteed by the `Balance` module. To see that property 1 holds, we examine all operations that touch the field `bal` of a purse. With the exception of `make_purse`, all such operations preserve currency. Also, the field `bal` is not aliased to a global reference. In fact, there are no global parameters. The absence of aliasing also assures property 3: if one does not have a reference to a purse, one cannot possibly affect the `bal` of that purse. For item 6 we check the implementation of `deposit`.

Let's return to the main question, of assuring Bob that the purse with the payment from Alice is not fake, even if he does not have any way of knowing how Alice has made it. How can Bob be certain that if the `deposit` succeeded, 10 coins are transferred into his purse and Alice cannot spend them again. In the original code E code, the answer relies on examining the implementation for sealer/unsealer pairs and reasoning from the properties of such pairs. In our case, the reasoning is simple: the `bal` field of a purse is not exposed (`purse` is the abstract type in the `simple_money` signature). It is also not aliased, which is clear from examining the two functions that return purses. Therefore, purses cannot be faked, and the invariants of their implementation (whenever balance of one purse is decremented, another purse is credited) hold globally.

One may say that the type-based enforcement of sibling communication is too rigid. Let's look again at the inferred type of `play_bob : 'a purse -> 'a purse -> unit`. Attempting to give Bob a purse whose currency cannot be ascertained the same as Bob's main purse raises a type error. This is often the desirable outcome. It could be the case however that the payment purse is received from a different host or read from a file, and its currency cannot be statically known. Also, Bob could be willing to accept payments in several currencies.

Our `simple_money` interface and OCaml do permit such \`dynamic' currency processing. It uses a form of sealing and does involve a run-time check, which is necessary if the type of currency is not statically known and may vary. As an illustration, we implement a money changer, mentioned in passing in ode-capabilities.

First we introduce

    type sealed\_purse = Sealed : 'currency Money.purse -> sealed\_purse

which hides the currency type in an existential. A money changer can then be given the type

    type money\_changer = sealed\_purse -> sealed\_purse -> unit

A money changer hence takes two purses of arbitrary currencies and attempts to transform all money from the former to latter, at some exchange rate. From the look at the `simple_money` interface, such an operation is impossible, one might think. However, if one has a reserve of appropriate currencies -- a bank -- money exchange is doable. The enclosed code implements it, outside the `Money` module and hence maintaining all the security invariants of `Money`. The key operation is the heterogeneous purse equality

    type ('a,'b) eq = Refl : ('a,'a) eq     (\* equality type \*)
    val purse\_eq : 'c1 purse -> 'c2 purse -> ('c1,'c2) eq option

which compares two purses (actually, their mints) of possibly different currencies to see if the currencies are actually the same. If they are, the operation returns the evidence of the equality. Thus, if we have two sealed purses `sealed1` and `sealed2`, we can write

    match (sealed1,sealed2) with
    (Sealed s1,Sealed s2) -> match purse\_eq s1 s2 with
      | Some Refl -> deposit (bal 10) s1 s2 
      | None      -> (\* s1 and s2 are of different currencies \*)

Within the `Some Refl` branch of the match statement, purses `s1` and `s2` can be treated as having the same `currency` type. One may therefore deposit money between them.

Using the lightweight static capabilities, the simple money example was first implemented (without the money changer) in 2006, as an answer to a challenge by Mark S. Miller. I thank him for the challenge and insightful explanations.

**References**

<[http://www.erights.org/elib/capability/ode/ode-capabilities.html](http://www.erights.org/elib/capability/ode/ode-capabilities.html)\>  
Explanation of rights amplification on the simple-money example, using the E language

[mint.ml](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/mint.ml) \[17K\]  
The re-implementation of this example and the money changer in plain OCaml, with many comments, explanations and tests

Conclusions
-----------

We have demonstrated the programming style that ensures safety without sacrificing efficiency. The key idea is a language protection layer (specifically, type abstraction), which lets us restrict access to some operations on data. Operations that preserve a desired invariant may be accessed \`publicly'; the other, potentially invariant-destroying operations, may only be invoked within a (usually small) \`trusted kernel', where the invariant is assured by careful code inspection or in some formal way. All in all, the invariants established by _local_ reasoning are preserved _globally_. The globally-valid invariants (such as sortedness, range limit, etc) then guarantee safe execution of the program and let us elide run-time safety checks.

_Safe and Efficient_ can be practiced right now. In fact, the main idea was proposed by Milner and Morris in the mid-1970s, and was shown to work already then. I personally have been using it successfully, in production and in answering challenges. Further challenges and suggestions are welcome.


[Source](http://okmij.org/ftp/Computation/lightweight-static-guarantees.html)