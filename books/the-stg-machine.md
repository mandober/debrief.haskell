# The STG machine

`Implementing lazy functional languages on stock hardware: The STG machine`
paper by Simon Peyton Jones, 1992, Version 2.5
https://www.microsoft.com/en-us/research/wp-content/uploads/1992/04/spineless-tagless-gmachine.pdf

## Abstract

**The Spineless Tagless G-machine (STG)** is an *abstract machine* designed to support *non-strict higher-order functional languages*.

This is a 3-part presentation of the machine:
1. We give a general discussion of the design issues involved in implementing non-strict functional languages.
2. We present the *STG language*, an austere but recognisably FPL, which as well as a *denotational meaning* has a *well-defined operational semantics*. The STG language is the *"abstract machine code"* for the STG-machine.
3. We discuss the mapping of the STG language onto stock hardware.

The success of an abstract machine model depends largely on how efficient this mapping can be made, though this topic is often relegated to a short section. Instead, we give a *detailed discussion of the design issues and the choices* we have made. Our principal target is the `C` language, treating the C compiler as a portable assembler.

## 1. Introduction

(this paper is from 30 years ago, from 1992)

The challenges of compiling non-strict FPLs have given rise to a whole stream of research work. Generally the discussion of this work has been focussed around the design of a so-called abstract machine, which distils the key aspects of the compilation technique without becoming swamped in the details of source language or code generation.

Quite a few such abstract-machine designs have been designed:
- the G-machine             (Augustsson, 1987; Johnsson, 1987)
- TIM                       (Fairbairn and Wray, 1987)
- the Spineless G-machine   (Burn, Peyton Jones and Robson, 1988)
- the Oregon G-machine chip (Kieburtz, 1987)
- the CASE machine          (Davie and McNally, 1989)
- the HDG machine           (Kingdon, Lester and Burn, 1991)
- the ⟨v,G⟩ machine         (Augustsson and Johnsson, 1989)
- the ABC machine           (Koopman, 1990)

The early implementations, especially those *based on graph reduction*, were radically different from conventional compiler technology: the difference between an *SK combinator implementation* (Turner, 1979) and e.g. a Lisp compiler is substantial.

So great was this divergence that new hardware architectures were developed specifically to support the execution model (Scheevel, 1986; Stoye, Clarke and Norman, 1984).

As understanding has developed, though, it has been possible to recognise features of more conventional systems emerging from the mist, and to generate efficient code for stock architectures.


This paper presents a new abstract machine for non-strict functional languages, **the Spineless Tagless G-machine**, set it in the context of conventional compiler technology, and we give a detailed discussion of its mapping onto stock hardware.

Our design exhibits a number of unusual features:

* In all other abstract machines mentioned above, the abstract machine code for a program consists of a sequence of abstract machine instructions. Each instruction is given a precise operational semantics using a state transition system. We take a different approach: *the abstract machine language is itself a very small functional language*, which has the usual denotational semantics. In addition, though, *each language construct has a direct operational interpretation*, and we give an *operational semantics for the same language using a state transition system*.

* Objects in the heap, both *unevaluated suspensions* and *head normal forms*, have a *uniform representation with a code pointer in their first field*. Many othe implementations examine the tag fields of heap objects to decide how to treat them. With our representation we never do this: instead, *a jump is made to the code pointed to by the object*. This is why we call the machine **tagless**.

* A pervasive feature of functional programs is the *construction of data structures*, and their traversal using *pattern-matching*. Many abstract machine designs say very little about how to do this efficiently, but we pay a lot of attention to it.

* The machine manipulates *unboxed values* directly, based on the ideas in a companion paper (Peyton Jones & Launchbury, 1991). This is essential for an efficient implementation of arithmetic, but it is usually hidden in the code generator.

* There is a scope for exploiting the fruits of both *strictness analysis* and *sharing analysis* to improve execution speed.

* *Lambda lifting*, a common feature of almost all FPL implementations, is not carried out. Instead, the free variables of lambda abstractions are identified, but *the abstraction is left in place*.

* The machine is particularly well-suited for parallel implementations, although space prevents this aspect being discussed in this paper (Peyton Jones, Clack & Salkild, 1989).

Almost all the individual ideas we describe are present in other implementations, but their combination produces a particularly fast implementation of lazy functional languages.

An earlier version of this paper (Peyton Jones & Salkild [1989]), had a similar title and introduction to this one. The underlying machine being described is mostly unchanged, but the presentation has been completely rewritten.

## 2. Overview

The paper is divided into 3 parts:
- Part I explores the design space to show how the STG fits in wider context
- Part II introduces the abstract machine and gives its operational semantics
- Part III discusses how the abstract machine is mapped onto stock hardware

### 2.1 Part I: The design space

The implementation of non-strict functional languages has tended to be done in a separate world to that of "real compilers". One goal of this paper is to help bridge the gap between these two cultures.

To this end we identify several **key aspects of a compiler**
- representation of data structures
- treatment of function application
- compilation of case-analysis on data structures
and compare the approach we take with that of others.

### 2.2 Part II: the abstract machine

The usual way to present an evaluation model for a FPL is to define an *abstract machine*, which *executes an instruction stream*.

The abstract machine is given an *operational semantics* using a *state transition system*, and *compilation rules* are given for converting a functional program into *abstract machine code*.

The application of these compilation rules is usually preceded by *lambda lifting* (Johnsson, 1985), which eliminates lambda abstractions in favour of *supercombinators* (functions with no free variables).

A good example of this approach is the G-machine (Johnsson, 1987; Peyton Jones, 1987), whose abstract machine code is called *G-code*.

This approach suffers an annoying disadvantage: the abstract machine is generally not abstract enough.

For example, *the abstract G-machine* uses the stack to hold many intermediate values. When G-code is to be compiled into native machine code, many stack operations can be eliminated by holding the intermediate values in registers. The code generator has to simulate the operation of the abstract stack, which is used, in effect, mainly to name intermediate values. Not only does this process complicate the code generator, but it makes G-code harder to manipulate and optimise.

To avoid this problem, one is driven to introduce explicitly-named values in the abstract machine, which is how the *T-code* of our earlier paper was derived (Peyton Jones & Salkild, 1989). Unfortunately, the simplicity of the abstract machine is now lost.

We take a slightly different approach here. Instead of defining a new abstract machine, we use a very small functional language, the *STG language*, as the abstract machine code.

It has the usual denotational semantics, so it is in principle possible to check the transformation of the original program into the STG language is correct. But we also give it a direct operational semantics using a state transition system, which explains how we intend it to be executed.

The problem of proving the entire system correct is thereby made easier than, for example, the G-machine, because only *the equivalence of the denotational and operational semantics of a single language is involved*. Even so, it is a substantial task, and we do not attempt it here.

The task of proving a simple G-machine correct is carried out by Lester in his thesis (Lester, 1989).


### 2.3 Part III: mapping the abstract machine onto real hardware

Typically, much is written about the compilation of a functional program into abstract machine code, and rather little about how to map the abstract machine onto the underlying hardware. Yet the abstract machine can only be considered a success if this mapping works well; that is, if the resulting code is efficient.

We believe that the Spineless Tagless G-machine comes out well in this regard, and devote considerable space to discussing the mapping process. One of the nice aspects is that a variety of mappings are possible, of increasing complexity and efficiency.

Our target machine code is the C language. This has become common of late, conferring, as it does, wide portability. We may pay some performance penalty for not generating native machine code, and plan to build other code generators which do so.

### 2.4 Source language and compilation route

We are interested in compiling strongly-typed, higher-order, non-strict, purely functional languages such as `Haskell` or `LML`. We expect heavy use of both higher-order functions and the non-strict semantics (Hughes, 1989).

This paper is only about the back end of a compiler. 
Our complete compilation route involves the following steps:

1. The primary source language is Haskell (Hudak et al., 1992), a strongly-typed, nonstrict, purely-functional language. Haskell's main innovative feature is its support for systematic overloading.

2. Haskell is compiled to a small **Core language**. All Haskell's syntactic sugar is translated out, type checking is performed, and overloading is resolved. *Pattern-matching is translated into simple case expressions*, each of which performs only a single level of matching.

3. Program analyses and a variety of transformations are applied to the Core language.

4. **The Core language is translated to the STG language**, which we introduce in Section 4. This transformation is rather simple.

5. **The code generator translates the STG language into Abstract C**. The latter is just an internal data type which can simply be printed out as C code, but which can also serve as an input to a native-code generator.


*Strictness analysis* plays an important role in compilers for non-strict languages, enabling the compiler to determine cases where function arguments can be passed in evaluated form, which is often more efficient. Using this technology, compilers for lazy languages can generate code which is sometimes as fast as or faster than C (Smetsers et al., 1991).

Usually the results of strictness analysis are passed to the code generator, which is thereby made significantly more complicated. We take a different approach: we extend the Core and STG languages with full-fledged *unboxed values*, which makes them expressive enough to incorporate the results of strictness analysis by simple program transformations.

We give a brief introduction to unboxed values in Section 4.7, but the full details, including the transformations required to exploit strictness analysis, are given in a separate paper (Peyton Jones & Launchbury, 1991).

The code generator, which is the subject of this paper, is therefore not directly involved in strictness analysis or its exploitation, so we do not discuss it further.

## Part I: Exploring the design space

## 3. Exploring the design space

The STG machine has its roots in lazy graph reduction. It is now folk-lore that, while graph reduction looks very different to conventional compiler technology, the best compilers based on graph reduction generate quite similar code to those for (say) Lisp. In this section we attempt to compare some aspects of the STG machine with more conventional compilers.

We attempt to compare some aspects of the STG machine with more conventional compilers. Any implementation is the result of a raft of inter-related design decisions, each of which is partly justified by the presence of the others.

We proceed by asking 3 key questions, which help to locate the implementation techniques for any non-strict higher-order language:

1. How are function values, data values and unevaluated expressions represented
2. How is function application performed
3. How is case analysis performed on data structures

This section gives the context and motivation for many of the implementation techniques described later.

### 3.1 The representation of closures

The heap contains two kinds of objects:
- **values** or Head Normal Forms (HNF)
  - function values
  - data values
- **thunks** or unevaluated suspensions


HNFs are further classified into two *function values* and *data values*.

A value may contain thunks inside it; e.g. a list `Cons` cell might have an unevaluated head and/or tail.

A value which contains no thunks inside it is called a **Normal Form** (NF).

In a polymorphic language, it is not always possible to distinguish thunks whose value will turn out to be a function, from thunks that turn out to be a data value. For example, consider the composition function:

```hs
compose f g x = f (g x)
```

Whether `g x` becomes a function or data value depends on the type of `g`, and since `compose` is polymorphic, this is not statically determined.

For reasons which will become apparent we use the term "closure" to refer to both values and thunks.

In the remainder of this section we consider various ways in which closures can be represented, contrasting the STG machine with other designs.


#### 3.1.1 Representing functions

>An implementation of a higher-order PL must provide a way to *represent a function value*. Such a value behaves like a suspended computation: when the value is applied to its arguments, the computation is performed.

The most compact way to represent a function value is as a block of *static code*, so that it gets shared by all *dynamic instances of the function value*, together with the values of its free variables.

  A function's body gets compiled as the static code, and the static code segment is accessible (visible) from anywhere. It is specified once, but shared by all dynamic instances of the function value. In this way, the static (in the sense unchangable) part of the function can just be referenced from each function's instance. A function call triggers the creation of the function's a new instance, that has own stack frame where its passed vars, local vars, return address is manged.

  This is commonly called a *closure*, but for reasons that will become apparent later, we use this term in a wider sense, to refer to both values and thunks.

>The most direct physical representation of such a closure is *a pointer to a contiguous block of heap-allocated storage*, consisting of a code pointer which points to the static code, followed by (pointers to) the values of the free variables.

```
pointers to free vars
  ┌─┴───┐
▣ ■ ■ … ■
│
pointer to the static code
```

This is the representation adopted by many compiled Lisp systems, by SML of New Jersey, and by the Spineless Tagless G-machine.

>To perform the computation, a distinguished register - *the environment pointer* is made to point to the closure, and the code is executed.

We call this operation *entering a closure*. The code can access its free variables by offsets from the environment pointer, and its arguments by some standard argument-passing convention (e.g. in registers, on the stack, or in an activation record).

  The code can access its free variables by offseting from the environment pointer, and it can acess its arguments as prescribed by the calling convention; e.g. a common convention says that some fixed number of args must be passed via registers, while the rest are passed via the stack (each conventiones has its own protocol, but to pass the args to a function, you can store them in registers + stack, all via stack, or some other methos like an activation record, or any combination of these). The call stack, generally, manages all function calls during an execution of a program, and each function call creates a new stack frame that is used to mange the return address, the stack frame pointer, pointers to other resources, etc.

Instead of storing the immediate values of the free variables in the closure, it is possible to store a pointer to a *block of free variables*, or even to a *chain of such blocks* (located in the heap). These representations attempt to save storage, at the cost of slowing down access. The "Orbit" compiler, for example, works hard to choose the best representation for closures, including allocating them on the stack rather than in the heap whenever possible, and sharing one block of free variables between several closures (Kranz, 1988). Apart from the compiler complexity involved, considerable extra care has to be taken in the *garbage collector* to avoid the *space leakage* which can occur when a closure captures a larger set of free variables than it requires. Appel's measurements for SML/NJ suggest that clever technics for representing closures gain little but potentially lose a lot (in space complexity), recommending a simple, flat, representation instead (Appel, 1992).

**The Three Instruction Machine (TIM)** takes another interesting position. Instead of representing a closure by a single pointer, it represents a closure by a pair of a code pointer and a pointer to a heap-allocated frame (Fairbairn & Wray, 1987). The frame, which is a vector of code-pointer and frame-pointer pairs, gives the values of the free variables of the closure, and may be shared between many closures. These code/frame pointer pairs need to be carefully handled in a lazy system, because they cannot be duplicated without the risk of also duplicating work. Proper sharing can still be ensured, but it results in a system remarkably similar to the more conventional ones.

#### 3.1.2 Representing thunks

In a non-strict language, values are passed to functions or stored in data structures in unevaluated form, and only evaluated when their value is actually needed (call-by-need).

Like function values, these unevaluated forms capture a suspended computation, and can be represented by a closure in the same way as a function value.

This particular type of closure is called a **thunk**, a term originating in the early, call-by-name, implementations of Algol. When the value of the thunk is required, the **thunk is forced**.

In principle, a thunk may be represented simply by a nullary function value, but this may be inefficient due to the risk of its repeated evaluation. This duplication of work can be avoided by using a so-called *lazy implementation* as follows: **when a thunk is forced for the first time, it is replaced with its value** (the update is propagated everywhere).

There are 3 main strategies for dealing with such updates in lazy implementations:
1. The naive reduction model
2. The cell model
3. The self-updating model


1. **The naive reduction model** updates the graph after each reduction (Peyton Jones, 1987). By reduction is meant the replacement of an instance of the lhs of a function definition by the corresponding instance of its rhs. Apart from a few optimisations, this is the update strategy used by the G-machine (Johnsson, 1984). Its main disadvantage is that a thunk may be updated with another thunk, so the same object may be updated repeatedly, and we do not consider this model further.

2. In **the cell model**, each closure is provided with a *status flag* to indicate whether it is evaluated or not. The code to force (that is, get the value of) a closure checks the status flag. If the closure is already evaluated, its value is extracted; otherwise the suspended computation is performed (by entering the closure), the value is written into the cell, and the status flag is flipped (Bloss, Hudak & Young, 1988).

3. **The self-updating model**, which is used by the STG machine.

The cell model places the responsibility for performing the update on the code which evaluates the thunk. The self-updating model instead places this *responsibility on the code inside the thunk itself*. 

The code to force a closure simply pushes a *continuation* on the stack and enters the closure: if the closure is a thunk, it arranges for an update to be performed when evaluation is complete; otherwise it just returns its value. No tests need be performed. 

*The update overwrites the thunk with a value*, which therefore must also have a code pointer, because subsequent forcing will re-enter the thunk-turned-value.

This representation is natural for function values, as we have already discussed, but is something of a surprise for *data values*. 

A list cell, for example, is represented by a code pointer together with the two pointers comprising the head and tail of the list. 

The code pointed to from the list cell simply returns immediately (later we explore variants of this scheme, in which the code for a list cell does rather more than simply return). 

In effect, the code pointer plays the role of the flag in cell model.


The latter two models each offer scope for optimisation.

Consider the cell model, for example. Forcing can be optimised if the compiler can prove that the thunk is certainly already evaluated (or the reverse), because the test on the flag can be omitted (Bloss, Hudak & Young, 1988). Furthermore, if the compiler can prove that there can be no subsequent code forcing on the thunk, then it can omit the code which performs the update.

A similar situation holds for the self-updating model. *If the compiler can prove that a particular thunk can only be evaluated at most once (which we expect to be quite common), it can create code for the thunk which doesn't perform the update*. Unlike the cell model, the self-updating model cannot take advantage of order-of-evaluation analyses.

#### 3.1.3 A uniform representation for closures

The self-updating model strongly suggests that every heap-allocated object (whether a HNF or a thunk) is represented uniformly, by a code pointer together with zero or more fields which give the values of free variables of the code.

The STG machine adopts this uniform representation, where all heap values are represented uniformly by some code together with a sequence of values.

Indeed this is why the machine is called *tagless*: since all objects have the same form there is no need for a tag to distinguish one kind of object from another (contrast the presentation in Peyton Jones, 1987).

We use the term *closure* to refer to both values and thunks because of their uniform representation.

The decision to use a uniform representation for all closures has other interesting ramifications, which we explore in this section.

Firstly, *when a thunk is updated with its value, it is possible that the value will take more space than the thunk*. In this case, the thunk must be updated with an indirection to the value. This causes no difficulty for the self-updating model, because indirections can be represented by a closure whose code simply enters the target closure. Such indirections can readily be removed during garbage collection.

Secondly, the self-updating model also allows other exceptional cases to be taken care of without extra tests. For example,

* *When a thunk is entered, its code pointer can be overwritten with a **black-hole** code pointer*. If the thunk is ever re-entered before it is updated, then its value must depend on itself. It follows that the program has entered an infinite loop, and a suitable error message can be displayed. Without this mechanism stack overflow occurs, which is less helpful to the programmer.

* In a system which supports concurrent threads of execution, exactly the same method can be used to synchronise threads. When a thunk is entered, its code pointer is overwritten with a "queue-me" code pointer. If another thread tries to evaluate the thunk before the first thread has completed, the former is suspended and added to a queue of threads attached to the thunk. When the thunk is updated, the queued threads are re-enabled.

* In a system with distributed memory, pointers to remote memory units often have to be treated differently to local pointers. However, it would be very expensive to test for remote-ness whenever dereferencing a pointer! In the self-updating model, a remote pointer can be represented as a special kind of indirection, and no tests for remote pointers need be performed.



### 3.2 Function application and the evaluation stack
