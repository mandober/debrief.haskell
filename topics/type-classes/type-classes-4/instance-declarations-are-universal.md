---
downloaded:       2021-12-11
page-url:         https://www.lochan.org/keith/publications/undec.html
page-title:       Instance Declarations are Universal
article-title:    Instance Declarations are Universal
---
# Instance Declarations are Universal

Keith Wansbrough
Department of Computing Science
University of Glasgow
keithw@dcs.gla.ac.uk
http://www.dcs.gla.ac.uk/~keithw/
Keith Wansbrough  
Department of Computing Science  
University of Glasgow  
[keithw@dcs.gla.ac.uk][1]  
[http://www.dcs.gla.ac.uk/~keithw/][2]

July 31, 1998

Following recent discussions about instance declarations in Haskell-2 on the Haskell mailing list, and the suggestion that without sufficient restrictions in this area Haskell's type system would become undecidable, I decided to demonstrate this directly. In this brief paper I present a construction that encodes a Turing machine directly in a series of instance declarations. The encoding is such that it forces the type checker to simulate the operation of a specified Turing machine in order to check the program. If the Turing machine terminates then the program is well-typed; if the Turing machine fails to terminate then it is ill-typed. The well-known undecidability of the Halting problem thereby carries across to the unrestricted version of the Haskell-2 type system.

A technically superfluous but convenient feature of the encoding is that a term is provided that, for a well-typed program, yields the final configuration as a result. It must be stressed that the Turing machine simulation occurs at *type-checking* time, even though the final configuration cannot be inspected until run time. This can easily be verified by attempting to type check an encoding of a non-terminating Turing machine.

The undecidability of this type system does not necessarily mean it is not useful. Indeed, the termination of a Haskell program is also undecidable, yet Haskell is (therefore??) a useful programming language. However, undecidability in the *type system* is unusual and it is not clear whether or not it is acceptable.

## Contents

-   [The programs][3].
-   [The example][4].
-   [The detail][5].
-   [How it works][6].

## The programs

The encoding is performed by a (conventional) Haskell program, [undecgen.lhs][7]. This contains a specification of a Turing machine, and a generator program. When the function `main` is invoked, a new literate Haskell script is generated, containing the encoded representation of the Turing machine.

The latter script, [tmdupl.lhs][8] (pre-generated copy), must be run under [Hugs 1.3c][9] or some other Haskell environment that supports arbitrary instance contexts and multiple parameter type classes. When the script is loaded, the type checker will simulate the Turing machine; if this process terminates then invoking `main` will display the final configuration.

## The example

The example given is a simple 11-state Turing machine which, given a string of 1s bounded by a 0 to the left of the head in the initial position, copies the string (separated by a 0) and leaves the head to the right of the second copy. For example, it turns `01110` into `011101110`. The example is adapted from p.188 of Harry Lewis and Christos Papadimitriou, Elements of the Theory of Computation, Prentice-Hall, 1981. As noted in the source file, changing a single rule will convert this machine to a non-terminating one.

Here is a sample run:

1crab %hugs
      \_\_\_    \_\_\_   \_\_\_    \_\_\_   \_\_\_\_\_\_\_\_\_\_   \_\_\_\_\_\_\_\_\_\_                        
     /  /   /  /  /  /   /  /  /  \_\_\_\_\_\_\_/  /  \_\_\_\_\_\_\_/         Hugs 1.4       
    /  /\_\_\_/  /  /  /   /  /  /  / \_\_\_\_\_   /  /\_\_\_\_\_\_                          
   /  \_\_\_\_   /  /  /   /  /  /  / /\_   /  /\_\_\_\_\_\_   /  The Nottingham and Yale
  /  /   /  /  /  /\_\_\_/  /  /  /\_\_\_/  /  \_\_\_\_\_\_\_/  /    Haskell User's System 
 /\_\_/   /\_\_/  /\_\_\_\_\_\_\_\_\_/  /\_\_\_\_\_\_\_\_\_/  /\_\_\_\_\_\_\_\_\_/         January 1998

   Copyright (c) The University of Nottingham and Yale University, 1994-1997.
    Bug reports: hugs-bugs@haskell.org.   Web: http://www.haskell.org/hugs.

Reading file "/local/fp/lib/hugs/1.4-01-1998/hugs/lib/Prelude.hs":
Parsing
Dependency analysis
Type checking
Compiling
Hugs session for:
/local/fp/lib/hugs/1.4-01-1998/hugs/lib/Prelude.hs
Type :? for help
> :l undecgen.lhs
Reading file "undecgen.lhs":
Parsing
Dependency analysis
Type checking
Compiling         
Hugs session for:
/local/fp/lib/hugs/1.4-01-1998/hugs/lib/Prelude.hs
undecgen.lhs
> main

> :q
\[Leaving Hugs\]
2crab %hugs1.3c
  \_\_   \_\_ \_\_  \_\_  \_\_\_\_   \_\_\_     \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
  ||   || ||  || ||  || ||\_\_     Hugs 1.3c: The Haskell User's Gofer System
  ||\_\_\_|| ||\_\_|| ||\_\_||  \_\_||    Copyright (c) Mark P Jones,
  ||---||         \_\_\_||          The University of Nottingham, 1994-1998.
  ||   ||                        Report bugs to hugs-bugs@haskell.org
  ||   ||     \[March 1998\]       \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_

Reading script file "/users/grad/keithw/lib/hugs1.3c/lib/Prelude.hs":
Parsing
Dependency analysis
Type checking
Compiling         
Hugs session for:
/users/grad/keithw/lib/hugs1.3c/lib/Prelude.hs
Type :? for help
? :l tmdupl.lhs
Reading script file "tmdupl.lhs":
Parsing
Dependency analysis
Type checking
Compiling         
Hugs session for:
/users/grad/keithw/lib/hugs1.3c/lib/Prelude.hs
tmdupl.lhs
? main
Halted at state Q10.  Final configuration is:
(((((((((),S0),S1),S1),S1),S0),S1),S1),S1) <# S0 #> ()
? :q
\[Leaving Hugs\]
3crab %

After the abovementioned edit, this changes to the following. Notice how the nontermination shows up at *type-checking time*, thus confirming that the type checker is simulating the Turing machine.

1crab %hugs1.3c
  \_\_   \_\_ \_\_  \_\_  \_\_\_\_   \_\_\_     \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_
  ||   || ||  || ||  || ||\_\_     Hugs 1.3c: The Haskell User's Gofer System
  ||\_\_\_|| ||\_\_|| ||\_\_||  \_\_||    Copyright (c) Mark P Jones,
  ||---||         \_\_\_||          The University of Nottingham, 1994-1998.
  ||   ||                        Report bugs to hugs-bugs@haskell.org
  ||   ||     \[March 1998\]       \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_

Reading script file "/users/grad/keithw/lib/hugs1.3c/lib/Prelude.hs":
Parsing
Dependency analysis
Type checking
Compiling         
Hugs session for:
/users/grad/keithw/lib/hugs1.3c/lib/Prelude.hs
Type :? for help
? :l tmdupl.lhs
Reading script file "tmdupl.lhs":
Parsing
Dependency analysis
Type checking^C{Interrupted!}

? :q
\[Leaving Hugs\]
2crab %

## The detail

A Turing machine configuration is represented by an instance of a multiple-parameter type class `TM`:

\> class TM     q    lss    s    rss where
>   tmfinal :: q -> lss -> s -> rss -> String

Here `q` is the machine state, `lss` the left-hand portion of the tape, `s` the current symbol, and `rss` the right-hand portion of the tape. The method `tmfinal` returns a string describing the final state of the Turing machine when started in this configuration.

A machine state is represented by a type `Qn` containing a single element of the same name:

\> data Q0 = Q0
> data Q1 = Q1
...
> data Q10 = Q10

A tape symbol is represented by a type `Sn` containing a single element of the same name. It must be `Show`able to enable the final configuration to be displayed.

\> data S0 = S0 deriving Show
> data S1 = S1 deriving Show

A portion of tape extending potentially infinitely in one direction is represented by a series of nested pairs. For the left-hand portion, the tape is either `()` (unit) representing all `S0`s, or a pair `(lss,ls)` representing the tape `lss` followed by the symbol `ls`. The right-hand portion is similar but the pair is swapped: `(rs,rss)`.

A transition begins with a comment describing it, followed by the instance declarations. The *head* of the instance declaration is the *initial* state, and the *context* is the *final* state; note that this gives the instance declaration a counterintuitive direction. The method declaration of `tmfinal`, however, is in the expected order.

\==> (Q0,S0)->(L,Q1):

> instance TM Q1  lss     ls (S0,rss) =>     TM Q0 (lss,ls) S0     rss  where
>   tmfinal   Q0 (lss,ls) S0     rss  = tmfinal Q1  lss     ls (S0,rss)

> instance TM Q1  ()      S0 (S0,rss) =>     TM Q0  ()      S0     rss  where
>   tmfinal   Q0  ()      S0     rss  = tmfinal Q1  ()      S0 (S0,rss)

The first instance declaration gives the rule in the general case; the second gives the rule to extend the tape when necessary. Note that these do not overlap; in fact, this implementation does not require overlapping instances at all.

A halting \`transition' looks slightly different:

\==> (Q10,S0)->(HALT,Q10):

> instance (Show lss, Show rss)       =>     TM Q10  lss     S0     rss  where
>   tmfinal   Q10  lss     S0     rss  = "Halted at state Q10.  "
>                                        ++ "Final configuration is:\\n"
>                                        ++ show lss ++ " <# "
>                                        ++ show S0 ++ " #> " ++ show rss

Here there is no final state for the transition, and hence the instance context does not contain a machine state. Instead, we need to be able to display the final configuration in `tmfinal`, so we require the tapes to be `Show`able. They will be, since individual symbols are and the rule for pairs is built in.

Finally, an initial state is given to the type checker in the declaration of `main`:

\> main :: IO ()

> main = putStr (tmfinal Q0 ((((),S1),S1),S1) S0 ())

## How it works

The operation is fairly simple. We start by trying to type an initial configuration: since we use `tmfinal` at a particular set of types, we need to find the appropriate instance declaration. The type checker matches the initial configuration against the head of the appropriate declaration, but then discovers that the context requires another configuration. It searches for *this*, and matches against the appropriate head. This causes another transition; and so on.

If eventually we meet a configuration with a context we can resolve (i.e., a halting configuration), the process terminates and the type checker has proven the program well-typed (i.e., that the TM halts). Then at *run time*, the invocation of `tmfinal` eventually invokes the appropriate code in the halting configuration and displays the final configuration as desired.

We cannot distinguish, however, between a (machine,instance) pair that simply runs for a long time and a pair that never terminates. Obviously in certain limited cases it *is* possible, but the familiar diagonalisation argument shows it is not possible in general. Either we restrict the set of acceptable instance declarations until only terminating (machine,instance) pairs are permitted (but thereby rule out many perfectly valid types along with the invalid ones); or we live with the fact that some types may cause the type checker to fail to terminate, rather than yielding an error.

*Keith Wansbrough, July 31, 1998*

---

Back to my [home page][10].

\--KW 8-)

---

Document: http://www.cl.cam.ac.uk/users/kw217/research/misc/undec.html  
Last updated: Fri, 31 Jul 1998 13:48:49 BST  
Author: [KSW][11] <[kw217@cl.cam.ac.uk][12]\>.

[1]: mailto:keithw@dcs.gla.ac.uk
[2]: http://www.dcs.gla.ac.uk/~keithw/
[3]: https://www.lochan.org/keith/publications/undec.html#programs
[4]: https://www.lochan.org/keith/publications/undec.html#example
[5]: https://www.lochan.org/keith/publications/undec.html#detail
[6]: https://www.lochan.org/keith/publications/undec.html#how
[7]: https://www.lochan.org/keith/publications/undecgen.lhs
[8]: https://www.lochan.org/keith/publications/tmdupl.lhs
[9]: http://www.cs.nott.ac.uk/~mpj/hugs13/hugsget13c.html
[10]: https://www.lochan.org/keith/
[11]: http://www.cl.cam.ac.uk/users/kw217/
[12]: mailto:kw217@cl.cam.ac.uk
