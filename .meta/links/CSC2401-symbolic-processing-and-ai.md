# CSC 2401: Symbolic Processing and AI

* CSC 2401: Symbolic Processing and AI. Fall 1995
https://web.archive.org/web/20111109175213/http://www.sju.edu/~jhodgson/ugai/aihome.html

Propositional Logic
https://web.archive.org/web/20070826161320/http://www.sju.edu/~jhodgson/ugai/prop.html

First Order Logic
https://web.archive.org/web/20070826161248/http://www.sju.edu/~jhodgson/ugai/1order.html

Inference
https://web.archive.org/web/20070822141138/http://www.sju.edu/~jhodgson/ugai/infer.html


## CSC2401: Additional References

* W. Clocksin and C. Mellish. Programming in Prolog. 4th Edition. Springer-Verlag.
I have placed this book on reserve. It is the original book on the language and still very good.
* Ehud Shapiro and Leon Sterling. The Art of Prolog. MIT press
Also on reserve. A very good book, written at a slightly more sophisticated level than C & M.
* Richard O'Keefe. The Craft of Prolog MIT press.
The contrasting title is intended. How to get things done in prolog. Contains lots of good code tricks.
* Ivan Bratko. Prolog programming for AI.
Good coverage of both Prolog and some basic AI techniques. All the programs actually work, although they are sometimes a bit too clever.
* Peter Flach. Simply Logical John Wiley
Somewhat terse but clear explanations of a number of basic ideas with Prolog code.
* Elaine Rich and Kevin Knight. Artificial Intelligence. McGraw Hill.
A classic undergraduate text.
* George Luger and William Stubblefield. Artificial Intelligence. Benjamin-Cummings
My second choice for text book. Has code in both LISP and Prolog.
* G. Gazdar and C. Mellish. Natural Language Programming in Prolog
A good introduction to the subject, with lots of Prolog code.
* Michael Covington. Natural Language Processing for Prolog programmers. Prentice Hall
Assumes more knowledge of Prolog than G & M and spends less time on linguisitc issues.
* Dennis Merritt. Building Expert Systems in Prolog. Springer Verlag.
A gentle introduction
* Steven Kim. Knowledel Systems through Prolog. Oxford UP.
Somewhat less gentle than Merritt but with fewer techniques.

### The Prolog Language

* SWI prolog.
A public domain version of prolog, runs under windows. Obtainable over the net from the University of Amsterdam.
* ALS prolog
A student edition of this is avalaible from ALS. Details can be found at "http://www.als.com/"
* BIM prolog
Available on turing and sjuphil locally.
* comp.lang.prolog
This is the prplog news group. Here you can find the responses to faqs (frequently asked questions). Use tin to read news on newshost.


## Propositional Logic

In propositional logic the symbols represent facts.
These facts are combined using Boolean connectives.

### The Sentences of Propositional Logic

The following is a grammar for the sentences of propositional logic.
* A sentence is either an atomic sentence or a complex sentence
* An atomic sentence = true or false or a literal.
* A complex sentence is one of
  - (Sentence)
  - Sentence Connective Sentence
  - ~ Sentence
* A Connective is one of /\ , \/ , <=> , or =>

The semantics of propositonal logic is given by the truth tables for /\ , \/ , ~ , < = > , and =>
Truth tables can be constructed for complex sentences.
This gives a complete and sound inference method.

A proposition is *valid* if it is true for all possible assignments of truth values to its atomic components.

It is satisfiable if there is an assignment of truth values to its components that makes it true. If there is no such assignment it is called unsatisfiable.

### Proof Rules for Propositional Logic

Modus Ponens
    From a = > b and a deduce b
and elimination
    From a1/\ a2 /\a3/\ .../\ak deduce a1
and introduction
    from a1 , a2, a3, ... , ak deduce a1/\ a2 /\a3/\ .../\ak
Or introduction
    from a1 deduce a1 \/ a2 \/ a3\/ ...\/ ak
Double negation elimination
    from ~ ~ a deduce a
Unit resolution
    from a \/ b and ~b deduce a
Resolution
    from a \/ b and ~b \/ c deduce a \/ c. This is the same as from ~a = > b and b = > c deduce ~a = > c



## First Order Logic

First order logic allows the use of sentences that contain variables.

So that rather than the proposition *Socrates is a man* one can have sentences of the form *X is a man* where X is a variable.

To work with these things we need to enrich the grammar of propositional logic with two extra notions.

Terms
terms are used to refer to objects as `mother_of(Mary)`

Quantifiers
These are used to express properties of collections of objects.


### The grammar of First Order Logic

We provide the grammar in a slightly less formal presentation than that in AIMA. As before the goal is to describe the syntactically allowable sentences.
* A sentence is one of:
  - An atomic sentence
  - Sentence Connective Sentence
  - Quantifier Variable Sentence
  - ~ Sentence
  - (Sentence)
* An atomic sentence is one of
  - Predication(Term, ... ) (A function whose range is {true, false})
  - Term = Term
* A term is one of
  - Function(Term, ...)
  - Constant
  - Variable
* A connective is one of = > , /\, \/, < = >
* A quantifier is one of E (there exists, actually backwards E ) or A ( for all, actually upside down A)
* The constants are drawn from some set of constants , (a , b, c, John, etc)
* The variables are drwn from som eset of variables, (x,y,z, etc)
* the predicates are drawn from some set of predicate names ( next_to, has_color, etc)
* The function names are drawn from some set of function names (mother, sizeof etc)


### Semantics

The notion of a model for a set of sentences in first order logic has a natural definition. At the risk of some oversimplification, there is a universe U, which contains all the objects referred to by the constans, and on which all the functions and predicates are defined.

A sentence is satisfiable if the variables that appear in it can be chosen from the Universe so as to make it true. This leaves only quantification. Here is a quick semantics for the quantifiers.
- (Ex) f(x) is true iff f(x) is satisfiable.
- (Ax) f(x) is true iff ~ (Ex) (~f(x)) is true.

First order logic is much harder than propositional logic. Proof procedures are harder to use because there are so many more choices, we will not pursue the matter here. Some ideas on proof procedures for FOL are described in the page on inference.


### Using FOL to model the real world

If agents are to be able to work sucessfully in the real world they need to be able to reason about the consequences of their actions. Note that reflex agents do not do this, making them unsuitable for applications in which the world changes. On the other hand, it is easy to see how a reflex agent's behaviour is expressible in terms of FOL. Thus:

`forall s,b,u,c,t (Precept([s, b, Glitter, u, c], t) => Action(Grab, t)`

encodes the desirablity of grabbing the gold when you see glitter.

Unfortunately reflex agents will always respond to a given percept set in the same way. So that they will end never be able to make a different choice from their first one. This is where some reasoning ability is needed. If we made the reflex agent inot a table driven agent that reacted based upon the sequence of past percepts then presumably we could choose differently on reencountering the same percept at the same place. As before it is unfeasible to store all this information. Fortunately it is enough to be able to keep track of the present state.

Michael Leyton in his book, "Symmetry, Causality, Mind" makes the interesting observation that asymmetry is history so that as T.S. Eliot says:

*"Time present and time past*
*Are both perhaps present in time future,*

One way in which one can keep track of the present state is through the *situational calculus*.
https://web.archive.org/web/20070826161248/http://www.sju.edu/~jhodgson/ugai/situate.html


## Inference

### Forward and Backward Chaining

An inference problem can be formulated as follows:
- There is a set of sentences given as true, the hypotheses
- There is a sentence which it is desired to prove, call this the conclusion
- There are rules of inferecne that permit the deduction of additional sentences from given ones.
- Use the given rules of inference to deduce the conclusion from the hypotheses.

For concreteness call the set of given sentences the Knowledge base KB, the conclusion g. Then we want to show KB |- g where |- stands for the use of our given inference procedure. This might for example be the generalised modus ponens of page 269 of AIMA.

In trying to construct a proof we can describe two apporaches. These are not exhaustive but they embody the two major "local" strategies.

### Forward chaining

Given the knowledge base KB we can find an inference rule

Hypotheses
-----------
Conclusion

whose hypotheses unify with sentences in KB, we can then add the conclusion to the knowledge base (after making the substitutions required to math the hypotheses to the sentences of KB). This gives us a new KB in which more in known and we can repeat the process until g appears as the conclusion of one of these steps. We are then done.

### Backward chaining
This time we start with g and look for an inference rule

Hypotheses
-----------
Conclusion

such that the conclusion can be unified with g. It then is enough to proof the hypotheses of this rule.

Note that Forward chaining can produce many irrelevant conclusions before arriving at a proof. Whereas backward chaining may pursue a misleading line of proof without some control.

### Resolution

Resolution is a simple proof rule that is a complete inference procedure, at least in the sense that if the sentnces in the hypothesis lead to a contradiction resolution will determine that fact.

For literals pi and qi where pj and ~qk unify with unifier s

p1 \/ p2\/ ...\/pj\/ ...\/ pm
q1\/...qk\/... qn
-----------------------------------------------
subst(s, (p1\/p2 .. pj-1\/pj+1\/..\/pm\/q1\/..qk-1\/qk+1..\/qn))

Note that resolution can be used in either direction.

### Canonical form for resolution

Sentences in FOL can be placed in Conjunctive normal form, meaning that they have the form P \/ Q \/ R

Negations can appear P but no quantifiers.

The process by which this is done uses these steps:
- Eliminate implications
- Move not inwards
- Rename bound variables so that they are all distinct
- Move all quantifiers to the left
- Skolemize to remove quantifiers
- Distribute /\ over \/
- Flatten nested conjuncts and disjuncts

It should not have escaped your notice that Prolog uses resolution.

## The Situational Calculus

The goal of the situational calculus is to be able to reason about change. This is done by maintaining an internal model of the world and mdifying it as actions take place. We will assume that the only effects that the agent needs to worry about are those that are caused by its own actions.

SC conceives of the world as a sequence of situations. There are then axioms that relate successive situations.

* Effect axioms:
Describe the effect of a particular action. Here for example is a an effect axiom for grabbing something:

    Forall object, situation Present(object,situation) /\ portable(object) => Holding(object, Result(grab, situation))

* Frame Axioms:
Not only do actions have effects, we also need to know what is unchanged when we do something. One difficulty is that potentially we require an unlimited number of axioms to deal with this. In practice there are ways of dealing with this. Here is an example frame axiom:

    Forall action, object, situation Holding (object, situation) /\ action not Release => Holding(object, Result(action, situation))

* successor-state axioms
describe the way in which predicates can change over time. The axiom must list all the ways in which a predicate can become true, and in which it can become false.


---

# CSC 2401: Course Outline

https://web.archive.org/web/20181110053908/http://people.sju.edu/~jhodgson/ugai/outline.html

- [Introduction](https://web.archive.org/web/20181110053908/http://people.sju.edu/~jhodgson/ugai/intro.html)
- [Intelligent Agents](https://web.archive.org/web/20181110053908/http://people.sju.edu/~jhodgson/ugai/agent.html)
- [The prolog Language](https://web.archive.org/web/20181110053908/http://people.sju.edu/~jhodgson/ugai/prolog.html)
- [Search](https://web.archive.org/web/20181110053908/http://people.sju.edu/~jhodgson/ugai/search.html)
- [Informed Search](https://web.archive.org/web/20181110053908/http://people.sju.edu/~jhodgson/ugai/infsrch.html)
- [Agents that reason logically](https://web.archive.org/web/20181110053908/http://people.sju.edu/~jhodgson/ugai/logical.html)
- [First order logic](https://web.archive.org/web/20181110053908/http://people.sju.edu/~jhodgson/ugai/1order.html)
- [Inference](https://web.archive.org/web/20181110053908/http://people.sju.edu/~jhodgson/ugai/infer.html)
- [Planning](https://web.archive.org/web/20181110053908/http://people.sju.edu/~jhodgson/ugai/plan.html)
- [Agents that communicate](https://web.archive.org/web/20181110053908/http://people.sju.edu/~jhodgson/ugai/commun.html)
- [Practical Natural Language Processing](https://web.archive.org/web/20181110053908/http://people.sju.edu/~jhodgson/ugai/nlp.html)
