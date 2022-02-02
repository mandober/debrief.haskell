# Summary: Type system

* Type systems provide the means to classify all the different (in any sense) terms in a PL, via a typing mechanism. Then, each term (value) is associated to a single type; in this sense types are like sets, and terms are their members.

* Having a type system at all is primarily for the sake for developers. Once a type system exists, the compiler will surely be interesting in using it, but it could do without one - a type system is really there for the users, it makes it sognificantly harder for them to accidentally shoot themselves in the foot.

* Types are round at compile-time, but after that phase they're gone. Usually, there are no types around at runtime, they are not needed at that time, so they are discarded.

* The type of a function specifies (partially) what it does. Although weak as a specification language, static types have compensating virtues: they are lightweight so programmers use them, machine-checked with minimal programmer assistance, ubiquitous so programmers cannot avoid them. As a result, static type checking is by far the most widely used verification technology today.
