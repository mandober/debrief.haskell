# Laziness

## 3.2 Haskell is lazy

Technically, Haskell is a language with a **non-strict semantics**. *Lazy evaluation* is just an implementation technique for a non-strict language.

When referring specifically to implementation techniques the term *"call-by-need"* is used, which contrasts call-by-value mechanism of languages like ML and Lisp.

Hughes's paper "Why functional programming matters" (Hughes, 1989) captured these in an influential manifesto for lazy programming, and coincided with the early stages of Haskell's design. Hughes first presented it in 1984, after which it circulated informally before finally being published in 1989.

*Call-by-need* is less efficient than *call-by-value* because of the extra bookkeeping required to delay evaluation until a term is required, so that some terms remain unevaluated, and to overwrite a term with its value, so that no term is evaluated twice. This cost is a significant but constant factor, and was understood at the time Haskell was designed.

A much more important problem is this: it is very hard for even experienced programmers to predict the space behaviour of lazy programs, and there can be much more than a constant factor at stake. The prevalence of the space leaks has led to adding some strict features to Haskell, such as `seq` and strict data types. As a result, the strict/lazy divide has become much less an all-or-nothing decision, and the practitioners of each recognise the value of the other.

## 3.2 Haskell is pure

An immediate consequence of laziness is that evaluation order is demand-driven. As a result, it becomes more or less impossible to reliably perform IO or other side effects as the result of a function call.

Therefore, Haskell is a pure language. For example, if a function `f` has type `Int -> Int` you can be sure that `f` will not read or write any mutable variables, nor will it perform any IO. In short, `f` really is a function in the mathematical sense: every call with the same argument returns the same value.

Once Haskell was committed to laziness, purity was inescapable. The converse is not true, but it is notable that in practice most pure programming languages are also lazy. This is because in a call-by-value language, whether functional or not, the temptation to allow unrestricted side effects inside a procedure is almost irresistible.

Purity is a big bet, with pervasive consequences. Unrestricted side effects are undoubtedly very convenient. Lacking side effects, Haskell's IO was initially painfully clumsy. Necessity being the mother of invention, this ultimately led to the invention of *monadic IO*, which we now regard as one of Haskell's main contributions to the world.

Whether a pure language (with monadic effects) is ultimately the best way to write programs is still an open question, but it certainly is a radical and elegant attack on the challenge of programming, and it was that combination of power and beauty that motivated the designers. In retrospect, therefore, perhaps the biggest single benefit of laziness is not laziness per se, but rather that laziness has kept Haskell pure, motivating a great deal of work on monads and encapsulated state.


## 3.3 Haskell has type classes

Although laziness was what brought Haskell’s designers together, it
is perhaps type classes that are now regarded as Haskell’s most distinctive characteristic. Type classes were introduced to the Haskell
Committee by Wadler in a message sent to the fplangc mailing
list dated 24 February 1988.
Initially, type classes were motivated by the narrow problem of
overloading of numeric operators and equality. These problems had
been solved in completely different ways in Miranda and SML.
SML used overloading for the built-in numeric operators, resolved
at the point of call. This made it hard to define new numeric operations in terms of old. If one wanted to define, say, square in terms of
multiplication, then one had to define a different version for each
numeric type, say integers and floats. Miranda avoided this problem by having only a single numeric type, called num, which was a
union of unbounded-size integers and double-precision floats, with
automatic conversion of int to float when required. This is convenient and flexible but sacrifices some of the advantages of static
typing – for example, in Miranda the expression (mod 8 3.4) is
type-correct, even though in most languages the modulus operator
mod only makes sense for integer moduli.

SML also originally used overloading for equality, so one could not
define the polymorphic function that took a list and a value and returned true if the value was equal to some element of the list. (To
define this function, one would have to pass in an equality-testing
function as an extra argument.) Miranda simply gave equality a
polymorphic type, but this made equality well defined on function
types (it raised an error at run time) and on abstract types (it compared their underlying representation for equality, a violation of the
abstraction barrier). A later version of SML included polymorphic
equality, but introduced special “equality type variables” (written
’’a instead of ’a) that ranged only over types for which equality
was defined (that is, not function types or abstract types).
Type classes provided a uniform solution to both of these problems.
They generalised the notion of equality type variables from SML,
introducing a notion of a “class” of types that possessed a given set
of operations (such as numeric operations or equality).
The type-class solution was attractive to us because it seemed more
principled, systematic and modular than any of the alternatives; so,
despite its rather radical and unproven nature, it was adopted by
acclamation. Little did we know what we were letting ourselves in
for!
Wadler conceived of type classes in a conversation with Joe Fasel
after one of the Haskell meetings. Fasel had in mind a different
idea, but it was he who had the key insight that overloading should
be reflected in the type of the function. Wadler misunderstood what
Fasel had in mind, and type classes were born! Wadler’s student
Steven Blott helped to formulate the type rules, and proved the
system sound, complete, and coherent for his doctoral dissertation
(Wadler and Blott, 1989; Blott, 1991). A similar idea was formulated independently by Stefan Kaes (Kaes, 1988).
We elaborate on some of the details and consequences of the typeclass approach in Section 6. Meanwhile, it is instructive to reflect
on the somewhat accidental nature of such a fundamental and farreaching aspect of the Haskell language. It was a happy coincidence
of timing that Wadler and Blott happened to produce this key idea
at just the moment when the language design was still in flux.
It was adopted, with little debate, in direct contradiction to our
implicit goal of embodying a tried-and-tested consensus. It had
far-reaching consequences that dramatically exceeded our initial
reason for adopting it in the first place.
