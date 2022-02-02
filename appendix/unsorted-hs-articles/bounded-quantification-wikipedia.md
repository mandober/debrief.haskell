# Bounded quantification - Wikipedia

> This article is about bounded quantification in type theory. For bounded quantification in mathematical logic, see Bounded quantifier.

This article is about bounded quantification in type theory. For bounded quantification in mathematical logic, see [Bounded quantifier](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Bounded_quantifier "Bounded quantifier").

In [type theory](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Type_theory "Type theory"), **bounded quantification** (also **bounded polymorphism** or **constrained genericity**) refers to [universal](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Universally_quantified "Universally quantified") or [existential quantifiers](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Existential_type "Existential type") which are restricted ("bounded") to range only over the subtypes of a particular type. Bounded quantification is an interaction of [parametric polymorphism](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Parametric_polymorphism "Parametric polymorphism") with [subtyping](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Subtyping "Subtyping"). Bounded quantification has traditionally been studied in the [functional](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Functional_programming "Functional programming") setting of [System F<:](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/System_F-sub "System F-sub"), but is available in modern [object-oriented languages](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Object-oriented_language "Object-oriented language") supporting [parametric polymorphism](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Parametric_polymorphism "Parametric polymorphism") ([generics](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Generic_programming "Generic programming")) such as [Java](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Java_(programming_language) "Java (programming language)"), [C#](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/C_Sharp_(programming_language) "C Sharp (programming language)") and [Scala](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Scala_(programming_language) "Scala (programming language)").

Overview\[[edit](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Bounded_quantification&action=edit&section=1 "Edit section: Overview")\]
---------------------------------------------------------------------------------------------------------------------------------------------------------------

The purpose of bounded quantification is to allow for [polymorphic functions](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Polymorphic_function "Polymorphic function") to depend on some specific behaviour of objects instead of [type inheritance](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Type_inheritance "Type inheritance"). It assumes a record-based model for object classes, where every class member is a record element and all class members are named functions. Object attributes are represented as functions that take no argument and return an object. The specific behaviour is then some function name along with the types of the arguments and the return type. Bounded quantification allows to considers all objects with such a function. An example would be a polymorphic `min` function that considers all objects that are comparable to each other.

### F-bounded quantification\[[edit](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Bounded_quantification&action=edit&section=2 "Edit section: F-bounded quantification")\]

**_F_\-bounded quantification** or **recursively bounded quantification**, introduced in 1989, allows for more precise typing of functions that are applied on recursive types. A recursive type is one that includes a function that uses it as a type for some argument or its return value.[\[1\]](#cite_note-1)

Example\[[edit](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Bounded_quantification&action=edit&section=3 "Edit section: Example")\]
-------------------------------------------------------------------------------------------------------------------------------------------------------------

This kind of type constraint can be expressed in [Java](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Java_(programming_language) "Java (programming language)") with a generic interface. The following example demonstrates how to describe types that can be compared to each other and use this as typing information in [polymorphic functions](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Polymorphic_function "Polymorphic function"). The `Test.min` function uses simple bounded quantification and does not preserve the type of the assigned types, in contrast with the `Test.Fmin` function which uses F-bounded quantification.

In mathematical notation, the types of the two functions are

min: ∀ T, ∀ S ⊆ {compareTo: T → int}. S → S → S

Fmin: ∀ T ⊆ Comparable\[T\]. T → T → T

where

Comparable\[T\] = {compareTo: T → int}

interface Comparable<T\> {
  public int compareTo(T other);
}

class Integer implements Comparable<Integer\> {
  @Override
  public int compareTo(Integer other) {
    //...
  }
}

class String implements Comparable<String\> {
  @Override
  public int compareTo(String other) {
    //...
  }
}

class Test {
  public static void main(String\[\] args) {
    Comparable<String\> a \= min("cat", "dog");
    Comparable<Integer\> b \= min(new Integer(10), new Integer(3));
    String str \= Fmin("cat", "dog");
    Integer i \= Fmin(new Integer(10), new Integer(3));
  }
  public static <S extends Comparable\> S min(S a, S b) {
    if (a.compareTo(b) <= 0)
      return a;
    else
      return b;
  }
  public static <T extends Comparable<T\>> T Fmin(T a, T b) {
    if (a.compareTo(b) <= 0)
      return a;
    else
      return b;
  }
}

See also\[[edit](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Bounded_quantification&action=edit&section=4 "Edit section: See also")\]
---------------------------------------------------------------------------------------------------------------------------------------------------------------

*   [Covariance and contravariance (computer science)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Covariance_and_contravariance_(computer_science) "Covariance and contravariance (computer science)")
*   [Curiously recurring template pattern](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Curiously_recurring_template_pattern "Curiously recurring template pattern")
*   [Wildcard (Java)](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Wildcard_(Java) "Wildcard (Java)")

Notes\[[edit](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Bounded_quantification&action=edit&section=5 "Edit section: Notes")\]
---------------------------------------------------------------------------------------------------------------------------------------------------------

References\[[edit](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Bounded_quantification&action=edit&section=6 "Edit section: References")\]
-------------------------------------------------------------------------------------------------------------------------------------------------------------------

*   [Cardelli, Luca](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Luca_Cardelli "Luca Cardelli"); [Wegner, Peter](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Peter_Wegner "Peter Wegner") (December 1985). ["On Understanding Types, Data Abstraction, and Polymorphism"](http://lucacardelli.name/Papers/OnUnderstanding.A4.pdf) (PDF). _[ACM Computing Surveys](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/ACM_Computing_Surveys "ACM Computing Surveys")_. **17** (4): 471–523. [CiteSeerX](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/CiteSeerX_(identifier) "CiteSeerX (identifier)") [10.1.1.117.695](chrome-extension://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.117.695). [doi](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Doi_(identifier) "Doi (identifier)"):[10.1145/6041.6042](https://doi.org/10.1145%2F6041.6042). [ISSN](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/ISSN_(identifier) "ISSN (identifier)") [0360-0300](chrome-extension://www.worldcat.org/issn/0360-0300).
*   [Peter S. Canning](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Peter_S._Canning&action=edit&redlink=1 "Peter S. Canning (page does not exist)"), [William R. Cook](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/William_Cook_(computer_scientist) "William Cook (computer scientist)"), [Walter L. Hill](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Walter_L._Hill&action=edit&redlink=1 "Walter L. Hill (page does not exist)"), [John C. Mitchell](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/John_C._Mitchell "John C. Mitchell"), and [William Olthoff](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=William_Olthoff&action=edit&redlink=1 "William Olthoff (page does not exist)"). ["F-bounded polymorphism for object-oriented programming"](http://dl.acm.org/citation.cfm?id=99392). In _Conference on Functional Programming Languages and Computer Architecture_, 1989.
*   [Benjamin C. Pierce](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Benjamin_C._Pierce "Benjamin C. Pierce") "Intersection types and bounded polymorphism". _Lecture Notes in Computer Science_ **664**, 1993.
*   [Gilad Bracha](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Gilad_Bracha "Gilad Bracha"), [Martin Odersky](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Martin_Odersky "Martin Odersky"), [David Stoutamire](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=David_Stoutamire&action=edit&redlink=1 "David Stoutamire (page does not exist)"), and [Philip Wadler](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Philip_Wadler "Philip Wadler"). "Making the future safe for the past: Adding genericity to the Java programming language". In _Object-Oriented Programming: Systems, Languages, Applications_ (OOPSLA). ACM, October 1998.
*   [Andrew Kennedy](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Andrew_Kennedy_(computer_scientist)&action=edit&redlink=1 "Andrew Kennedy (computer scientist) (page does not exist)") and [Don Syme](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Don_Syme "Don Syme"). "Design and Implementation of Generics for the .NET Common Language Runtime". In _Programming Language Design and Implementation_, 2001.
*   [Pierce, Benjamin C.](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Benjamin_C._Pierce "Benjamin C. Pierce") (2002). _Types and Programming Languages_. MIT Press. [ISBN](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/ISBN_(identifier) "ISBN (identifier)") [978-0-262-16209-8](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Special:BookSources/978-0-262-16209-8 "Special:BookSources/978-0-262-16209-8")., Chapter 26: Bounded quantification

External links\[[edit](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/w/index.php?title=Bounded_quantification&action=edit&section=7 "Edit section: External links")\]
---------------------------------------------------------------------------------------------------------------------------------------------------------------------------

*   [Bounded Polymorphism](http://www.c2.com/cgi/wiki?BoundedPolymorphism) at the [Portland Pattern Repository](chrome-extension://cjedbglnccaioiolemnfhjncicchinao/wiki/Portland_Pattern_Repository "Portland Pattern Repository")
*   ["F-bounded Polymorphism"](http://www.cs.washington.edu/research/projects/cecil/www/Vortex-Three-Zero/doc-cecil-lang/cecil-spec-86.html) in _The Cecil Language: Specification and Rationale_


[Source](https://en.wikipedia.org/wiki/Bounded_quantification)