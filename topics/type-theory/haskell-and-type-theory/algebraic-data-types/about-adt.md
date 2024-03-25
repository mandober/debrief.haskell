# Algebraic data types

* ADTs fulfill a similar role to objects in other languages, but with more restrictions: objects are based on classes that are an open universe - a client can extend a class with a new subclass that wasn't known at the time the superclass was defined. On the other hand, ADTs are a closed universe, where the definition of an ADT specifies precisely all the cases that are ever possible.

* The two most commonly used sorts of ADTs are product and sum types. A product ADT is a conjunction of component types ("AND" type) and it is supported by the majority of PLs in form of tuples, records, structs, objects, and similar compound types, whose values are constructed only when all the values of its constituent types are supplied.

* Sum types are *disjoint tagged unions*, and also known as variant types.
