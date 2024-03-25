# Parametric Polymorphism

https://en.wikipedia.org/wiki/Parametric_polymorphism

* **Parametric polymorphism** allows functions to work with all types, hence the synonym *unbounded polymorphism*.

* Polymorphic types can become monomorphic by consistent substitution of their variables.

* Regular functions, *from value to value*, take a value and return a value. Parametric polymorphism allows for *functions from type to value* since their first, although implicit, arg is a type. HKT enables functions *from type to type*, and these type-level functions are called *type operators* (type families, type ctors).
