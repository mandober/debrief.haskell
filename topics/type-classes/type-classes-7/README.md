# Type Class :: README

* type class
  - type class declaration
  - type class instance

* type class sorts
  * by subject (type ctor)
    - elemental type classes (on `Type`s)
    - constructor type classes (on unsaturated type ctors)
  * by subordinance
    - nominal class
    - subclass
    - superclass
  * by features
    - class context specifies constraints
      - specifies superclasses, `(RealFrac a, Floating a) => RealFloat a`
      - specifies type equality, `(a ~ Int) => …`
      - specifies implicit types, `(?a) => …`
    - multi-parameter type classes, `class T1 T2 where`
    - type classes with fundeps, `class Add m n s | m n -> s, m s -> n`
    - type classes with assiciated type family
      class IsList l where
        type Item :: l a
    - type classes with assiciated data family
  * by scope
    - blank class
    - overlapping, overlaps, overlappable


* instances
  - instance declaration
    - user-defined instances
    - automatically derived instances
    - blank instances
  * sorts of instances
    * by issue
      - overlapping instances
      - orphaned instances
      - flexible instances
      - incoherent instances


* type class properties
  grouping data types (type ctors)
  provide restricted form of polymorphism
  - first ∀ then explixt list of ∃
  - 
  open construction: can always add new members
