# Constructor

https://wiki.haskell.org/Constructor

Constructors:
- Type constructor
- Value (data) constructor

* *Data declaration* is the statement that defines a datatype. A data declaration introduces one or more *type constructors*; and one or more *value constructors* for each type constructor. Type ctors and value ctors are in separate namespace, so they can have the same name in one data declaration (which is often the case).

* *Type ctor* is the type name, although a type ctor can contain other items as well, most often, type variables and constraints.

* A type ctor may have zero (nullary), one (unary), two (binary), or more (poliadic) args.

* *Data ctors* are the values that inhabit the type they are defined in. They are the values living at the term level (not type level). In fact, a value ctor is a function that takes (also partially) args and construct a value of the type it belong to.

* Data ctors group values and tag alternatives in an algebraic data type. e.g. the `Tree a` type can have two data ctors, `Tip` and `Node`. Any value that belongs to the type `Tree a` is constructed by either `Tip` or `Node`. In fact, `Tip` is a nullary ctor, a constant, (groups no values) so there is only one value that Tip constructs and that value may be called `Tip` as well.

* *Type variable* is a stand-in for a concrete type; it will be substituted by a specific type on use; e.g. the type `Tree a` when used will be *instantiated* as `Tree Int` or `Tree (Tree Boolean)`, etc.

* Data ctors are *first class values* in Haskell and have a type; the type of Left data ctor `Left :: a -> Either a b`. As first class values, they may be passed to functions, held in a list, be data elements of other algebraic data types and so forth. Data ctors are not types.

* Data ctors group values, and a data ctor also holds a sequance of values together. They are also much like tags, in that they tag a value or sequance of values for easy recognition.

* Pattern matching is able to differentiate and destruct vrious values by matching the values on their data ctors.

* Both type and data ctors may be infix operators with symbolic names.

* Type ctors are used in functions' signatures; data ctors are used in pattern match to identify a (semi-unknown) value.

* A type of a *type ctor* is called a **kind**.
