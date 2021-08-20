# Abstract data type

https://wiki.haskell.org/Abstract_data_type

An __abstract data type__ (ADT) is a type whose internals are hidden, exposing only a set of functions that makes a data type's public API.

The API provides the functions meant to be used to manipulate the data type. These functions are made available by the type creator as the safe means of interacting with the ADT. The type's designer makes choices about the operations his ADT wil support and then he provides the corresponding public functions. The entire set of ADT's behaviours is encapsulated in the public API. Changing any internal aspect of an ADT such that the change propagates outwards and affects the API, requires bumping (the middle or even the first number in a) the packages's version.

The author of the package is *upstream* in relation to the consumer of the package, which is *downstream*. Any change in a package should be done at the source, by prompting the author, since any change done by a consumer will have  only confined ad hoc effect.

In case of Haskell, this means the data ctors are not exported, instead only the *smart constructors* are exported, which are functions that enhance the data ctors (data ctors are also functions, albeit very simple ones) in some way; e.g. they perform verification, make assertions, etc., but, most importantly, they make sure the invariants maintained by the data type are never violated.


In Haskell, by convention, these internal unexported language entitites are gathered in a module prefixed with the name "Internal"; e.g. like `Data.Set` that exposes the public API, and `Data.Set.Internal` that hides the set's internals.

Haskell supports the definition of abstract data types via the module system. In many cases it is not necessary to completely hide the representation of data type, so a normal type definition is sufficient.

In addition, parameterized types can be viewed as a kind of abstract type, because they leave some parts of the data type undefined, or abstract.

Here's an example of a parameterized data type.

data Tree a = Nil | Node { left :: Tree a, value :: a, right :: Tree a }


This type is abstract because it leaves some aspects of its structure undefined, to be provided by the user of the data type. This is a weak form of abstract data type. In this example, the type of elements contained in the tree is left open. For example, a user of this data type might use it like this:

three number tree :: Tree Integer
three number tree = Node (Node Nil 1 Nil) 2 (Node Nil 3 Nil)

The user here fills in the type of elements of the tree (`Integer`). A different user might specify a different type for that parameter. This flexibility is what allows this type to be used in many different contexts. Different abstract data types leave different parts of the data abstract.


In contrast, a concrete data type is one which does not provide such flexibility.

The above example uses parametrization to achieve abstraction, while still exposing the structure of the data to its users.

## Stack

A more traditional abstract data type completely hides the internal structure, or representation, of data. The following example illustrates this more traditional form of abstract data type.

We can implement a simple polymorphic stack using a list without actually telling the consumer anything about its inner workings. The module only exports the type constructor (but not the data constructor) and the functions:

module Stack (Stack, empty, isEmpty, push, top, pop) where

empty :: Stack a
isEmpty :: Stack a \-> Bool
push :: a \-> Stack a \-> Stack a
top :: Stack a \-> a
pop :: Stack a \-> (a,Stack a)

newtype Stack a \= StackImpl \[a\] \-- opaque!
empty \= StackImpl \[\]
isEmpty (StackImpl s) \= null s
push x (StackImpl s) \= StackImpl (x:s)
top (StackImpl s) \= head s
pop (StackImpl (s:ss)) \= (s,StackImpl ss)


If you later decide to change the stack implementation, the API doesn't change. Also you can be sure that the user cannot modify "your" data structures inside the abstract data type.



Abstract data types are just one form of data abstraction.

Abstract data types are modeled on abstract algebras, which consist of a set of values and an collection of operations on those values.

Object-oriented data abstraction is fundamentally different: objects are implemented as collections of observations (methods) that can be performed upon them.

> The focus on observations, rather than construction, means that *objects are best understood as co-algebras*. In this sense, *objects and abstract data types are duals of each other*.

The central concept related to abstract data types is the "interface" to an abstract data type. This is the set of operations that the abstract data type provides that can be used to manipulate values of the data type.

In the above example, the interface contains the following operations: Nil (constructor), Node (constructor), left (projection), value (projection) and right (projection). The set of operations in the interface does not contain any operations for manipulating the part of the data type that was left abstract.

The "stack" example is fully abstract, but the "Tree" example is not. In particular, the implementation of the Tree operations in the interface was not made abstract. This means that the implementation cannot be changed without changes to all code that uses the type.

## Using a type class

In Haskell, it is also possible to describe an interface to a data type using a type class. Class provides a mechanism for making the implementation of operations for a data type abstract as well. For example, the above interface for the abstract Tree data type might be described as:

class Tree t where
    nil   :: t a
    node  :: t a \-> a \-> t a \-> t a
    left  :: (MonadPlus m) \=> t a \-> m (t a)
    right :: (MonadPlus m) \=> t a \-> m (t a)
    value :: (MonadPlus m) \=> t a \-> m a


Note that this description is even more abstract than the description of the Tree abstract data type above.

This interface will also make the implementation of the tree abstract (not just the element type).

This interface allows the user to change the implementation of the tree data type later easily. However, the type class does not prevent access to the underlying data representation, hence type classes provide a weaker form of abstraction than the Stack example given above.

Syntax support: the names of abstract types start with a lowercase letter, and names of concrete types start with an uppercase letter.
