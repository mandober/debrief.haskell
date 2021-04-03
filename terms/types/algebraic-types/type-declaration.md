# Type declaration

* `data` is tha main keyword used for declaring new ADTs.
* `newtype`is a performance orientated version applicable only for type with a single data ctor. It is a zero-cost wrapper that's discarded on compilation.
* `type` keyword is a version used only as a convenience type aliasing tool.

The *type declaration* must appear at the top level and the BOL, unindented. (a type declaration is sometimes called a statement, declaration expression, expression statement, or just type declaration; in any case, these names all descibe the particular syntax or a language construct tha tis used to introduce a custom type, not the semantics of what it evaluates to and whatnot).

The type declaration has two sides (save for a special case of empty types): the left-hand side marks a type declaration by beginning with the `data` keyword, which is then followed by actual declarations of new names (labels) via their identifiers: the type name (beginning in capital letter) comes first. This is an arbitrary name chosen by the programer which will almost always be used in discussions to refer to this type. It may differ from the name that will be used to refer to this type in different contexts in the code. However, if a type has a symbolic name then it may have several (nick)names that are used in discussions. [can user types have symbolic names at all?]

The poor (and far from pure) point is that the exact name, i.e. precise syntax to refer to a type most definitely starts with its name given in declaration, and in some cases that will be sufficient, but in some context one or more of its type variables will follow as well, contributing to a type's name.

With modules in picture, the type names (but of other language items as well) becomes more or less complicated depending on exact context and the way they were imported to the referring module.

In a type declaration, the name of a type is also a constructor for that particular type. A *type constructor* is similar to a term-level function, only it is a type-level function that takes type arguments as input and produce a type as output. And just like their term-level counterparts, they also support currying and partial application (which also contribute to the confusion regarding which exact form of a type's name to use in different occasions).

In a type declaration, an occurance of a name on the LHS is its *declaration occurance*, while the appearance of that name on the RHS is its *applied occurance*. This is relevant to type variables only since other types cannot occur on the LHS (they have already been declared).

For example, a type declaration for an integer list does not have the `Int` appearing in the declaration context, but a list generalized to all types would have a declaration occurance of a type parameter, and consequantly, the applied occurances of that type param on the RHS.

```hs
-- specialized
data List = Nil | Cons Int List

-- generalized
data List a = Nil | Cons a (List a)
```

Notice all the things that had to change going from the specialized to the generalized version:
- declaring a type parameter
- replacing `Int` with a type parameter
- changing the reference to the type's name
- which consequently brough on the parenthesis







---

A type declaration is introduced, at the top level only, with the `data` keyword which is followed by the custom type name and the sequence of (none, one or more) type parameters. The type parameters are named using a single letter but is only a convention; they must begin with a lowercase, though. The occurance of a type param here, in the LHS 

is its declaration, so this, RHS of the type declaration statement can be considered a declarations part, as opposed to the LHS which may be considered the use part.

sequence of none, one or more types, which can be a mix of concrete types and type variables.

```hs
data Bool = ...
data Maybe a = ...
data Axis = ...
```
