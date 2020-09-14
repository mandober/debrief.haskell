# Class declaration

Type classes allow us to declare which types are instances of which class, and to provide definitions of the overloaded operations associated with a class. 

For example, to define a type class containing an equality operator: 

```hs
class Eq a where 
  (==) :: a -> a -> Bool
```
Here Eq is the name of the class being defined, and == is the single operation in the class. This declaration may be read "a type a is an instance of the class Eq if there is an (overloaded) operation ==, of the appropriate type, defined on it." (Note that == is only defined on pairs of objects of the same type.)

The constraint that a type a must be an instance of the class Eq is written Eq a. Thus Eq a is not a type expression, but rather it expresses a constraint on a type, and is called a context. Contexts are placed at the front of type expressions. For example, the effect of the above class declaration is to assign the following type to ==: 

```hs
(==) :: (Eq a) => a -> a -> Bool
```

This should be read, "For every type a that is an instance of the class Eq, == has type a->a->Bool". This is the type that would be used for == in the elem example, and indeed the constraint imposed by the context propagates to the principal type for elem: 

```hs
elem :: (Eq a) => a -> [a] -> Bool
```

This is read, "For every type a that is an instance of the class Eq, elem has type a->[a]->Bool". This is just what we want---it expresses the fact that elem is not defined on all types, just those for which we know how to compare elements for equality.

But how do we specify which types are instances of the class Eq, and the actual behavior of == on each of those types? This is done with an **instance declaration**.
