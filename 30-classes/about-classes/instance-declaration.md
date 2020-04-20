# Instance declaration

**Instance declaration** allow us to specify the types that are members of the class and then to define the actual behavior of overloaded operations associated with that class.

```hs
instance Eq Integer where 
  x == y =  x `integerEq` y
```
The function `integerEq` happens to be the primitive function that compares integers for equality, but in general any valid expression is allowed on the right-hand side, just as for any other function definition.

The overall declaration is essentially saying: "The type Integer is an instance of the class Eq, and here is the definition of the method corresponding to the `==` operation".
```hs
instance Eq Float where
  (==) = floatEq
```
Similarly, we have add another type, Float, to the class by also defining its behaviour wrt `==`.

Let's examine Eq class:

```hs
class  Eq a  where
  (==), (/=)            :: a -> a -> Bool
  x /= y                =  not (x == y)
```

The type sigs are given for both operations (they're the same), but `/=` also has a method body defined - this is *default implementation*.

If a method for a particular operation is omitted in an instance declaration, then the default one defined in the class declaration (if any) is used.
