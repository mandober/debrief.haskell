
The Ord class is used for totally ordered datatypes.

Instances of Ord can be derived for any user-defined datatype whose constituent types are in Ord . The declared order of the constructors in the data declaration determines the ordering in derived Ord instances. The Ordering datatype allows a single comparison to determine the precise ordering of two objects.

Ord , as defined by the Haskell report, implements a total order and has the following properties:

Comparability: x <= y || y <= x = True
Transitivity: if x <= y && y <= z = True , then x <= z = True
Reflexivity: x <= x = True
Antisymmetry: if x <= y && y <= x = True , then x == y = True
The following operator interactions are expected to hold:

x >= y = y <= x

x < y = x <= y && x /= y

x > y = y < x

x < y = compare x y == LT

x > y = compare x y == GT

x == y = compare x y == EQ

min x y == if x <= y then x else y = True

max x y == if x >= y then x else y = True

Note that (7.) and (8.) do not require min and max to return either of their arguments. The result is merely required to equal one of the arguments in terms of (==) .

Minimal complete definition: either compare or <= . Using compare can be more efficient for complex types.

Documentation
