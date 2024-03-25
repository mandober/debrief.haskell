# Multi-parameter Type Classes

Multi-parameter classes are Haskell's feature that gets it very close to Prolog. Prolog is all about programming with logical relations and computation is performed by defining how things related to one another.

For example, normally the addition is considered an operation that takes two input arguments and returns the result, of adding these args, as the output.

However, Prolog takes a logical view of addition and considers only the relations defined by the 3 values involved, `a + b = c`. In fact, Prolog has no notion of input and output and treats all 3 values just the same. What is normally the name for an operation, in Prolog becomes the name for a predicate.

And the predicate `add/3` is defined in a way that Peano would approve:

```prolog
% definition of natural numbers
nat(0).                                 % 0 ∈ ℕ
nat(s(N)) :- nat(N).                    % S(n) ∈ ℕ <- n ∈ ℕ

% addition as a 3-place predicate
add(  0 , B,   B).                      %   0  + b = b
add(s(A), B, s(C)) :- add(A, B, C).     % s(a) + b = s(a + b)
```

This allows us to get much more than we've bargained for; namely, Prolog makes available a lot more operation than just addition: we can use it as additon by making a query about the value of `c` having provided the values of `a` and `b`, but we can also query the value of `a` having proivided `b` and `c`, or the value of `b` having provided `a` and `c`. But, we can also go wider: query for possible values of two arguments having provided one (which in this case might not be too exciting), or even the most general case of asking for the possible values of all 3 values having provided none.

In Prolog, the implication is `q ⟸ p` i.e. `q :- p`; in Haskell, it's `p => q`.

```hs
class Add a b c | a b -> c

-- recursion on the first param
instance Add Z b b
instance (Add a b c) => Add (S a) b (S c)

-- xor:

-- recursion on the second param
instance Add a Z a
instance (Add a b c) => Add a (S b) (S c)
```

This `Add` class where the only fundep is `a b -> c` has no preference whether the first or second param is used for recursion.

To strengthen that preference we can introduce an additional fundep. Adding the fundep `a c -> b` requires the first param `a` to be recursive. But adding the fundep `b c -> a` requires the second param, `b`, to be recursive.

```hs
class Add1 a b c | a b -> c,     a  c -> b
--                               a  b    c
instance                 Add1    Z  b    b
instance (Add1 a b c) => Add1 (S a) b (S c)

                              a <- b     a
                              a <- b     c
class Add2 a b c | a b -> c,  c    b ->  a
--                            a    b     c
instance                 Add2 a    Z     a
instance (Add2 a b c) => Add2 a (S b) (S c)
```


Add A b c | a b -> c    ,     a c -> b
Add a B c | a b -> c    ,     b c -> a

Add    a  b    c

Add    Z  b    b    a c -> b    (c = b)  a b -> b
Add (S a) b (S c)   a b -> c


Plus a b c | a b -> c, b c -> a

Plus a    b     c
Plus a (S b) (S c)   a b -> c
                     1 2 -> 3
Plus a    Z     a    b c -> a   (c = a)  a b -> a
                     b c -> a
                     2 3 -> 1
