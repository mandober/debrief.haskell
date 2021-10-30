# 3.2 Variables, Constructors, Operators, and Literals
3200-vars-ctors-operators-literals.md

++

++

## Operators

Operator:
- operator symbol
- backquoted ordinary identifier

Haskell provides special syntax to support infix notation. An *operator* is a function that can be applied using *infix syntax* (Section 3.4), or *partially applied* using a *section* (Section 3.5).

An operator is either an *operator symbol*, such as `+` or `$$`, or is an *ordinary identifier* enclosed in grave accents (backquotes).

If no fixity declaration is given for an infix operator then it defaults to highest precedence and left associativity (see Section 4.4.2).

Dually, an operator symbol can be converted to an ordinary identifier by enclosing it in parentheses. For example, (+) x y is equivalent to x + y, and foldr (⋆) 1 xs is equivalent to foldr (\\x y -> x⋆y) 1 xs.

## Constructors

Special syntax is used to name some constructors for some of the built-in types, as found in the production for gcon and literal. These are described in Section 6.1.

## Literals

An integer literal represents the application of the function fromInteger to the appropriate value of type Integer. Similarly, a floating point literal stands for an application of fromRational to a value of type Rational (that is, Ratio Integer).

Translation: The integer literal i is equivalent to fromInteger i, where fromInteger is a method in class Num (see Section 6.4.1).

The floating point literal f is equivalent to fromRational (n Ratio.% d), where fromRational is a method in class Fractional and Ratio.% constructs a rational from two integers, as defined in the Ratio library. The integers n and d are chosen so that `n\d = f`.
