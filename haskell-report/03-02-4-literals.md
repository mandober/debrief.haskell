# 3.2.4 Literals

An integer literal represents the application of the function fromInteger to the appropriate value of type Integer. Similarly, a floating point literal stands for an application of fromRational to a value of type Rational (that is, Ratio Integer).

Translation:

* The integer literal `i` is equivalent to `fromInteger i`, where `fromInteger` is a method in class `Num` (see Section 6.4.1).

* The floating point literal `f` is equivalent to `fromRational (n Ratio.% d)`, where `fromRational` is a method in class `Fractional` and `Ratio.%` constructs a rational from two integers, as defined in the `Ratio` library. The integers `n` and `d` are chosen so that `n \ d = f`.
