# Default declaration aka Type defaulting

------------------------------------------------------------------------------
https://kseo.github.io/posts/2017-01-04-type-defaulting-in-haskell.html
https://www.alexeyshmalko.com/2014/haskell-defaulting-rules/

Haskell Report 2010: 
4.3.4 Ambiguous Types, and Defaults for Overloaded Numeric Operations

------------------------------------------------------------------------------
function overloading
ambiguous type
expression type-signature
fixed type
type annotation
`asTypeOf` function


------------------------------------------------------------------------------

A problem inherent to Haskell-style overloading is the possibile occurances of an *ambiguous type*. Ambiguous types may be resolved by type annotations, e.g. through *expression type-signature*.

Occasionally, an otherwise type-ambiguous expression needs to be made the same type as some variable, rather than being given a *fixed type* using an expression type-signature. This is the purpose of the function `asTypeOf`.

`asTypeOf` is a type-restricted version of `const`. It is usually used as an infix operator, and its typing forces its first argument (which is usually overloaded) to have the same type as the second.

`asTypeOf x y` valuates to x, but x is forced to have y's type.


Ambiguities in the `Num` class are most common, so Haskell provides another way to resolve them: with a *default declaration*. Only one default declaration is permitted per module, and its effect is limited to that module. The *type defaulting rule* is very conservative. Defaults are limited to Prelude numeric classes and cannot be applied to user-defined classes.

Haskell default rule can be summarized as:
```hs
default Num Integer
default Real Integer
default Enum Integer
default Integral Integer
default Fractional Double
default RealFrac Double
default Floating Double
default RealFloat Double
```

When we want to change the default type for the type class, Haskell lets us specify a list of types in a special top-level *default declaration*:
`default (Int, Float)`


```hs
-- clearing ambiguous types with expression type-signature
let x = read "..." in show (x::Bool)

-- clearing ambiguous types with asTypeOf
approxSqrt x = encodeFloat 1 (exponent x `div` 2) `asTypeOf` x

-- default declaration
-- consider the following classic example:
show :: Show a => a -> String
read :: Read a => String -> a

f :: String -> String
f s = show (read s)

-- GHC complains about ambiguous type variable. the problem is the intermediate
-- subexpression "(read s)". The "read" fn can parse an Int, Char, etc but the compiler cannot arbitrarily choose a type. To clear this ambiguity:
f s = show (read s :: Int)

-- However, this becomes cumbersome when handling numeric types. For example:
negate :: Num a => a -> a
show :: Show a => a -> String
(show (negate 4))
{- The expression "(show (negate 4)) is ambiguous because the literal 4 is of "Num a => a" type in Haskell. 4 can be an Int, Float, etc., so the compiler cannot choose one. Compromise is the ad-hoc rule for choosing a particular default type: The default type of Num is Integer, so the compiler infers the type of "(negate 4)" as Integer instead of rejecting it as an invalid program.
-}

-- If no default declaration is given in a module then it's assumed to be:
default (Integer, Double)

-- empty default declaration turns off all defaults in a module:
default ()
```


---

[default declaration] [type defaulting] [ambiguous type] [asTypeOf fn]
